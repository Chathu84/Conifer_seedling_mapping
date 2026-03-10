out_dir <- "/home/jovyan/data-store/classification_data"

pretty_var   <- function(x) stringr::str_to_sentence(gsub("_+", " ", x))
pretty_class <- function(x) stringr::str_to_sentence(gsub("_+", " ", x))

orig_levels <- levels(training$new_cover)
safe_levels <- make.names(orig_levels)
lvl_map     <- setNames(safe_levels, orig_levels)
inv_map     <- setNames(orig_levels, safe_levels)


rf_fit <- model_rf_top12

preds <- names(training)[-c(1:2, 198:203)]
## =========================
## Predictions & files
## =========================
# All rows (prob + hard class)
prob_all <- predict(rf_fit, newdata = training[,-c(1:2,198:203)], type="prob")
colnames(prob_all) <- pretty_class(inv_map[colnames(prob_all)])

pred_all <- predict(rf_fit, newdata = training[,-c(1:2,198:203)], type="raw")
pred_all_pretty <- pretty_class(inv_map[as.character(pred_all)])

pred_tbl <- training %>%
  transmute(row_id = row_number(),
            true = pretty_class(lvl_map[as.character(new_cover)]),
            pred = pred_all_pretty) %>%
  bind_cols(as_tibble(prob_all))

write_csv(pred_tbl, file.path("/home/jovyan/data-store/classification_data/rf_predictions_all_rows.csv"))

# Test set files + confusion matrix
prob_test <- predict(rf_fit, newdata = temp_data[,-c(1:2,198:203)], type="prob")
colnames(prob_test) <- pretty_class(inv_map[colnames(prob_test)])
pred_test <- predict(rf_fit, newdata = temp_data[,-c(1:2,198:203)], type="raw")

cm <- confusionMatrix(
  data      = factor(pretty_class(inv_map[as.character(pred_test)]),
                     levels = pretty_class(inv_map[safe_levels])),
  reference = factor(pretty_class(inv_map[as.character(temp_data$new_cover)]),
                     levels = pretty_class(inv_map[safe_levels]))
)
capture.output(cm, file = file.path("/home/jovyan/data-store/classification_data/confusion_matrix_test.txt"))

prob_test_out <- prob_test %>%
  bind_cols(tibble(
    true = pretty_class(inv_map[as.character(temp_data$new_cover)]),
    pred = pretty_class(inv_map[as.character(pred_test)])
  ))
write_csv(prob_test_out, file.path("/home/jovyan/data-store/classification_data/rf_test_probabilities.csv"))

## =========================
## Variable importance (Permutation via caret)
## =========================
imp <- caret::varImp(rf_fit)$importance
imp$Variable <- rownames(imp)
if (!"Overall" %in% names(imp)) {
  imp$Overall <- rowMeans(imp[setdiff(names(imp), "Variable")], na.rm = TRUE)
}

imp_top <- imp %>% arrange(desc(Overall)) %>% slice_head(n=30) %>%
  mutate(VarPlot = pretty_var(Variable))
ggplot(imp_top, aes(x="Overall", y=reorder(VarPlot, Overall), fill=Overall)) +
  geom_tile(color="white") +
  scale_fill_viridis_c() +
  labs(title="Variable Importance (Permutation)", x=NULL, y=NULL) +
  theme_classic()
ggsave(file.path(out_dir, "importance_heatmap.png"), width=6, height=8, dpi=300)

## =========================
## Per-class “best variables” (association with class prob)
## =========================
preds_cv <- rf_fit$pred
best <- rf_fit$bestTune
preds_cv <- preds_cv %>% filter(mtry == best$mtry)

train_rows <- training[preds_cv$rowIndex, -c(1:2,198:203), drop=FALSE]
colnames(train_rows) <- make.unique(colnames(train_rows))
preds_aug <- bind_cols(preds_cv, train_rows)


# Use the model’s class levels (these match prob column names)
class_cols <- rf_fit$levels    # "Mature.trees" etc.

# Pretty-name mapping for plots/tables
pretty_map <- c(
  Dead = "Dead",
  Deciduous = "Deciduous",
  Mature.trees = "Mature trees",
  Seedlings = "Seedlings",
  Shrub = "Shrub"
)

prob_long <- preds_aug %>%
  tidyr::pivot_longer(dplyr::all_of(class_cols),
                      names_to = "ClassSafe",
                      values_to = "Probability") %>%
  dplyr::mutate(
    ClassPretty = unname(pretty_map[ClassSafe]),
    Probability = pmin(pmax(Probability, 1e-6), 1 - 1e-6),
    logit_prob  = qlogis(Probability)
  )
# class_cols <- levels(training$new_cover)
# 
# prob_long <- preds_aug %>%
#   pivot_longer(all_of(class_cols), names_to="ClassSafe", values_to="Probability") %>%
#   mutate(ClassPretty = pretty_class(inv_map[ClassSafe]),
#          Probability = pmin(pmax(Probability, 1e-6), 1-1e-6),
#          logit_prob  = qlogis(Probability))

vars_long <- preds_aug %>%
  pivot_longer(all_of(preds), names_to="Variable", values_to="Value")

prob_var <- inner_join(prob_long %>% select(rowIndex, ClassPretty, logit_prob),
                       vars_long %>% select(rowIndex, Variable, Value),
                       by="rowIndex")

imp_by_class <- prob_var %>%
  group_by(ClassPretty, Variable) %>%
  summarise(Importance = abs(cor(logit_prob, Value, method="spearman", use="complete.obs")),
            n = sum(is.finite(logit_prob) & is.finite(Value)), .groups="drop") %>%
  filter(n > 10) %>%
  mutate(VarPlot = pretty_var(Variable))

top5 <- imp_by_class %>%
  group_by(ClassPretty) %>%
  slice_max(order_by=Importance, n=5, with_ties=FALSE) %>%
  ungroup()

# If you have tidytext installed, the reordered scale is nice; otherwise drop reorder_within
if (requireNamespace("tidytext", quietly = TRUE)) {
  library(tidytext)
  p_top <- ggplot(top5, aes(x = Importance, y = reorder_within(VarPlot, Importance, ClassPretty), fill = ClassPretty)) +
    geom_col() + tidytext::scale_y_reordered()
} else {
  p_top <- ggplot(top5, aes(x = Importance, y = reorder(VarPlot, Importance), fill = ClassPretty)) + geom_col()
}
p_top + labs(title="Top variables per class (|Spearman| vs class logit-probability)",
             x="Importance", y=NULL) + theme_classic() + theme(legend.title=element_blank())
ggsave(file.path(out_dir, "top_variables_per_class.png"), width=8, height=6, dpi=300)

## =========================
## Single-variable AUC heatmap (one-vs-rest)
## =========================
auc_tbl <- map_dfr(unique(prob_long$ClassPretty), function(cls) {
  y <- as.integer(prob_long$ClassPretty == cls)[match(prob_var$rowIndex, prob_long$rowIndex)]
  map_dfr(preds, function(v) {
    x <- vars_long$Value[vars_long$Variable == v]
    x <- x[match(prob_var$rowIndex, vars_long$rowIndex)]
    r <- try(pROC::roc(response=y, predictor=x, quiet=TRUE), silent=TRUE)
    auc <- if (inherits(r, "try-error")) NA_real_ else as.numeric(pROC::auc(r))
    tibble(ClassPretty=cls, Variable=v, VarPlot=pretty_var(v), AUC=auc)
  })
})

auc_top <- auc_tbl %>%
  group_by(ClassPretty) %>%
  slice_max(order_by=AUC, n=12, with_ties=FALSE) %>%
  ungroup()

ggplot(auc_top, aes(x=AUC, y=reorder_within(VarPlot, AUC, ClassPretty), fill=AUC)) +
  geom_tile(height=0.85) +
  facet_wrap(~ ClassPretty, scales="free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_viridis_c(limits=c(0.5,1), oob=scales::squish, name="AUC") +
  labs(title="Single-variable class separation (one-vs-rest AUC)",
       x="AUC (0.5 = no separation, 1 = perfect)", y=NULL) +
  theme_classic()
ggsave(file.path(out_dir, "single_variable_auc_heatmap.png"), width=10, height=7, dpi=300)

## =========================
## PDP + ICE (from randomForest model)
## =========================
rf_final     <- rf_fit$finalModel            # randomForest object
train_frame  <- rf_fit$trainingData          # caret's training frame (.outcome + predictors)
topK <- 6
vars_pdp <- imp %>% arrange(desc(Overall)) %>% slice_head(n=topK) %>% pull(Variable)
vars_pdp <- intersect(vars_pdp, setdiff(names(train_frame), ".outcome"))

dir.create(file.path(out_dir, "pdp_ice"), showWarnings = FALSE)
plot_ice_pdp <- function(var, cls_safe, out_png) {
  pd <- pdp::partial(
    object = rf_final,
    pred.var = var,
    which.class = cls_safe,  # safe level (randomForest supports this)
    prob  = TRUE,
    ice   = TRUE,
    center= TRUE,
    plot  = FALSE,
    grid.resolution = 50,
    train = train_frame
  )
  pd2 <- pd; names(pd2)[1] <- "x"
  qs <- pd2 %>% group_by(x) %>% summarise(
    lower = quantile(yhat, 0.025, na.rm=TRUE),
    upper = quantile(yhat, 0.975, na.rm=TRUE), .groups="drop"
  )
  p <- ggplot(pd2, aes(x=x, y=yhat)) +
    geom_ribbon(data=qs, aes(ymin=lower, ymax=upper), alpha=0.25, inherit.aes=FALSE) +
    geom_line(aes(group=yhat.id), alpha=0.20) +
    stat_summary(fun=mean, geom="line", linewidth=1.1) +
    labs(title=paste0("ICE + PDP — Class: ", pretty_class(inv_map[cls_safe]), " — ", pretty_var(var)),
         x=pretty_var(var), y="Predicted probability") +
    theme_classic()
  ggsave(out_png, p, width=7, height=4.5, dpi=300)
}
for (v in vars_pdp) {
  for (cls_safe in safe_levels) {
    out_png <- file.path(out_dir, "pdp_ice",
                         paste0("ICE_PDP_", pretty_class(inv_map[cls_safe]), "_", gsub("[^A-Za-z0-9]+","_", v), ".png"))
    plot_ice_pdp(v, cls_safe, out_png)
  }
}

## =========================
## Tuning curve snapshot
## =========================
ggsave(
  file.path(out_dir, "rf_tuning_results.png"),
  plot = ggplot(rf_fit$results, aes(x=mtry, y=Accuracy)) + geom_point() + geom_line() + theme_classic(),
  width=6, height=4, dpi=300
)

message("All done. Outputs at: ", out_dir)
