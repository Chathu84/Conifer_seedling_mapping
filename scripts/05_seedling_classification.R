8

install.packages(c("ranger", "MLmetrics", "NeuralNetTools", "LiblineaR", "snow"))

# Packages for spatial data processing & visualization
library(rgdal)
library(tidyverse)
# library(gdalUtils)
library(raster)
library(sf)
library(sp)
# library(RStoolbox)
# library(getSpatialData)
# library(rasterVis)
library(mapview)

library(RColorBrewer)
#library(plotly)
library(grDevices)

# Machine learning packages
library(caret)
library(randomForest)
library(ranger)
library(MLmetrics)
library(nnet)
library(NeuralNetTools)
library(LiblineaR)
#library(Rmpi)

# Packages for general data processing and parallel computation
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)
library(reshape2)



wd = "/home/jovyan/data-store/revision_analysis/"
setwd(wd)

out_dir <- "/home/jovyan/data-store/revision_analysis/figures"

input_files =  dir(file.path("/home/jovyan/data-store/revision_analysis/var_polygons"), pattern = "*.gpkg", full.names = FALSE, ignore.case = TRUE)

poly_data = data.frame()

for(i in 1:length(input_files)){
  print(input_files[i])
  ployfile = as.data.frame(st_read(paste0("/home/jovyan/data-store/revision_analysis/var_polygons/",input_files[i])))
  poly_data = rbind(poly_data,ployfile)
}


training = poly_data[, c(1,2,11,19,20,22,24:218)]

training[training == -Inf] <- NA

# old_names <- c(
#   "mean_R", "variance_R", "min_R", "max_R", "median_R",
#   "mean_G", "variance_G", "min_G", "max_G", "median_G",
#   "mean_B", "variance_B", "min_B", "max_B", "median_B",
#   "mean_RI", "variance_0", "min_RI", "max_RI", "median_RI",
#   "mean_GI", "variance_1", "min_GI", "max_GI", "median_GI",
#   "mean_BI", "variance_2", "min_BI", "max_BI", "median_BI",
#   "mean_EXR", "variance_3", "min_EXR", "max_EXR", "median_EXR",
#   "mean_VARI", "variance_4", "min_VARI", "max_VARI", "median_VA0",
#   "mean_GRVI", "variance_5", "min_GRVI", "max_GRVI", "median_GR0",
#   "mean_MGRVI", "variance_6", "min_MGRVI", "max_MGRVI", "median_MG0",
#   "mean_CIVE", "variance_7", "min_CIVE", "max_CIVE", "median_CI0",
#   "mean_EXG", "variance_8", "min_EXG", "max_EXG", "median_EXG",
#   "mean_GLA", "variance_9", "min_GLA", "max_GLA", "median_GLA",
#   "mean_R_me0", "variance10", "min_R_mean", "max_R_mean", "median_R_0",
#   "mean_R_va0", "variance11", "min_R_var0", "max_R_var0", "median_R_1",
#   "mean_R_ho0", "variance12", "min_R_hom0", "max_R_hom0", "median_R_2",
#   "mean_R_co0", "variance13", "min_R_con0", "max_R_con0", "median_R_3",
#   "mean_R_di0", "variance14", "min_R_dis0", "max_R_dis0", "median_R_4",
#   "mean_R_en0", "variance15", "min_R_ent0", "max_R_ent0", "median_R_5",
#   "mean_R_se0", "variance16", "min_R_sec0", "max_R_sec0", "median_R_6",
#   "mean_R_co1", "variance17", "min_R_cor0", "max_R_cor0", "median_R_7",
#   "mean_G_me0", "variance18", "min_G_mean", "max_G_mean", "median_G_0",
#   "mean_G_va0", "variance19", "min_G_var0", "max_G_var0", "median_G_1",
#   "mean_G_ho0", "variance20", "min_G_hom0", "max_G_hom0", "median_G_2",
#   "mean_G_co0", "variance21", "min_G_con0", "max_G_con0", "median_G_3",
#   "mean_G_di0", "variance22", "min_G_dis0", "max_G_dis0", "median_G_4",
#   "mean_G_en0", "variance23", "min_G_ent0", "max_G_ent0", "median_G_5",
#   "mean_G_se0", "variance24", "min_G_sec0", "max_G_sec0", "median_G_6",
#   "mean_G_co1", "variance25", "min_G_cor0", "max_G_cor0", "median_G_7",
#   "mean_B_me0", "variance26", "min_B_mean", "max_B_mean", "median_B_0",
#   "mean_B_va0", "variance27", "min_B_var0", "max_B_var0", "median_B_1",
#   "mean_B_ho0", "variance28", "min_B_hom0", "max_B_hom0", "median_B_2",
#   "mean_B_co0", "variance29", "min_B_con0", "max_B_con0", "median_B_3",
#   "mean_B_di0", "variance30", "min_B_dis0", "max_B_dis0", "median_B_4",
#   "mean_B_en0", "variance31", "min_B_ent0", "max_B_ent0", "median_B_5",
#   "mean_B_se0", "variance32", "min_B_sec0", "max_B_sec0", "median_B_6",
#   "mean_B_co1", "variance33", "min_B_cor0", "max_B_cor0", "median_B_7",
#   "mean_chm", "variance34", "min_chm", "max.chm", "median_chm"
# )
# 
# new_names <- c(
#   "mean_R", "variance_R", "min_R", "max_R", "median_R",
#   "mean_G", "variance_G", "min_G", "max_G", "median_G",
#   "mean_B", "variance_B", "min_B", "max_B", "median_B",
#   "mean_RI", "variance_RI", "min_RI", "max_RI", "median_RI",
#   "mean_GI", "variance_GI", "min_GI", "max_GI", "median_GI",
#   "mean_BI", "variance_BI", "min_BI", "max_BI", "median_BI",
#   "mean_EXR", "variance_EXR", "min_EXR", "max_EXR", "median_EXR",
#   "mean_VARI", "variance_VARI", "min_VARI", "max_VARI", "median_VARI",
#   "mean_GRVI", "variance_GRVI", "min_GRVI", "max_GRVI", "median_GRVI",
#   "mean_MGRVI", "variance_MGRVI", "min_MGRVI", "max_MGRVI", "median_MGRVI",
#   "mean_CIVE", "variance_CIVE", "min_CIVE", "max_CIVE", "median_CIVE",
#   "mean_EXG", "variance_EXG", "min_EXG", "max_EXG", "median_EXG",
#   "mean_GLA", "variance_GLA", "min_GLA", "max_GLA", "median_GLA",
#   "mean_R_mean", "variance_R_mean", "min_R_mean", "max_R_mean", "median_R_mean",
#   "mean_R_variance", "variance_R_variance", "min_R_variance", "max_R_variance", "median_R_variance",
#   "mean_R_homogeneity", "variance_R_homogeneity", "min_R_homogeneity", "max_R_homogeneity", "median_R_homogeneity",
#   "mean_R_contrast", "variance_R_contrast", "min_R_contrast", "max_R_contrast", "median_R_contrast",
#   "mean_R_dissimilarity", "variance_R_dissimilarity", "min_R_dissimilarity", "max_R_dissimilarity", "median_R_dissimilarity",
#   "mean_R_entropy", "variance_R_entropy", "min_R_entropy", "max_R_entropy", "median_R_entropy",
#   "mean_R_second_moment", "variance_R_second_moment", "min_R_second_moment", "max_R_second_moment", "median_R_second_moment",
#   "mean_R_correlation", "variance_R_correlation", "min_R_correlation", "max_R_correlation", "median_R_correlation",
#   "mean_G_mean", "variance_G_mean", "min_G_mean", "max_G_mean", "median_G_mean",
#   "mean_G_variance", "variance_G_variance", "min_G_variance", "max_G_variance", "median_G_variance",
#   "mean_G_homogeneity", "variance_G_homogeneity", "min_G_homogeneity", "max_G_homogeneity", "median_G_homogeneity",
#   "mean_G_contrast", "variance_G_contrast", "min_G_contrast", "max_G_contrast", "median_G_contrast",
#   "mean_G_dissimilarity", "variance_G_dissimilarity", "min_G_dissimilarity", "max_G_dissimilarity", "median_G_dissimilarity",
#   "mean_G_entropy", "variance_G_entropy", "min_G_entropy", "max_G_entropy", "median_G_entropy",
#   "mean_G_second_moment", "variance_G_second_moment", "min_G_second_moment", "max_G_second_moment", "median_G_second_moment",
#   "mean_G_correlation", "variance_G_correlation", "min_G_correlation", "max_G_correlation", "median_G_correlation",
#   "mean_B_mean", "variance_B_mean", "min_B_mean", "max_B_mean", "median_B_mean",
#   "mean_B_variance", "variance_B_variance", "min_B_variance", "max_B_variance", "median_B_variance",
#   "mean_B_homogeneity", "variance_B_homogeneity", "min_B_homogeneity", "max_B_homogeneity", "median_B_homogeneity",
#   "mean_B_contrast", "variance_B_contrast", "min_B_contrast", "max_B_contrast", "median_B_contrast",
#   "mean_B_dissimilarity", "variance_B_dissimilarity", "min_B_dissimilarity", "max_B_dissimilarity", "median_B_dissimilarity",
#   "mean_B_entropy", "variance_B_entropy", "min_B_entropy", "max_B_entropy", "median_B_entropy",
#   "mean_B_second_moment", "variance_B_second_moment", "min_B_second_moment", "max_B_second_moment", "median_B_second_moment",
#   "mean_B_correlation", "variance_B_correlation", "min_B_correlation", "max_B_correlation", "median_B_correlation",
#   "mean_chm", "variance_chm", "min_chm", "max.chm", "median_chm"
# )
# 


# title_case <- new_names |>
#   gsub("_+", " ", x = _) |>
#   trimws() |>
#   tools::toTitleCase()
# 
# cat(
#   "new_names <- c(\n  \"",
#   paste(title_case, collapse = "\", \""),
#   "\"\n)\n",
#   sep = ""
# )


# Assuming old_names and new_names are full-length character vectors of equal length:
# name_map <- setNames(new_names, old_names)
# 
# # Then apply:
# training <- training %>%
#   rename_with(~ name_map[.x], .cols = names(name_map))
# 


library(dplyr)

# training <- training %>%
#   mutate(across(everything(), ~ gsub("Standing.Dead", "Dead", .))) %>%
#   mutate(across(everything(), ~ gsub("Woody.Shrub", "Shrub", .)))
# 

training <- training %>%
  mutate(across(where(is.factor), ~ as.character(.))) %>%
  mutate(across(where(is.character), ~ gsub("Standing.Dead", "Dead", .))) %>%
  mutate(across(where(is.character), ~ gsub("Woody.Shrub", "Shrub", .))) %>%
  mutate(across(where(is.character), as.factor))  # convert back to factor if needed


# training <- training %>%
#   mutate(across(everything(), ~ ifelse(. == 0 | . == -Inf, NA, .)))


training = na.omit(training)


library(dplyr)

training <- training %>%
  mutate(
    # ensure it's numeric (in case it came in as character)
    max.chm = as.numeric(max.chm),
    
    new_cover = case_when(
      Cover_Type == "Evergreen" & !is.na(max.chm) & max.chm <= 4.3 ~ "Seedlings",
      Cover_Type == "Evergreen" & !is.na(max.chm) & max.chm >  4.3 ~ "Mature trees",
      TRUE ~ Cover_Type
    )
  )

training$new_cover <- factor(training$new_cover)
# 
# training <- training %>%
#   mutate(new_cover = case_when(
#     Cover_Type == "Evergreen" & "`Max Chm`" <= 4.3 ~ "Seedlings",
#     Cover_Type == "Evergreen" & "`Max Chm`" > 4.3  ~ "Prefire_conifer",
#     TRUE ~ Cover_Type  # keep original for other cover types
#   ))

library(tibble)

# Initialize an empty list to collect summary tables
summary_list <- list()

for(i in seq_along(input_files)) {
  file_name <- input_files[i]
  print(file_name)
  
  # Read the shapefile
  polyfile <- st_read(paste0("/home/jovyan/data-store/revision_analysis/var_polygons/", file_name), quiet = TRUE)
  polyfile <- as.data.frame(polyfile)
  
  # Assign new_cover based on logic
  polyfile <- polyfile %>%
    mutate(
      # ensure it's numeric (in case it came in as character)
      max.chm = as.numeric(max.chm),
      
      new_cover = case_when(
        Cover_Type == "Evergreen" & !is.na(max.chm) & max.chm <= 4.3 ~ "Seedlings",
        Cover_Type == "Evergreen" & !is.na(max.chm) & max.chm >  4.3 ~ "Prefire_conifer",
        TRUE ~ Cover_Type
      ))
  
  # Summarize counts
  summary_df <- polyfile %>%
    count(new_cover, name = "n_samples") %>%
    mutate(file = file_name)
  
  # Append to list
  summary_list[[i]] <- summary_df
}

# Combine all summaries into one data frame
final_summary <- bind_rows(summary_list)

# Optional: reorder columns
final_summary <- final_summary %>%
  dplyr::select(file, new_cover, n_samples)

# View
print(final_summary)

# write_csv(final_summary, "/home/jovyan/data-store/seedling_Detection/plot_summary.csv")
# training <- training[training$max.chm <= 4, ]

# # Histograms of predictors
# training %>% 
#   select("Cover_Type") %>% 
#   melt(measure.vars = names(.)) %>% 
#   ggplot() +
#   geom_histogram(aes(value)) +
#   geom_vline(xintercept = 0, color = "gray70") +
#   facet_wrap(facets = vars(variable), ncol = 3)

#Split into train and test
set.seed(321)






index <- createDataPartition(training$new_cover, p = 0.5, list = FALSE)  
train_data <- training[index, ]  # 70% training data
temp_data <- training[-index, ]  # Remaining 30% for testing + validation

# Further split temp_data into test (15%) and validation (15%)
index2 <- createDataPartition(temp_data$new_cover, p = 1, list = FALSE)
test_data <- temp_data[index2, ]  
validation_data <- temp_data[-index2, ]


# Fix the factor levels of Cover_Type in both training and test data
train_data$new_cover <- factor(make.names(train_data$new_cover))
test_data$new_cover <- factor(make.names(test_data$new_cover))


# write_csv(train_data, "/home/jovyan/data-store/seedling_Detection/train_data.csv")
# write_csv(test_data, "/home/jovyan/data-store/seedling_Detection/test_data.csv")

# # Convert all factor columns that look numeric to numeric
# convert_to_numeric <- function(df) {
#   df[] <- lapply(df, function(x) {
#     if (is.factor(x)) {
#       x <- as.character(x)
#       if (all(grepl("^[0-9.]+$", x))) as.numeric(x) else x
#     } else {
#       x
#     }
#   })
#   as.data.frame(df)
# }
# 
# # Apply to both training and test datasets
# train_data <- convert_to_numeric(train_data)
# test_data <- convert_to_numeric(test_data)

# # A stratified random split of the data
# idx_train <- createDataPartition(training$Type,
#                                  p = 0.7, # percentage of data as training
#                                  list = FALSE)
# dt_train <- training[idx_train,]
# dt_test <- training[-idx_train,]
# 
# table(dt_train$Type)

#Fit models

n_folds <- 10

folds <- createFolds(1:nrow(train_data), k = n_folds)


# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model


for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

#random forest

# Register a doParallel cluster, using 3/4 (75%) of total CPU-s



# cl <- makeCluster(3/4 * detectCores())
# 
# registerDoParallel(cl)


model_rf <- caret::train(new_cover ~ . , method = "rf", data = train_data[,-c(1:6)],
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)








importance_df <- varImp(model_rf)$importance
importance_df$Variable <- rownames(importance_df)

# If using classification with multiple classes, you might need to average:
if (ncol(importance_df) > 2) {
  importance_df$Overall <- rowMeans(importance_df[, 1:(ncol(importance_df)-1)])
}


# Get top 8 most important variables
top_vars <- importance_df %>%
  dplyr::arrange(desc(Overall)) %>%
  dplyr::slice(1:12) %>%
  dplyr::pull(Variable)

top_vars <- gsub("`", "", top_vars)
# or: top_vars <- stringr::str_replace_all(top_vars, fixed("`"), "")



train_top <- train_data[, top_vars]
train_top$new_cover <-train_data$new_cover

test_top <- test_data[, top_vars]
test_top$new_cover <-test_data$new_cover

model_rf_top12 <- caret::train(new_cover ~ . , method = "rf", data = train_top[,-c(1:6)],
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)


saveRDS(model_rf_top12, file = "/home/jovyan/data-store/revision_analysis/model_outputs/model_rf_5class_top12.rds")

cm_rf <- confusionMatrix(data = predict(model_rf_top12, newdata = test_top[,-c(1:4)]),
                         as.factor(test_top$new_cover))
cm_rf


library(ggplot2)
library(reshape2)

library(ggplot2)

cm <- cm_rf$table
cm_df <- as.data.frame(cm)

library(ggplot2)

cm <- cm_rf$table
cm_df <- as.data.frame(cm)

p_cmf <- ggplot(cm_df, aes(x = Reference, y = Prediction)) +
  geom_tile(fill = NA, color = "black", linewidth = 0.7) +
  geom_text(aes(label = Freq), size = 5) +
  coord_fixed() +
  theme_bw(base_size = 12) +
  labs(x = "Reference class",
       y = "Predicted class") +
  theme(
    panel.grid = element_blank(),
    
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8)
  )

ggsave(file.path(out_dir, "cmf_random_forest.png"), width=8, height=6, dpi=800)


# plot(model_rf_top10)
library(caret)
library(ggplot2)
library(dplyr)
library(stringi)


# Predict class probabilities
probs <- predict(model_rf_top12, newdata = test_data[,-c(1:4)], type = "prob")

# Combine with true labels
probs$true_label <- test_data$new_cover

# Save as CSV
write.csv(probs, "/home/jovyan/data-store/revision_analysis/rf_class_probabilities_var12.csv", row.names = FALSE)


train_probs <- predict(model_rf_top12, newdata = train_data[,-c(1:4)], type = "prob")
train_probs$true_label <- train_data$new_cover

write.csv(train_probs, "/home/jovyan/data-store/revision_analysis/rf_train_class_probabilities_var12.csv", row.names = FALSE)

test_data <- cbind(test_data,probs)

train_data <- cbind(train_data,train_probs)

all_data = rbind(train_data,test_data)

library(dplyr)

# ----------------------------
# helper: add probs + predicted class
# ----------------------------
add_probs_and_pred <- function(model, data, drop_cols) {
  # probability predictions
  probs <- predict(model, newdata = data[, -drop_cols, drop = FALSE], type = "prob")
  
  probs <- as.data.frame(probs)
  
  # predicted class = argmax(probability)
  pred_class <- colnames(probs)[max.col(probs, ties.method = "first")]
  probs$pred_label <- pred_class
  
  # also store max prob (useful for filtering low-confidence)
  probs$pred_prob_max <- apply(probs[, colnames(probs), drop = FALSE], 1, max)
  
  # attach true label if it exists
  if ("new_cover" %in% names(data)) probs$true_label <- data$new_cover
  
  # bind back to original data
  out <- bind_cols(data, probs)
  out
}

# ----------------------------
# choose columns to drop for prediction
# IMPORTANT: must match your training features used in model_rf_top12
# (you used train_data[,-c(1:2,198:203)] earlier; here you wrote -c(1:4))
# ----------------------------
drop_cols <- c(1:4)  # keep as you requested (but ensure it's correct)

# ----------------------------
# Train + Test with predicted labels
# ----------------------------
train_out <- add_probs_and_pred(model_rf_top12, train_data, drop_cols)
test_out  <- add_probs_and_pred(model_rf_top12, test_data,  drop_cols)

# Combine (same columns)
all_data <- bind_rows(train_out, test_out)

# ----------------------------
# Save
# ----------------------------
write.csv(
  train_out %>% st_drop_geometry() %>% as.data.frame(),
  "/home/jovyan/data-store/revision_analysis/rf_train_probs_with_pred_var12.csv",
  row.names = FALSE
)

write.csv(
  test_out %>% st_drop_geometry() %>% as.data.frame(),
  "/home/jovyan/data-store/revision_analysis/rf_test_probs_with_pred_var12.csv",
  row.names = FALSE
)

write.csv(
  all_data %>% st_drop_geometry() %>% as.data.frame(),
  "/home/jovyan/data-store/revision_analysis/rf_all_probs_with_pred_var12.csv",
  row.names = FALSE
)
## 1) Repair duplicate column names (both tables, just in case)
names(all_data)  <- make.unique(names(all_data), sep = "__")
names(poly_data) <- make.unique(names(poly_data), sep = "__")
# (equivalently: all_data <- tibble::as_tibble(all_data, .name_repair = "unique"))

## 2) Make sure the key types match
all_data$FID_all_Fi  <- as.character(all_data$FID_all_Fi)
poly_data$FID_all_Fi <- as.character(poly_data$FID_all_Fi)

# ## 3) Keep one geometry per FID (prevents row multiplication)
# poly_geom <- poly_data %>%
#   dplyr::select(FID_all_Fi, geometry) %>%
#   dplyr::distinct(FID_all_Fi, .keep_all = TRUE)
# 
# ## 4) Only keep rows that match in BOTH tables  (inner join)
# all_data_geom <- dplyr::inner_join(all_data, poly_geom, by = "FID_all_Fi")
# 
# # Find all columns that are 'sfc'
# sfc_cols <- names(all_data_geom)[vapply(all_data_geom, inherits, logical(1), "sfc")]
# 
# if (length(sfc_cols) == 0) {
#   stop("No geometry ('sfc') column found. If you saved a copy before renaming, restore it.")
# }
# 
# # If there are multiple sfc columns, pick a sensible one:
# keep <- if ("geometry" %in% sfc_cols) {
#   "geometry"
# } else {
#   # choose the sfc with the fewest empty geometries (usually the real one)
#   empties <- sapply(all_data_geom[sfc_cols], function(g) sum(sf::st_is_empty(g)))
#   sfc_cols[ which.min(empties) ]
# }
# 
# # Set geometry pointer and drop the others
# sf::st_geometry(all_data_geom) <- keep
# drop_me <- setdiff(sfc_cols, keep)
# if (length(drop_me)) all_data_geom[drop_me] <- NULL
# 
# # Make sure CRS is set (use whatever is correct for your data)
# if (is.na(sf::st_crs(all_data_geom))) {
#   sf::st_crs(all_data_geom) <- 26913  # EPSG:26913, change if needed
# }
# 
# # Quick check
# print(sf::st_geometry_type(all_data_geom)[1])
# nrow(all_data_geom)
# 
# 
# out_gpkg <- "/data-store/iplant/home/nilangakoon/Fire_recovery/seedling_Detection/re-test-10-16-2025/output_var12_2.gpkg"
# sf::st_write(all_data_geom, dsn = out_gpkg, layer = "all_data_geom", delete_dsn = TRUE)


# Get saved predictions with class probabilities
preds <- model_rf_top12$pred

# View structure
str(preds)


importance_df <- varImp(model_rf_top12)$importance
importance_df$Variable <- rownames(importance_df)



# preds_with_vars <- preds %>%
#   left_join(train_data[, c("mean_CIVE", "mean_RI", "mean_GLA", "mean_GI",
#                            "mean_EXG", "mean_GRVI", "mean_VARI", "mean_MGRVI", "new_cover")],
#             by = c("obs" = "new_cover"))  # Or use an ID column if available


# Make train_data have a rowIndex column that matches caret preds$rowIndex
train_data2 <- train_data %>%
  mutate(rowIndex = row_number())

preds_with_vars <- preds %>%
  left_join(
    train_data2 %>% select(rowIndex,
                           median.EXG, median.VARI, mean.GRVI, max.chm,
                           median.GRVI, mean.VARI, new_cover),
    by = "rowIndex"
  )


# train_class_means <- train_data %>%
#   group_by(new_cover) %>%
#   summarise(across(c(median.EXG, median.VARI, mean.GRVI, max.chm,
#                      median.GRVI, mean.VARI),
#                    mean, na.rm = TRUE),
#             .groups = "drop")
# 
# preds_with_vars <- preds %>%
#   left_join(train_class_means, by = c("obs" = "new_cover"))


library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(grid)

# --- 0) Identify columns ---
class_cols <- c("Dead","Deciduous","Mature.trees","Seedlings","Shrub")
meta_cols  <- c("pred","obs","rowIndex","mtry","Resample", class_cols)

# use ALL numeric predictors (no need for name prefixes)
predictor_cols <- names(preds_with_vars)[sapply(preds_with_vars, is.numeric)]
predictor_cols <- setdiff(predictor_cols, meta_cols)

# add a row id to join later
preds_with_vars <- preds_with_vars %>% dplyr::mutate(.rid = dplyr::row_number())

# --- 1) Long tables: probabilities and variables ---
prob_long <- preds_with_vars %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(class_cols),
    names_to = "Category",
    values_to = "Probability"
  ) %>%
  dplyr::mutate(
    Probability = pmin(pmax(Probability, 1e-6), 1 - 1e-6),
    logit_prob  = qlogis(Probability)
  ) %>%
  dplyr::select(.rid, Category, Probability, logit_prob)

# Build vars_long with explicit namespaces
vars_long <- preds_with_vars %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(predictor_cols),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  dplyr::select(.rid, Variable, Value)

# Pair each class-prob with each predictor by row id
prob_var_long <- dplyr::inner_join(prob_long, vars_long, by = ".rid")

# Importance per class/variable: |Spearman rho| of logit(prob) vs predictor
importance_by_class <- prob_var_long %>%
  dplyr::group_by(Category, Variable) %>%
  dplyr::summarise(
    Importance = abs(stats::cor(logit_prob, Value, use = "complete.obs", method = "spearman")),
    n = sum(is.finite(logit_prob) & is.finite(Value)),
    .groups = "drop"
  ) %>%
  dplyr::filter(n > 10)

# Top 3 per class
top5 <- importance_by_class %>%
  dplyr::group_by(Category) %>%
  dplyr::slice_max(order_by = Importance, n = 5, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Category) %>%
  dplyr::mutate(Variable = forcats::fct_reorder(Variable, Importance, .desc = TRUE)) %>%
  dplyr::ungroup()

# Plot (bold axes/labels, no grids)
ggplot2::ggplot(top5, ggplot2::aes(x = Importance, y = Variable, fill = Category)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::labs(
    title = "",
    x = "Probability of importance",
    y = "Variables"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.line   = ggplot2::element_line(color = "black", linewidth = 1.1),
    axis.title  = ggplot2::element_text(face = "bold", color = "black"),
    axis.text   = ggplot2::element_text(face = "bold", color = "black"),
    axis.ticks  = ggplot2::element_line(color = "black"),
    axis.ticks.length = grid::unit(4, "pt"),
    panel.grid  = ggplot2::element_blank(),
    plot.title  = ggplot2::element_text(face = "bold", color = "black")
  )

ggsave(file.path(out_dir, "var_for_class_importance.png"), width=8, height=6, dpi=800)

# 
# # Gather predictions for each class
# prob_long <- preds_with_vars %>%
#   pivot_longer(cols = levels(preds$obs), names_to = "Category", values_to = "Probability")

# # Now compute correlation or model-based estimate of influence
# importance_by_class <- prob_long %>%
#   pivot_longer(cols = starts_with("mean_"), names_to = "Variable", values_to = "Value") %>%
#   group_by(Category, Variable) %>%
#   summarise(Importance = abs(cor(Probability, Value, use = "complete.obs")), .groups = "drop")

library(grid)  # for unit()
# 
# ggplot(importance_by_class, aes(x = Importance, y = Variable, fill = Category)) +
#   geom_col(position = "dodge") +
#   labs(title = "Probability of Variable Importance by Class",
#        x = "Probability",
#        y = "Variables") +
#   theme_minimal()

ggplot(importance_by_class, aes(x = Importance, y = Variable, fill = Category)) +
  geom_col(position = "dodge") +
  labs(
    title = "Probability of Variable Importance by Class",
    x = "Probability",
    y = "Variables"
  ) +
  theme_classic() +
  theme(
    # bold black axis lines
    axis.line = element_line(color = "black", linewidth = 1.1),
    # bold black axis labels
    axis.title = element_text(face = "bold", color = "black"),
    axis.text   = element_text(face = "bold", color = "black"),  # ← bold black tick labels
    # black tick marks
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    # no grids
    panel.grid = element_blank(),
    # (optional) bold title
    plot.title = element_text(face = "bold", color = "black")
  )

ggsave(file.path(out_dir, "var_for_class_importance_organized.png"), width=8, height=6, dpi=800)




# 
# 
# 
# 
# 
# ##################################
# 
library(dplyr); library(tidyr); library(ggplot2); library(pROC); library(forcats)

# --- inputs ---
# preds_with_vars has columns:
# obs (factor of classes), predictors (numeric), and probs per class if you have them (not needed here)

class_levels   <- levels(preds_with_vars$obs)
meta_cols      <- c("pred","obs","rowIndex","mtry","Resample","Dead","Deciduous","Mature.trees","Seedlings","Shrub")
predictor_cols <- names(preds_with_vars)[sapply(preds_with_vars, is.numeric)]
predictor_cols <- setdiff(predictor_cols, meta_cols)

# compute one-vs-rest AUC for every (class, variable)
auc_tbl <- purrr::map_dfr(class_levels, function(cls) {
  y <- as.integer(preds_with_vars$obs == cls)  # 1 = class, 0 = others
  purrr::map_dfr(predictor_cols, function(v) {
    x <- preds_with_vars[[v]]
    # AUC of variable alone as a scorer (direction chosen automatically)
    r <- try(pROC::roc(response = y, predictor = x, quiet = TRUE), silent = TRUE)
    auc <- if (inherits(r, "try-error")) NA_real_ else as.numeric(pROC::auc(r))
    dplyr::tibble(Category = cls, Variable = v, AUC = auc)
  })
})

# keep best K per class (optional) and plot heatmap
K <- 10
auc_top <- auc_tbl %>%
  group_by(Category) %>%
  slice_max(order_by = AUC, n = K, with_ties = FALSE) %>%
  ungroup()


library(dplyr); library(ggplot2); library(forcats)

# make a facet-specific factor and strip Category from labels at draw time
auc_top2 <- auc_top %>%
  mutate(VarFac = paste(Category, Variable, sep = "___")) %>%
  group_by(Category) %>%
  mutate(VarFac = forcats::fct_reorder(VarFac, AUC, .desc = TRUE)) %>%
  ungroup()

# keep only finite AUCs
auc_top2_ok <- auc_top2 %>%
  filter(is.finite(AUC))

# order within each class
auc_top2_ok <- auc_top2_ok %>%
  group_by(Category) %>%
  mutate(VarFac = fct_reorder(VarFac, AUC, .desc = TRUE)) %>%
  ungroup()

# ggplot(auc_top2_ok, aes(x = AUC, y = VarFac, fill = AUC)) +
#   geom_tile(width = 0.45, height = 0.9, color = "white") +  # explicit size
#   facet_wrap(~ Category, scales = "free_y") +
#   scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
#   scale_x_continuous(limits = c(0.5, 1), expand = expansion(mult = c(0, 0.02))) +
#   scale_fill_viridis_c(limits = c(0.5, 1), oob = scales::squish, name = "AUC") +
#   labs(title = "Single-variable class separation (one-vs-rest AUC)",
#        x = "AUC (0.5 = no separation, 1 = perfect)", y = "Variable") +
#   theme_classic() +
#   theme(
#     axis.line  = element_line(color = "black", linewidth = 1.1),
#     axis.title = element_text(face = "bold", color = "black"),
#     axis.text  = element_text(face = "bold", color = "black"),
#     axis.ticks = element_line(color = "black")
#   )


# assume auc_top2_ok already filtered to finite AUC and ordered
auc_top2_ok <- auc_top2_ok %>%
  mutate(VarLabel = sub("^.*___", "", VarFac))   # strip the "Category___" prefix

# ---- 1) Clean labels + drop .rid if it accidentally got in ----
auc_pub <- auc_top2_ok %>%
  filter(VarLabel != ".rid", !is.na(AUC)) %>%   # drop row-id artifact
  mutate(
    VarLabel = sub("^.*___", "", VarFac),        # strip prefix for legend/labels
    # reorder within each facet so top AUC appears at top
    VarFac2  = fct_reorder(VarFac, AUC)
  )

# ---- 2) Okabe-Ito palette (colorblind-safe, publication standard) ----
okabe_ito <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

# Use as many colors as needed (recycles if >8; ideally keep <=8 variables shown)
pal_vals <- setNames(rep(okabe_ito, length.out = length(unique(auc_pub$VarLabel))),
                     unique(auc_pub$VarLabel))

p_auc_pub <- ggplot(auc_pub, aes(y = VarFac2, color = VarLabel)) +
  geom_segment(aes(x = 0.5, xend = AUC, yend = VarFac2),
               linewidth = 1.15, lineend = "round") +
  geom_point(aes(x = AUC), size = 3.0) +
  facet_wrap(~ Category, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
  scale_x_continuous(
    limits = c(0.5, 1.0),
    breaks = seq(0.5, 1.0, 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(values = pal_vals, name = "Variable") +
  labs(
    title = "Single-variable class separation based on AUC",
    x = "AUC (0.5 = no separation, 1 = perfect)",
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line  = element_line(color = "black", linewidth = 1.0),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text  = element_text(face = "bold", color = "black", size = 12),
    axis.ticks = element_line(color = "black"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = ggplot2::margin(10, 15, 10, 10)
  )

p_auc_pub

ggsave(file.path(out_dir, "auc_lollipop_pub.png"), width=8, height=6, dpi=800)

# ggplot(auc_top2_ok, aes(y = VarFac, color = VarLabel)) +
#   geom_segment(aes(x = 0.5, xend = AUC, yend = VarFac),
#                linewidth = 1, lineend = "round") +
#   geom_point(aes(x = AUC), size = 2.6) +
#   facet_wrap(~ Category, scales = "free_y") +
#   scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
#   scale_x_continuous(limits = c(0.5, 1), expand = expansion(mult = c(0, 0.02))) +
#   scale_color_viridis_d(name = "Variable") +   # or: scale_color_brewer(palette = "Set2")
#   labs(title = "Single-variable class separation based on AUC",
#        x = "AUC", y = "Variable") +
#   theme_classic() +
#   theme(
#     axis.line  = element_line(color = "black", linewidth = 1.1),
#     axis.title = element_text(face = "bold", color = "black"),
#     axis.text  = element_text(face = "bold", color = "black"),
#     axis.ticks = element_line(color = "black")
#   )

# ggplot(auc_top2, aes(x = AUC, y = VarFac, fill = AUC)) +
#   geom_tile(color = "white") +
#   facet_wrap(~Category, scales = "free_y") +
#   scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +  # show only Variable
#   scale_fill_viridis_c(limits = c(0.1, 1), oob = scales::squish, name = "AUC") +
#   labs(x = "AUC", y = "Variable")

# # order variables by within-class AUC (helps readability)
# auc_top <- auc_top %>%
#   group_by(Category) %>%
#   mutate(Variable = reorder_within(Variable, AUC, Category)) %>% # helper from tidytext
#   ungroup()

# If you don't have tidytext::reorder_within, do a manual reorder per facet:
# auc_top <- auc_top %>% group_by(Category) %>% mutate(ord = rank(-AUC, ties.method="first")) %>% ungroup()


# library(dplyr)
# library(ggplot2)
# library(forcats)
# library(grid)
# 
# # 1) take the top 5 variables for each class
# top5 <- prob_long %>%
#   group_by(Category) %>%
#   slice_max(order_by = Importance, n = 5, with_ties = FALSE) %>%
#   ungroup()
# 
# # optional: order variables within each class by importance
# top5 <- top5 %>%
#   group_by(Category) %>%
#   mutate(Variable = fct_reorder(Variable, Importance, .desc = TRUE)) %>%
#   ungroup()
# 
# # 2) plot
# ggplot(top5, aes(x = Importance, y = Variable, fill = Category)) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "Top 5 Variables per Class by Importance",
#     x = "Probability",
#     y = "Variables"
#   ) +
#   theme_classic() +
#   theme(
#     axis.line   = element_line(color = "black", linewidth = 1.1),
#     axis.title  = element_text(face = "bold", color = "black"),
#     axis.text   = element_text(face = "bold", color = "black"),
#     axis.ticks  = element_line(color = "black"),
#     axis.ticks.length = unit(4, "pt"),
#     panel.grid  = element_blank(),
#     plot.title  = element_text(face = "bold", color = "black")
#   )
# 
# 
# 



library(pdp)


install.packages("plotly")  # Only if not already installed
library(plotly)

# randomForest::importance(model_rf_top12$finalModel) %>% 
#   .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
#   plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
#           width = 350, height = 300)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) Importance table (class-specific) ----
imp_mat <- randomForest::importance(model_rf_top12$finalModel)

# drop overall columns if present
drop_cols <- intersect(colnames(imp_mat), c("MeanDecreaseAccuracy", "MeanDecreaseGini"))
if (length(drop_cols) > 0) imp_mat <- imp_mat[, setdiff(colnames(imp_mat), drop_cols), drop = FALSE]

imp_df <- as.data.frame(imp_mat)
imp_df$Variable <- rownames(imp_df)

# long format for ggplot
imp_long <- imp_df %>%
  tidyr::pivot_longer(
    cols = -Variable,
    names_to = "Class",
    values_to = "Importance"
  )

# Optional: keep only top N variables by overall mean importance for readability
topN <- 20
top_vars <- imp_long %>%
  group_by(Variable) %>%
  summarise(Overall = mean(Importance, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = topN) %>%
  pull(Variable)

imp_long <- imp_long %>%
  filter(Variable %in% top_vars)

# order axes (nice for papers)
imp_long <- imp_long %>%
  group_by(Variable) %>%
  mutate(Overall = mean(Importance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Variable = reorder(Variable, Overall),
    Class = factor(Class, levels = colnames(imp_mat))
  )

# ---- 2) Publication-ready heatmap ----
p_imp <- ggplot(imp_long, aes(x = Class, y = Variable, fill = Importance)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "C", name = "Importance") +
  labs(
    title = "Class specific variable importance",
    x = "Class",
    y = "Variable"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    plot.margin = ggplot2::margin(10, 15, 10, 10)
  )

p_imp

ggsave(file.path(out_dir, "heatmap.png"), width=8, height=6, dpi=800)

# 
# 
# partial_rf <- partial(model_rf, pred.var = "mean_CIVE", which.class = "Dead", prob = TRUE)
# plotPartial(partial_rf)




# Get importance
importance_df <- varImp(model_rf_top12)$importance

# Handle multiclass: compute overall importance
if (ncol(importance_df) > 1) {
  importance_df$Overall <- rowMeans(importance_df[, 1:ncol(importance_df)])
}

# Add variable names as a column
importance_df$Variable <- rownames(importance_df)

# Select top N variables (e.g., top 10)
top_vars <- importance_df %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 12)

# Plot
ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "",
       x = "Variable",
       y = "Importance (Overall)") +
  theme_classic() +
  theme(
    # bold black axis lines
    axis.line = element_line(color = "black", linewidth = 1.1),
    # bold black axis labels
    axis.title = element_text(face = "bold", color = "black"),
    axis.text   = element_text(face = "bold", color = "black"),  # ← bold black tick labels
    # black tick marks
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    # no grids
    panel.grid = element_blank(),
    # (optional) bold title
    plot.title = element_text(face = "bold", color = "black"))

ggsave(file.path(out_dir, "class_var_importance.png"), width=8, height=6, dpi=800)


model_metrics <- model_rf_top12$results

metrics_long <- model_metrics %>% 
  pivot_longer(cols = c(Accuracy, Kappa, Mean_F1, Mean_Sensitivity, Mean_Specificity), 
               names_to = "Metric", values_to = "Value")

ggplot(metrics_long, aes(x = mtry, y = Value)) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Model Performance",
       x = "mtry",
       y = "value") +
  theme_classic() +
  theme(
    # bold black axis lines
    axis.line = element_line(color = "black", linewidth = 1.1),
    # bold black axis labels
    axis.title = element_text(face = "bold", color = "black"),
    axis.text   = element_text(face = "bold", color = "black"),  # ← bold black tick labels
    # black tick marks
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    # no grids
    panel.grid = element_blank(),
    # (optional) bold title
    plot.title = element_text(face = "bold", color = "black"))

ggsave(file.path(out_dir, "model_performace.png"), width=8, height=6, dpi=800)



# 
# # Extract the final model
# rf_final <- model_rf_top12$finalModel
# 
# # Drop the .outcome column from x
# x <- model_rf_top12$trainingData[, setdiff(names(model_rf_top12$trainingData), ".outcome")]
# y <- model_rf_top12$trainingData$.outcome
# 
# # Retrain
# rf_manual <- randomForest(x = x, y = y, importance = TRUE)
# 
# # Try again to get per-class importance
# imp <- randomForest::importance(rf_manual, type = 1)
# # Convert to long format
# library(reshape2)
# imp_long <- melt(imp, varnames = c("Variable", "Class"), value.name = "Importance")
# 
# library(ggplot2)
# ggplot(imp_long, aes(x = reorder(Variable, Importance), y = Importance, fill = Class)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   theme_minimal() +
#   labs(
#     title = "Per-Class Variable Importance (Gini)",
#     x = "Variables",
#     y = "Importance",
#     fill = "Class"
#   )
# 
# 
# ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(title = "Variable Importance", x = "Variable", y = "Mean Decrease Gini") +
#   theme_minimal()
# 
# install.packages("pdp")
# library(pdp)
# 
# library(gridExtra)
# install.packages("gridExtra")
# 
# # Get variable importance
# 
# 
# 
# top_vars <- rownames(imp)[order(imp[, "MeanDecreaseAccuracy"], decreasing = TRUE)]
# top_vars <- top_vars[1:2]  # Choose top 2 features for brevity
# 
# 
# imp_df <- as.data.frame(imp)
# imp_df$Variable <- rownames(imp)
# 
# top_vars <- imp_df %>%
#   arrange(desc(MeanDecreaseAccuracy)) %>%
#   slice_head(n = 10)
# 
# ggplot(top_vars, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(x = "Variable", y = "MeanDecreaseAccuracy", title = "Top 10 Variable Importances") +
#   theme_minimal()
# 
# 


# # Create PDP for a feature (e.g., Petal.Width) and a class (e.g., "setosa")
# pdp_setosa <- partial(
#   object = model_rf_top12,
#   pred.var = "median_CIVE",
#   which.class = "Seedlings",
#   prob = TRUE,     # if you want class probability
#   plot = TRUE,
#   rug = TRUE
# )
# 


# Get response classes
rf_final <- model_rf_top12$finalModel
predictor_data <- model_rf_top12$trainingData
predictors <- names(predictor_data)[-1]  # exclude response
response <- predictor_data[[1]]
classes <- levels(response)

# # Store all plots in a list
# # Create a nested list: pdp_ice_plots[[class]][[variable]]
# pdp_ice_plots <- list()
# 
# for (cls in classes) {
#   pdp_ice_plots[[cls]] <- list()
#   
#   for (var in predictors) {
#     pd <- partial(
#       object = rf_final,
#       pred.var = var,
#       which.class = cls,
#       prob = TRUE,
#       ice = TRUE,
#       center = TRUE,     # Center ICE curves for better comparison
#       plot = FALSE,
#       train = predictor_data
#     )
#     
#     p <- autoplot(pd, alpha = 0.2) +
#       ggtitle(paste("ICE plots for Class:", cls, "|Variable:", var)) +
#       theme_minimal()
#     
#     pdp_ice_plots[[cls]][[var]] <- p
#   }
# }
# 
# 
# # Show ICE plots for one class (e.g., "setosa")
# gridExtra::grid.arrange(
#   grobs = pdp_ice_plots[["Seedlings"]],
#   ncol = 2
# )
# 
# # Show ICE plots for one variable (e.g., "Petal.Width")
# gridExtra::grid.arrange(
#   grobs = lapply(classes, function(cls) pdp_ice_plots[[cls]][["median_CIVE"]]),
#   ncol = 2
# )
# 
# for (var in predictors) {
#   plot_grid <- gridExtra::grid.arrange(
#     grobs = lapply(classes, function(cls) pdp_ice_plots[[cls]][[var]]),
#     ncol = 2,
#     top = paste("Individual Conditional Expectation plot for Variable:", var)
#   )
#   
#   ggsave(filename = paste0("ICE_grid_", var, ".png"), plot = plot_grid, width = 10, height = 6)
# }
# 
# 
# library(pdp)
# library(ggplot2)
# 
# # Partial with ICE and PDP overlay
# pd <- partial(
#   rf_final,
#   pred.var = "Median VARI",
#   which.class = "Seedlings",
#   prob = TRUE,
#   ice = TRUE,
#   center = TRUE,
#   plot = FALSE,
#   train = predictor_data
# )
# 
# 
# # Light pink for ICE curves, dark pink for PDP line
# ggplot(pd, aes(x = median_CIVE, y = yhat)) +
#   geom_line(aes(group = interaction(yhat.id)), color = "#56B4E9", alpha = 0.4) +  # ICE lines
#   stat_summary(fun = mean, geom = "line", color = "#E69F00", size = 1.2) +        # PDP line
#   theme_minimal() +
#   labs(
#     title = "Median_CIVE on class seedlings",
#     x = "median_CIVE",
#     y = "Predicted Probability"
#   )
# 
# # ggsave(filename = paste0("Median_CIVE on class seedlings", ".png"), plot = plot_grid, width = 10, height = 6)
# 
# 
# # Compute 2.5% and 97.5% quantiles per x (Petal.Width)
# quantiles <- pd %>%
#   group_by(median_EXG) %>%
#   summarise(
#     lower = quantile(yhat, 0.025),
#     upper = quantile(yhat, 0.975),
#     .groups = "drop"
#   )
# 
# 
# library(ggplot2)
# 
# ggplot(pd, aes(x = median_EXG, y = yhat)) +
#   # 95% quantile band (must map x here because inherit.aes = FALSE)
#   geom_ribbon(
#     data = quantiles,
#     aes(x = median_EXG, ymin = lower, ymax = upper, fill = "95% band"),
#     alpha = 0.25, colour = NA, inherit.aes = FALSE
#   ) +
#   # ICE lines
#   geom_line(aes(group = yhat.id, color = "ICE"), alpha = 0.20) +
#   # PDP line (mean)
#   stat_summary(aes(color = "PDP"), fun = mean, geom = "line", linewidth = 1.2) +
#   # Legends & colors
#   scale_color_manual(
#     name  = "Lines",
#     values = c("ICE" = "#56B4E9", "PDP" = "#009E73")
#   ) +
#   scale_fill_manual(
#     name  = "Band",
#     values = c("95% band" = "#E69F00")
#   ) +
#   guides(
#     color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.2)),
#     fill  = guide_legend(override.aes = list(alpha = 0.35))
#   ) +
#   labs(
#     title = "Individual Conditional Expectation plot + PDP with 95% Quantile Band",
#     x = "median_EXG",
#     y = "Predicted Probability"
#   ) +
#   theme_classic() +
#   theme(
#     axis.line   = element_line(color = "black", linewidth = 1.1),
#     axis.title  = element_text(face = "bold", color = "black"),
#     axis.text   = element_text(face = "bold", color = "black"),
#     axis.ticks  = element_line(color = "black"),
#     legend.title = element_text(face = "bold"),
#     legend.position = "right"
#   )
# 
# 



#####################################

# packages
library(pdp)
library(dplyr)
library(ggplot2)
library(rlang)     # for sym / .data pronoun
# optional: caret for getting predictors() from a caret::train object
# library(caret)

# -------------------------
# 1) pick variables to plot
# -------------------------
# Try caret::predictors(); fall back to names in your training data
vars_all <- tryCatch(caret::predictors(rf_final),
                     error = function(e) names(predictor_data))


# make sure names are unique
names(predictor_data) <- make.unique(names(predictor_data))

# keep only predictors that actually exist in the data
vars_num <- intersect(vars_all, names(predictor_data))

# test numeric with a type-safe vapply -> logical(1) per column
num_idx  <- vapply(predictor_data[vars_num], is.numeric, logical(1))
vars_num <- vars_num[num_idx]

# # keep only predictors present and numeric (ICE works best for numeric)
# vars_num <- vars_all[vars_all %in% names(predictor_data)]
# vars_num <- vars_num[sapply(predictor_data[vars_num], is.numeric)]

# -------------------------
# 2) a helper that plots 1 variable and saves it
# -------------------------
plot_ice_pdp <- function(var, cls = "Deciduous",
                         model = rf_final, train = predictor_data,
                         out_dir = "plots_ice_pdp", width = 7, height = 4.5) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Compute partial + ICE (a modest grid for speed; adjust if needed)
  pd <- pdp::partial(
    object = model,
    pred.var = var,
    which.class = cls,
    prob = TRUE,
    ice = TRUE,
    center = TRUE,
    plot = FALSE,
    grid.resolution = 50,
    train = train
  )
  
  # 95% band across ICE at each x
  x_sym <- rlang::sym(var)
  quantiles <- pd %>%
    dplyr::group_by(!!x_sym) %>%
    dplyr::summarise(
      lower = stats::quantile(yhat, 0.025, na.rm = TRUE),
      upper = stats::quantile(yhat, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot (Okabe–Ito colors + legend)
  p <- ggplot(pd, aes(x = .data[[var]], y = yhat)) +
    geom_ribbon(
      data = quantiles,
      aes(x = .data[[var]], ymin = lower, ymax = upper, fill = "95% band"),
      alpha = 0.25, colour = NA, inherit.aes = FALSE
    ) +
    geom_line(aes(group = yhat.id, color = "ICE"), alpha = 0.20) +
    stat_summary(aes(color = "PDP"), fun = mean, geom = "line", linewidth = 1.2) +
    scale_color_manual(name = "Lines",
                       values = c("ICE" = "#56B4E9", "PDP" = "#009E73")) +
    scale_fill_manual(name = "Band",
                      values = c("95% band" = "#E69F00")) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.2)),
      fill  = guide_legend(override.aes = list(alpha = 0.35))
    ) +
    labs(
      title = paste0("ICE + PDP (95% band) — Class: ", cls, " — ", var),
      x = var,
      y = "Predicted Probability"
    ) +
    theme_classic() +
    theme(
      axis.line   = element_line(color = "black", linewidth = 1.1),
      axis.title  = element_text(face = "bold", color = "black"),
      axis.text   = element_text(face = "bold", color = "black"),
      axis.ticks  = element_line(color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    )
  
  # Save
  outfile <- file.path(out_dir, sprintf("ICE_PDP_%s_%s.png", cls, var))
  ggsave(outfile, p, width = width, height = height, dpi = 800)
  message("Saved: ", outfile)
  
  invisible(p)
}

# -------------------------
# 3) run for every variable (change class if needed)
# -------------------------
target_class <- "Seedlings"   # e.g., "Dead", "Deciduous", "Mature.trees", "Shrub", "Seedlings"
lapply(vars_num, plot_ice_pdp, cls = target_class,out_dir = out_dir)

# If you want to do ALL classes:
# classes <- levels(predictor_data$obs)  # or your class vector
# for (cls in classes) lapply(vars_num, plot_ice_pdp, cls = cls)


#########################


vars_keep <- importance_by_class %>%
  arrange(desc(Importance)) %>%
  distinct(Variable, .keep_all = TRUE) %>%
  slice_head(n = 6) %>%        # e.g., best 6 overall
  pull(Variable)


library(ggplot2)
library(forcats)

heat_df <- importance_by_class %>%
  filter(Variable %in% vars_keep) %>%
  # order rows by mean importance across classes (nice layout)
  group_by(Variable) %>%
  mutate(VarOrder = mean(Importance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Variable = fct_reorder(Variable, VarOrder),              # rows
    Category = factor(Category)                               # columns (set order if you like)
  )

ggplot(heat_df, aes(x = Category, y = Variable, fill = Importance)) +
  geom_tile(color = "white", linewidth = 0.4) +
  # color-blind friendly (viridis)
  scale_fill_viridis_c(name = "Importance",
                       limits = c(0, NA), oob = scales::squish) +
  labs(title = "Class Importance Heatmap",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 30, hjust = 1, face = "bold", color = "black"),
    axis.text.y  = element_text(face = "bold", color = "black"),
    axis.line    = element_blank(),   # keep it clean like your mock
    axis.ticks   = element_blank(),
    legend.title = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "class_heatmap.png"), width=8, height=6, dpi=800)






library(randomForest)
library(pdp)
library(ggplot2)
library(dplyr)

# Use your model and training data
rf_final <- model_rf_top12$finalModel
predictor_data <- model_rf_top12$trainingData
predictors <- names(predictor_data)[-1]
response <- predictor_data[[1]]
classes <- levels(response)

# Directory to save plots
output_dir <- "/home/jovyan/data-store/revision_analysis/figures/ice_plots"
dir.create(output_dir, showWarnings = FALSE)


for (cls in classes) {
  for (var in predictors) {
    
    # 1. Compute ICE + PDP
    pd <- partial(
      object = rf_final,
      pred.var = var,
      which.class = cls,
      prob = TRUE,
      ice = TRUE,
      center = TRUE,
      plot = FALSE,
      train = predictor_data
    )
    
    # 2. Compute quantiles at each x
    colnames(pd)[1] <- "x"  # standardize for generality
    quantiles <- pd %>%
      group_by(x) %>%
      summarise(
        lower = quantile(yhat, 0.025, na.rm = TRUE),
        upper = quantile(yhat, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 3. Generate the plot
    p <- ggplot(pd, aes(x = x, y = yhat)) +
      geom_ribbon(
        data = quantiles,
        aes(x = x, ymin = lower, ymax = upper),
        fill = "#D55E00",
        alpha = 0.3,
        inherit.aes = FALSE
      ) +
      geom_line(aes(group = yhat.id), color = "#0072B2", alpha = 0.3) +  # ICE lines
      stat_summary(fun = mean, geom = "line", color = "deeppink", size = 1.2) +  # PDP line
      theme_minimal() +
      labs(
        title = paste("ICE + PDP | Class:", cls, "| Variable:", var),
        x = var,
        y = "Predicted Probability"
      )
    
    # 4. Save to file
    filename <- paste0(output_dir, "/", cls, "_", var, "_ICE_PDP.png")
    ggsave(filename, plot = p, width = 7, height = 5, dpi = 800)
  }
}


# # stopCluster(cl); remove(cl)
# # # Unregister the doParallel cluster so that we can use sequential operations
# # # if needed; details at https://stackoverflow.com/a/25110203/5193830
# # registerDoSEQ()
# saveRDS(model_rf, file = "/data-store/iplant/home/nilangakoon/Fire_recovery/new_var_smoothed_chm/model_rf_5class_chm_7_21_2025.rds")
# 
# model_rf <- readRDS("/data-store/iplant/home/nilangakoon/Fire_recovery/new_var_smoothed_chm/model_rf_5class_top10.rds")
# # 
# # #performance
# # model_rf$times$everything
# 
# plot(model_rf)
# 
# #confusion metrix
# 
# cm_rf <- confusionMatrix(data = predict(model_rf, newdata = test_data[,-c(1:4)]),
#                          as.factor(test_data$new_cover))
# cm_rf
# 
# wd = "/home/jovyan/data-store/plots_seedlings/variables"
# 
# file_list2 = dir(file.path(wd), pattern = ".shp", full.names = FALSE, ignore.case = TRUE) 
# names_to_cloud = substr(file_list2, 0, nchar(file_list2)-4) # truncate the file names.
# 
# 
# 
# 
# for (i in 1:length(file_list2)){
#   print(i)
#   
#   mean_values_df <- st_read(file.path(wd,paste0(names_to_cloud[i],".shp")))
#   
#   
#   pred_data <- data.frame(mean_values_df)
#   
#   excluded_cols <- c("geometry")
#   newdata <- pred_data %>% select(-all_of(excluded_cols))
#   
#   old_names <- colnames(newdata)
#   old_names <- old_names[-c(1:3)]
#   
#   name_map <- setNames(new_names, old_names)
#   
#   # Then apply:
#   newdata <- newdata %>%
#     rename_with(~ name_map[.x], .cols = names(name_map))
#   
#   # Check names(pred_data) first
#   
#   
#   data_pr1 = data.frame(predict(model_rf_top10, newdata = newdata))
#   names(data_pr1) <- "prediction"
#   mean_values_df$prediction <- data_pr1[[1]]
#   file_name2 <- file.path(wd,"predictions",paste0(names_to_cloud[i],"_prediction.shp"))
#   
#   sf::st_write(mean_values_df, file_name2, delete_dsn = TRUE)
# }
# 
# model_rf$finalModel
# 
# # Predictor importance
# 
# caret::varImp(model_rf_top10)$importance %>%
#   as.matrix %>% 
#   plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
#           width = 350, height = 300)
# 
# 
# randomForest::importance(model_rf_top10$finalModel) %>% 
#   .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
#   plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
#           width = 350, height = 300)
# 
# randomForest::varImpPlot(model_rf_top10$finalModel)

#############SVM############################################


# Grid of tuning parameters
svm_grid <- expand.grid(cost = c(0.2, 0.5, 1),
                        Loss = c("L1", "L2"))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_svm <- caret::train(new_cover ~ . , method = "svmLinear3", data = train_data[,-c(1:4)],
                          allowParallel = TRUE,
                          tuneGrid = svm_grid,
                          trControl = ctrl)


# stopCluster(cl); remove(cl)
# registerDoSEQ()
# Warning message:
# In train.default(x, y, weights = w, ...) :
#   Class probabilities were requested for a model that does not implement them
# (see why above)
saveRDS(model_svm, file = "/home/jovyan/data-store/revision_analysis/model_outputs/model_svm_5_class.rds")



  
model_svm <- readRDS("/home/jovyan/data-store/revision_analysis/model_svm_5_class.rds")

# Model summary & confusion matrix

model_svm$times$everything # total computation time

plot(model_svm) # tuning results

# The confusion matrix using the test dataset
cm_svm <- confusionMatrix(data = predict(model_svm, newdata = test_data),
                          as.factor(test_data$new_cover))
cm_svm



##################Neural Network#####################################
# Grid of tuning parameters
nnet_grid <- expand.grid(size = c(5, 10, 15),
                         decay = c(0.001, 0.01, 0.1))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
# model_nnet <- train(new_cover ~ ., method = 'nnet', data = train_data[,-c(1:4)],
#                     importance = TRUE,
#                     maxit = 1000, # set high enough so to be sure that it converges
#                     allowParallel = TRUE,
#                     tuneGrid = nnet_grid,
#                     trControl = ctrl)
# 
# 

# ctrl_nopar2 <- trainControl(
#   method = "repeatedcv",
#   number = 5,
#   repeats = 3,
#   classProbs = FALSE,
#   summaryFunction = defaultSummary,
#   allowParallel = FALSE
# )

model_nnet <- train(
  new_cover ~ .,
  data = train_data[,-c(1:4)],
  method = "nnet",
  tuneGrid = nnet_grid,
  trControl = ctrl,
  preProcess = c("center", "scale"),
  maxit = 1000,
  trace = FALSE,
  MaxNWts = 200000
)

# stopCluster(cl); remove(cl)


# registerDoSEQ()

saveRDS(model_nnet, file = "/home/jovyan/data-store/revision_analysis/model_outputs/model_nnet_5class.rds")

model_nnet <- readRDS("/home/jovyan/data-store/seedling_Detection/model_nnet_5class.rds")
# Model summary & confusion matrix

model_nnet$times$everything # total computation time

plot(model_nnet) # tuning results


# The confusion matrix using the test dataset
cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = test_data),
                           as.factor(test_data$new_cover))
cm_nnet

cols <- grDevices::colorRampPalette(colors = brewer.pal(n = 9, name = "YlGnBu"))(10)

dev.new(height=4, width=4)
garson(model_nnet) +
  scale_y_continuous('Rel. Importance') + 
  scale_fill_gradientn(colours = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.new(height=4, width=4)
cols_rank_import <- cols[rank(garson(model_nnet, bar_plot = FALSE)$rel_imp)]
plotnet(model_nnet, circle_col = list(cols_rank_import, 'lightblue'))



###Compare models###################

# Create model_list
model_list <- list(rf = model_rf, svm = model_svm, nnet = model_nnet)
# Pass model_list to resamples()
resamples <- caret::resamples(model_list)

# All metrics with boxplots
bwplot(resamples)

bwplot(
  resamples,
  metric = c("Accuracy","Kappa","Mean_Detection_Rate","Mean_Balanced_Accuracy"),
  layout = c(2,2),
  scales = list(x=list(cex=1.4), y=list(cex=1.4)),
  strip = strip.custom(par.strip.text=list(cex=1.4,font=2))
)


########################## other factors on accuracy of prediction

##with logistic regression

# Detected_correctly = 1 (TP)
# Detected_wrong = 0 (FP or FN)
library(dplyr)
library(readr)

in_csv  <- "/home/jovyan/data-store/revision_analysis/rf_all_probs_with_pred_var12.csv"
out_csv <- "/home/jovyan/data-store/revision_analysis/rf_all_probs_with_pred_var12_with_correct.csv"

df <- readr::read_csv(in_csv, show_col_types = FALSE)

# choose column names that exist in your file
true_col <- if ("true_label" %in% names(df)) "true_label" else if ("new_cover" %in% names(df)) "new_cover" else stop("No true label column found")
pred_col <- if ("pred_label" %in% names(df)) "pred_label" else if ("pred" %in% names(df)) "pred" else stop("No predicted label column found")

df2 <- df %>%
  mutate(
    true = as.character(.data[[true_col]]),
    pred = as.character(.data[[pred_col]]),
    
    # overall correctness (1/0)
    correct = as.integer(!is.na(true) & !is.na(pred) & (true == pred)),
    
    # detection-type label (useful later)
    match_type = case_when(
      is.na(true) & !is.na(pred) ~ "FP_like",   # no truth but predicted (rare in your setup)
      !is.na(true) & is.na(pred) ~ "FN_like",   # truth but no prediction (rare if always predicts)
      true == pred               ~ "TP",
      TRUE                       ~ "FP_FN"      # mismatch
    )
  )

readr::write_csv(df2, out_csv)
out_csv

model_acc <- glm(correct ~ Fire + Seedling_p + Slope + mean.chm,
                 data = df2,
                 family = binomial)

summary(model_acc)

library(ggplot2)

# ggplot(df2, aes(x = Seedling_p, y = correct)) +
#   stat_summary(fun = mean, geom = "bar", fill = "orange") +
#   # geom_smooth(method = "glm", method.args = list(family = "binomial"),
#   #             color = "darkgreen", fill = "lightgreen") +
#   labs(x = "Seedling probability",
#        y = "Detection probability") +
#   theme_classic()
df2$Seedling_p <- factor(
  df2$Seedling_p,
  levels = c("Extremely low", "Low", "Moderate", "High", "Extreme")
)

ggplot(df2, aes(x = Seedling_p, y = correct)) +
  stat_summary(fun = mean, geom = "bar", fill = "orange") +
  labs(
    x = "Seedling probability",
    y = "Detection probability"
  ) +
  theme_classic()

ggsave(file.path(out_dir, "seedling_p.png"), width=8, height=6, dpi=800)


ggplot(df2, aes(x = Slope, y = correct)) +
  stat_summary(fun = mean, geom = "bar", fill = "orange") +
  # stat_summary(fun = mean, geom = "bar", fill = "orange") "binomial"),
  #             color = "blue", fill = "lightblue") +
  labs(x = "Slope",
       y = "Detection probability") +
  theme_classic()

ggsave(file.path(out_dir, "slope_acc.png"), width=8, height=6, dpi=800)


ggplot(df2, aes(x = Fire, y = correct)) +
  stat_summary(fun = mean, geom = "bar", fill = "orange") +
  # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(x = "Fire ",
       y = "Detection accuracy") +
  theme_classic()

ggsave(file.path(out_dir, "fire_acc.png"), width=8, height=6, dpi=800)


ggplot(df2, aes(x = mean.chm, y = correct)) +
  # stat_summary(fun = mean, geom = "bar", fill = "orange") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              color = "darkgreen", fill = "lightgreen") +
  labs(x = "Seedling height(m)",
       y = "Detection probability") +
  theme_classic()

ggsave(file.path(out_dir, "height_acc.png"), width=8, height=6, dpi=800)


library(dplyr)

data %>%
  mutate(
    height_bin = cut(Seedling_p, breaks = 10),
    slope_bin = cut(Slope, breaks = 10)
  ) %>%
  group_by(height_bin, slope_bin) %>%
  summarise(acc = mean(correct), .groups = "drop") %>%
  ggplot(aes(height_bin, slope_bin, fill = acc)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Accuracy") +
  labs(x = "Seedling height", y = "Slope") +
  theme_classic()

#Detection probability vs height
# faceted by Fire severity

ggplot(df2, aes(x = mean.chm, y = correct)) +
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              color="darkgreen") +
  facet_wrap(~Fire) +
  labs(x="Seedling height",
       y="Detection probability") +
  theme_classic()

ggsave(file.path(out_dir, "height_fire.png"), width=8, height=6, dpi=800)





























































# library(ggplot2)
# library(caret)
# 
# # Convert resamples object to long data
# res_df <- as.data.frame(resamples)
# 
# # Reshape for ggplot
# res_long <- reshape2::melt(res_df, id.vars = "Resample")
# 
# # Plot manually with facets and custom x-scales
# ggplot(res_long, aes(value, variable)) +
#   geom_boxplot() +
#   facet_wrap(~ variable, scales = "free_x") +  # allow per-panel x axis
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 



#paired t test to compare model performances
t_tests <- resamples %>%
  diff(metric = "Accuracy") %>%
  summary
t_tests


###Visualize classifications############



brick_input = brick(input)


system.time({
  predict_rf <- raster::predict(object = brick_input, overwrite = TRUE,
                                model = model_rf, type = 'raw',filename="predict_rf.tif")
  predict_svm <- raster::predict(object = brick_input,overwrite = TRUE,
                                 model = model_svm, type = 'raw',filename="predict_svm.tif")
  predict_nnet <- raster::predict(object = brick_input,overwrite = TRUE,
                                  model = model_nnet, type = 'raw',filename="predict_nnet.tif")
})

pred_rf = raster(predict_rf)

writeRaster(pred_rf,"predict_rf.tif")

sync(viewRGB(brick(rst_crop_lst[1:3]), r = 3, g = 2, b = 1) +
       mapView(poly, zcol = "class", col.regions = cls_dt$hex),
     mapView(predict_rf, col.regions = cls_dt$hex), 
     mapView(predict_svm, col.regions = cls_dt$hex),
     mapView(predict_nnet, col.regions = cls_dt$hex))

dev.new(height=4, width=4)
mapView(predict_rf)

dev.new(height=4, width=4)
mapView(predict_svm)

dev.new(height=4, width=4)
mapView(predict_nnet)

plot(predict_rf)


input2 =  stack(paste0(wd,"/","testing/texture/no_date/","Heyman-10-05-22-8-1_all_layers.tif"))


chm2 = stack(paste0(wd,"/","testing/texture/no_date/","CHM_resampled.tif"))

input2 = stack(input2,chm2)

brick_input2 = brick(input2)

system.time({
  predict_rf2 <- raster::predict(object = brick_input2, overwrite = TRUE,
                                 model = model_rf, type = 'raw',filename="predict_rf2.tif")
  predict_svm2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                  model = model_svm, type = 'raw',filename="predict_svm2.tif")
  predict_nnet2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                   model = model_nnet, type = 'raw',filename="predict_nnet2.tif")
})





###########################
#############################
###############################
####################################

training = na.omit(training[,-c(1:3,42)])

# Histograms of predictors
training %>% 
  select(-"Type") %>% 
  melt(measure.vars = names(.)) %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  geom_vline(xintercept = 0, color = "gray70") +
  facet_wrap(facets = vars(variable), ncol = 3)

#Split into train and test
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(training$Type,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)
dt_train <- training[idx_train,]
dt_test <- training[-idx_train,]

table(dt_train$Type)

#Fit models

n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

#random forest

# Register a doParallel cluster, using 3/4 (75%) of total CPU-s

library(Rmpi)

cl <- makeCluster(3/4 * detectCores())

registerDoParallel(cl)

model_rf <- caret::train(Type ~ . , method = "rf", data = dt_train,
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)
stopCluster(cl); remove(cl)
# Unregister the doParallel cluster so that we can use sequential operations
# if needed; details at https://stackoverflow.com/a/25110203/5193830
registerDoSEQ()
saveRDS(model_rf, file = "model_rf_no_h.rds")

#performance
model_rf$times$everything

plot(model_rf)

#confusion metrix

cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         as.factor(dt_test$Type))
cm_rf


model_rf$finalModel

# Predictor importance

caret::varImp(model_rf)$importance %>%
  as.matrix %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)


randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

randomForest::varImpPlot(model_rf$finalModel)

#############SVM############################################


# Grid of tuning parameters
svm_grid <- expand.grid(cost = c(0.2, 0.5, 1),
                        Loss = c("L1", "L2"))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_svm <- caret::train(Type ~ . , method = "svmLinear3", data = dt_train,
                          allowParallel = TRUE,
                          tuneGrid = svm_grid,
                          trControl = ctrl)


# stopCluster(cl); remove(cl)
registerDoSEQ()
# Warning message:
# In train.default(x, y, weights = w, ...) :
#   Class probabilities were requested for a model that does not implement them
# (see why above)
saveRDS(model_svm, file = "model_svm_no_h.rds")


# Model summary & confusion matrix

model_svm$times$everything # total computation time

plot(model_svm) # tuning results

# The confusion matrix using the test dataset
cm_svm <- confusionMatrix(data = predict(model_svm, newdata = dt_test),
                          as.factor(dt_test$Type))
cm_svm



##################Neural Network#####################################
# Grid of tuning parameters
nnet_grid <- expand.grid(size = c(5, 10, 15),
                         decay = c(0.001, 0.01, 0.1))

# cl <- makeCluster(3/4 * detectCores())
# registerDoParallel(cl)
model_nnet <- train(Type ~ ., method = 'nnet', data = dt_train,
                    importance = TRUE,
                    maxit = 1000, # set high enough so to be sure that it converges
                    allowParallel = TRUE,
                    tuneGrid = nnet_grid,
                    trControl = ctrl)


# stopCluster(cl); remove(cl)


registerDoSEQ()
saveRDS(model_nnet, file = "model_nnet_no_h.rds")


# Model summary & confusion matrix

model_nnet$times$everything # total computation time

plot(model_nnet) # tuning results


# The confusion matrix using the test dataset
cm_nnet <- confusionMatrix(data = predict(model_nnet, newdata = dt_test),
                           as.factor(dt_test$Type))
cm_nnet

cols <- grDevices::colorRampPalette(colors = brewer.pal(n = 9, name = "YlGnBu"))(10)

dev.new(height=4, width=4)
garson(model_nnet) +
  scale_y_continuous('Rel. Importance') + 
  scale_fill_gradientn(colours = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.new(height=4, width=4)
cols_rank_import <- cols[rank(garson(model_nnet, bar_plot = FALSE)$rel_imp)]
plotnet(model_nnet, circle_col = list(cols_rank_import, 'lightblue'))



###Compare models###################

# Create model_list
model_list <- list(rf = model_rf, svm = model_svm, nnet = model_nnet)
# Pass model_list to resamples()
resamples <- caret::resamples(model_list)

# All metrics with boxplots
bwplot(resamples)


#paired t test to compare model performances
t_tests <- resamples %>%
  diff(metric = "Accuracy") %>%
  summary
t_tests


###Visualize classifications############



brick_input = brick(input)


system.time({
  predict_rf <- raster::predict(object = brick_input, overwrite = TRUE,
                                model = model_rf, type = 'raw',filename="predict_rf_no_h.tif")
  predict_svm <- raster::predict(object = brick_input,overwrite = TRUE,
                                 model = model_svm, type = 'raw',filename="predict_svm_no_h.tif")
  predict_nnet <- raster::predict(object = brick_input,overwrite = TRUE,
                                  model = model_nnet, type = 'raw',filename="predict_nnet_no_h.tif")
})

# pred_rf = raster(predict_rf)
# 
# writeRaster(pred_rf,"predict_rf.tif")

sync(viewRGB(brick(rst_crop_lst[1:3]), r = 3, g = 2, b = 1) +
       mapView(poly, zcol = "class", col.regions = cls_dt$hex),
     mapView(predict_rf, col.regions = cls_dt$hex), 
     mapView(predict_svm, col.regions = cls_dt$hex),
     mapView(predict_nnet, col.regions = cls_dt$hex))

dev.new(height=4, width=4)
mapView(predict_rf)

dev.new(height=4, width=4)
mapView(predict_svm)

dev.new(height=4, width=4)
mapView(predict_nnet)

plot(predict_rf)


input2 =  stack(paste0(wd,"/","testing/texture/no_date/","Heyman-10-05-22-8-1_all_layers.tif"))


chm2 = stack(paste0(wd,"/","testing/texture/no_date/","CHM_resampled.tif"))

input2 = stack(input2,chm2)

brick_input2 = brick(input2)

system.time({
  predict_rf2 <- raster::predict(object = brick_input2, overwrite = TRUE,
                                 model = model_rf, type = 'raw',filename="predict_rf2_no_h.tif")
  predict_svm2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                  model = model_svm, type = 'raw',filename="predict_svm2_no_h.tif")
  predict_nnet2 <- raster::predict(object = brick_input2,overwrite = TRUE,
                                   model = model_nnet, type = 'raw',filename="predict_nnet2_no_h.tif")
})


















mapView(predict_rf2)

writeRaster(predict_nnet,"predict_nnet.tif",overwrite=TRUE)



# Prepare colors for each class.

training$id <- as.integer(factor(training$Type))


cls_dt <- unique(training$Type) %>% 
  arrange(training$id) %>% 
  mutate(hex = c(Dead_wood  = "#ff7f00",
                 Ground = "#e41a1c",
                 Herbs       = "#4daf4a",
                 Live_trees      = "#984ea3",
                 Rocks        = "#377eb8",
                 Seedling = "#F8766D",
                 Shadows = "#C77CFF",
                 Shrubs = "#00BFC4"))


#########################################################

library(terra)
library(ggplot2)
library(sf)


wd <- "/data-store/iplant/home/shared/earthlab/nfs_career/data/SUMMER_2023/Hayman-HAY6-09_06_2023/Hayman6_new/variables/predictions"

file_list2 = dir(file.path(wd), pattern = ".shp", full.names = FALSE, ignore.case = TRUE) 
names_to_cloud = substr(file_list2, 0, nchar(file_list2)-4) # truncate the file names.



# Load RGB raster and convert to dataframe
r <- terra::rast("/data-store/iplant/home/shared/earthlab/nfs_career/data/SUMMER_2023/Hayman-HAY6-09_06_2023/metashape_outputs/Hayman-HAY6-09_06_2023_ORTHO.tif")



# Load polygons (must have 'class' column)
polygons <- st_read(file.path(wd,paste0(names_to_cloud[5],".shp")))

# Ensure CRS match
polygons <- st_transform(polygons, crs(r))


#crop the digital surface model (a raster file that contains surface elevation data)
poly_vect <- vect(polygons) 

poly_rast = crop(r,ext(poly_vect))

# Plot RGB raster
plotRGB(poly_rast, r = 1, g = 2, b = 3, stretch = "lin")

# Convert prediction to a factor if it's not
polygons$prediction <- as.factor(polygons$prediction)

palette <- c("Evergreen" = "darkgreen",
             "Seedlings" = "lightgreen",
             "Shrub" = "brown",
             "Dead" = "grey")

polygon_colors <- palette[as.character(polygons$prediction)]

# Plot polygons colored by class
plot(st_geometry(polygons), col = polygon_colors, add = TRUE)

# Add legend
legend("topright", legend = levels(polygons$prediction), fill = palette, cex = 0.8)



# # Convert raster to data frame for ggplot
# poly_rast_df <- as.data.frame(poly_rast, xy = TRUE)
# colnames(poly_rast_df)[3:5] <- c("R", "G", "B")  # Rename if needed
# 
# # Normalize to 0–1 if needed (assumes original values range from 0–255)
# poly_rast_df$R <- poly_rast_df$R / 255
# poly_rast_df$G <- poly_rast_df$G / 255
# poly_rast_df$B <- poly_rast_df$B / 255
# 
# 
# poly_rast_df$hex <- rgb(poly_rast_df$R, poly_rast_df$G, poly_rast_df$B)
# 
# #install.packages("ggnewscale")
# library(ggnewscale)
# 
# 
# ggplot() +
#   geom_raster(data = poly_rast_df, aes(x = x, y = y, fill = hex)) +
#   scale_fill_identity() +
#   new_scale_fill() +  # from ggnewscale package
#   geom_sf(data = polygons, aes(fill = prediction), color = "black", alpha = 0.5) +
#   scale_fill_manual(
#     values = c("Shrub" = "sienna", 
#                "Seedlings" = "lightgreen", 
#                "Evergreen" = "darkgreen", 
#                "Dead" = "grey50")
#   ) +
#   theme_minimal() +
#   coord_sf() +
#   labs(title = "Polygons Colored by Class on RGB Raster") +
#   theme(legend.position = "right")
# 
# ############################################################

# poly <- rgdal::readOGR(dsn   = "./data/train_polys", 
#                        layer = "train_polys", 
#                        stringsAsFactors = FALSE)
# # Need to have a numeric id for each class - helps with rasterization later on.
# poly@data$id <- as.integer(factor(poly@data$class))
# setDT(poly@data)


# #setwd("C:/data/")   # set the working directory to the directory where the data is stored
# randomForest.classify("all_with_chm_test.tif",    # file name of the unclassified image
#                       "Training_all_classes.shp",  # point shape file of the locations of the training samples
#                       "Type",           # the column in the training samples shape file which contain the class names
#                       "classifiedImage.tif"   # file name of the classified image
# )