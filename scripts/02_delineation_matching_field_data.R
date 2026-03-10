library(sf)
library(dplyr)
library(clue)     # solve_LSAP (Hungarian algorithm)
library(ggplot2)
library(terra)

# folder containing gpkg files
dir_path <- "/home/jovyan/data-store/var_polygons"

# find all gpkg files
files <- list.files(dir_path, pattern="\\.gpkg$", full.names=TRUE)

# read and merge
v_list <- lapply(files, vect)

merged <- do.call(rbind, v_list)

# write merged file
out_file <- file.path("/home/jovyan/data-store/revision_analysis/merged_polygons.gpkg")

merged_sf <- st_as_sf(merged)

st_write(
  merged_sf,
  out_file,
  driver = "GPKG",
  delete_dsn = TRUE,   # overwrite whole file
  quiet = TRUE
)


library(sf)
library(dplyr)
library(clue)

hungarian_match_points_join <- function(pred_sf, ref_sf, dmax = 0.5,
                                        pred_id = "pred_id", ref_id = "ref_id",
                                        ref_prefix = "ref_", pred_prefix = "pred_") {
  stopifnot(inherits(pred_sf, "sf"), inherits(ref_sf, "sf"))
  stopifnot(!st_is_longlat(pred_sf), !st_is_longlat(ref_sf))
  
  nP <- nrow(pred_sf)
  nR <- nrow(ref_sf)
  
  # IDs (create if not present)
  if (!pred_id %in% names(pred_sf)) pred_sf[[pred_id]] <- seq_len(nP)
  if (!ref_id  %in% names(ref_sf))  ref_sf[[ref_id]]  <- seq_len(nR)
  
  # Distance matrix (meters)
  D <- st_distance(pred_sf, ref_sf)
  D <- as.matrix(unclass(D))
  
  eps  <- 1e-6
  bigM <- (dmax + 1) * 1e6
  
  Dcost <- D
  Dcost[Dcost > dmax] <- bigM
  unmatched_cost <- dmax + eps
  
  # Square cost matrix for Hungarian with dummies
  N <- nP + nR
  C <- matrix(0, nrow = N, ncol = N)
  
  # pred -> ref
  C[1:nP, 1:nR] <- Dcost
  
  # pred -> dummy_ref (one per pred)
  C[1:nP, (nR + 1):(nR + nP)] <- bigM
  diag(C[1:nP, (nR + 1):(nR + nP)]) <- unmatched_cost
  
  # dummy_pred (one per ref) -> ref
  C[(nP + 1):(nP + nR), 1:nR] <- bigM
  diag(C[(nP + 1):(nP + nR), 1:nR]) <- unmatched_cost
  
  # dummy_pred -> dummy_ref
  C[(nP + 1):(nP + nR), (nR + 1):(nR + nP)] <- 0
  
  assign <- solve_LSAP(C)
  assign <- as.integer(assign)
  
  # For preds
  col_for_pred <- assign[1:nP]
  matched_ref_row <- ifelse(col_for_pred <= nR, col_for_pred, NA_integer_)
  match_dist_m <- ifelse(is.na(matched_ref_row), NA_real_,
                         D[cbind(seq_len(nP), matched_ref_row)])
  
  out_pred <- pred_sf %>%
    mutate(
      matched_ref_row = matched_ref_row,
      matched_ref_id  = ifelse(is.na(matched_ref_row), NA, ref_sf[[ref_id]][matched_ref_row]),
      match_dist_m    = match_dist_m
    )
  
  # ---- JOIN ALL REF ATTRIBUTES ONTO PRED ----
  # Build ref attribute table with row index (so we can join by matched_ref_row)
  ref_attr <- ref_sf %>%
    mutate(matched_ref_row = seq_len(nR)) %>%
    st_drop_geometry()
  
  # Remove geometry column already dropped; also avoid duplicating ref_id name collisions
  # Prefix all ref columns except the join key 'matched_ref_row'
  ref_attr_renamed <- ref_attr %>%
    rename_with(~ ifelse(.x == "matched_ref_row", .x, paste0(ref_prefix, .x)))
  
  # left join: unmatched preds keep NA in all ref_* fields
  pred_joined <- out_pred %>%
    left_join(ref_attr_renamed, by = "matched_ref_row")
  
  # ---- OPTIONAL: also join pred attrs onto ref (reverse) ----
  pred_rows_that_chose_ref <- match(seq_len(nR), col_for_pred)  # pred row index or NA
  
  out_ref <- ref_sf %>%
    mutate(
      matched_pred_row = pred_rows_that_chose_ref,
      matched_pred_id  = ifelse(is.na(pred_rows_that_chose_ref), NA, pred_sf[[pred_id]][pred_rows_that_chose_ref]),
      match_dist_m     = ifelse(is.na(pred_rows_that_chose_ref), NA,
                                D[cbind(pred_rows_that_chose_ref, seq_len(nR))])
    )
  
  pred_attr <- pred_sf %>%
    mutate(matched_pred_row = seq_len(nP)) %>%
    st_drop_geometry()
  
  pred_attr_renamed <- pred_attr %>%
    rename_with(~ ifelse(.x == "matched_pred_row", .x, paste0(pred_prefix, .x)))
  
  ref_joined <- out_ref %>%
    left_join(pred_attr_renamed, by = "matched_pred_row")
  
  list(pred = pred_joined, ref = ref_joined, D = D)
}

library(sf)
library(dplyr)

pred <- st_read("/home/jovyan/data-store/classification_data/merged_polygons.gpkg") %>%
  st_transform(32613)

ref  <- st_read("/data-store/iplant/home/nilangakoon/Fire_recovery/seedling_detection_data/spatial_data/field_polygons.geojson") %>%
  st_transform(32613)

pred_pt <- st_centroid(pred)
ref_pt  <- st_centroid(ref)

res <- hungarian_match_points_join(pred_pt, ref_pt, dmax = 2,
                                   ref_prefix = "ref_", pred_prefix = "pred_")

matched_pred <- res$pred
matched_ref  <- res$ref

# GeoJSON is okay for small-ish outputs, but GPKG is usually better for many fields
st_write(matched_pred,
         "/home/jovyan/data-store/classification_data/delineated_with_ref_join_hungarian.gpkg",
         delete_dsn = TRUE)

# If you specifically want GeoJSON:
st_write(matched_pred,
         "/home/jovyan/data-store/classification_data/delineated_with_ref_join_hungarian.geojson",
         delete_dsn = TRUE)


# True positives
TP <- sum(!is.na(matched_pred$matched_ref_id))

# False positives
FP <- sum(is.na(matched_pred$matched_ref_id))

# False negatives
FN <- sum(is.na(matched_ref$matched_pred_id))


precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * precision * recall / (precision + recall)

metrics <- data.frame(
  TP = TP,
  FP = FP,
  FN = FN,
  precision = precision,
  recall = recall,
  f1_score = f1_score
)

print(metrics)


distances <- matched_pred$match_dist_m
distances <- distances[!is.na(distances)]

distance_stats <- data.frame(
  mean = mean(distances),
  median = median(distances),
  rmse = sqrt(mean(distances^2)),
  p90 = quantile(distances, 0.9),
  max = max(distances)
)

print(distance_stats)

library(ggplot2)

ggplot(matched_pred, aes(match_dist_m)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0.5, color="red") +
  theme_bw() +
  labs(x="Match distance (m)", y="Count")


thresholds <- seq(0.1, 2, by = 0.1)

results <- lapply(thresholds, function(t){
  
  res <- hungarian_match_points_join(pred_pt, ref_pt, dmax = t)
  
  mp <- res$pred
  mr <- res$ref
  
  TP <- sum(!is.na(mp$matched_ref_id))
  FP <- sum(is.na(mp$matched_ref_id))
  FN <- sum(is.na(mr$matched_pred_id))
  
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  f1 <- 2*precision*recall/(precision+recall)
  
  data.frame(threshold=t, precision=precision, recall=recall, f1=f1)
})

curve_df <- do.call(rbind, results)

ggplot(curve_df, aes(threshold, f1)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x="Distance threshold (m)", y="F1 score")


tp_data <- matched_pred %>%
  dplyr::filter(!is.na(matched_ref_id))

st_write(
  tp_data,
  "/home/jovyan/data-store/classification_data/true_positive_matches.gpkg",
  delete_dsn = TRUE
)