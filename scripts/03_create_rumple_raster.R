library(terra)

# ---- inputs ----
chm_path <- "/data-store/iplant/home/nilangakoon/Fire_recovery/seedling_detection_data/plot_data/sch4/sch4_orig_chm_base.tif"                 # your CHM raster
out_path <- "/home/jovyan/data-store/rumple/sch4_CHM_rumple_3x3.tif"      # output rumple raster

# ---- read ----
chm <- rast(chm_path)

# IMPORTANT: distances must be in meters for "slope" to be correct
if (isTRUE(is.lonlat(chm))) {
  stop("Your CHM is in lon/lat (degrees). Reproject to a projected CRS in meters (e.g., UTM) before computing rumple.")
}

# ---- slope (radians) ----
# slope from terrain is based on elevation gradients and cell resolution
slope <- terrain(chm, v = "slope", unit = "radians", neighbors = 8)

# ---- per-cell roughness factor: 1 / cos(slope) ----
# This is the (3D area / planar area) per cell.
factor <- 1 / cos(slope)

# Handle any numeric weirdness (near-vertical slopes are not expected for CHM, but be safe)
factor[!is.finite(factor)] <- NA

# Optional: if CHM has NA, enforce NA in factor too
factor <- mask(factor, chm)

# ---- 3x3 rumple = mean(factor) within moving window ----
w <- matrix(1, 3, 3)

rumple_3x3 <- focal(
  factor,
  w = w,
  fun = mean,
  na.rm = TRUE,
  fillvalue = NA
)

names(rumple_3x3) <- "rumple_3x3"

# ---- save ----
writeRaster(rumple_3x3, out_path, overwrite = TRUE)

rumple_3x3