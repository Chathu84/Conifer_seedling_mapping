# Load necessary libraries
library(raster)
library(terra)
library(sf)
library(dplyr)
library(exactextractr)
library(future.apply)

wd = "/data-store/iplant/home/nilangakoon/Fire_recovery/new_var_smoothed_chm"

setwd(wd)
site_name <- "sch4"
# Read the polygon dataset (ensure it is in sf format)
polygon_data <- st_read(file.path(wd, paste0(site_name,"_var_polygons.shp")))

polygon_data <- polygon_data[,-c(24:213)]

# raster_All
# [1] "_hm1_all_layers1.tif" "_hm2_all_layers1.tif" "_hm3_all_layers1.tif" "_hm4_all_layers1.tif" "big1_all_layers1.tif" "big2_all_layers1.tif"
# [7] "hay1_all_layers1.tif" "sch1_all_layers1.tif" "sch2_all_layers1.tif" "sch3_all_layers1.tif" "sch4_all_layers1.tif"
# 

chm <- terra::rast(paste0("/data-store/iplant/home/nilangakoon/Fire_recovery/seedling_detection_data/plot_data/",site_name,"/",site_name,"_orig_chm_base.tif"))#terra::rast(file.path(wd, "clipped_data","chm_dir",paste0(names_to_cloud[r],"_all_chm.tif")))


rumple <- terra::rast(paste0("/home/jovyan/data-store/rumple/",site_name,"_CHM_rumple_3x3.tif"))

raster_file = terra::rast(paste0("/data-store/iplant/home/nilangakoon/Fire_recovery/seedling_detection_data/all_layers/", site_name,"_all_layers1.tif"))# terra::rast(file.path(wd, "clipped_data","all_layers",paste0(names_to_cloud[r],"_all_layers1.tif")))


raster_file <- c(raster_file, chm, rumple)


names(raster_file) <- c('R', 'G', 'B',  'RI', 'GI', 'BI', 'EXR', 'VARI',  'GRVI', 'MGRVI',
                      'CIVE', 'EXG', 'GLA','R_mean', 'R_variance', 'R_homogeneity','R_contrast', 'R_dissimilarity', 'R_entropy','R_second_moment',
                      'R_correlation', 'G_mean', 'G_variance', 'G_homogeneity','G_contrast', 'G_dissimilarity', 'G_entropy','G_second_moment',
                      'G_correlation','B_mean', 'B_variance', 'B_homogeneity','B_contrast', 'B_dissimilarity', 'B_entropy','B_second_moment',
                      'B_correlation','chm','rumple')

# exactextractr uses sf polygons
poly_sf <- st_as_sf(polygon_data)

# Use multicore (Linux); on some systems use multisession
future::plan(future::multicore, workers = 64)

# Crop raster first to reduce I/O
r_sub <- crop(raster_file, vect(poly_sf))

stats <- exact_extract(
  r_sub,
  poly_sf,
  fun = c("mean", "min", "max", "median", "stdev"),
  progress = FALSE
)

# variance
stats <- dplyr::mutate(stats, across(ends_with("_stdev"), ~ .x^2, .names = "{.col}_variance"))

# bind + write to GPKG
poly_out <- cbind(poly_sf, stats)

st_write(poly_out, paste0("/home/jovyan/data-store/revision_analysis/var_polygons/",site_name,"_polygon_stats.gpkg"), delete_dsn = TRUE)



