
#### Below code prepare training polygons for the one-class classification


list.of.packages <- c("sf","rnaturalearth","here","stars","dplyr","ggplot2","ggnewscale","scico","geobgu","ggrepel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#devtools::install_github("michaeldorman/geobgu",force = TRUE)


# load packages
library(sf) # Simple Features for R
library(rnaturalearth) # World Map Data from Natural Earth
library(here) # A Simpler Way to Find Your Files
library(stars) # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(scico) # Colour Palettes Based on the Scientific Colour-Maps
library(geobgu) # install from GitHub ("michaeldorman/geobgu")
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
library(devtools)
library(raster)



wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training/"

setwd(wd)

texture_file = "hm_box1_8-25-2022-6-9texture1.tif"

polygon_file = "training_seedlings.shp"

input = stack(file.path(paste0(wd,"/","texture1/Hm_box1/08-25-2022/",texture_file)))

polygon = st_read(file.path(paste0(wd,"/",polygon_file)))

#input_resamp = raster::focal(input, w = matrix(1,3,3), mean)

input[raster::getValues(input)<0] <- 0

for (i in 1:8){
  
  data_frame = paste0("training_val_",i)
  print( data_frame)
  
  data_frame <-
    polygon %>% mutate(
      rastMean = raster_extract(input[[i]], polygon, fun = mean, na.rm = TRUE),
      rastMax = raster_extract(input[[i]], polygon, fun = max, na.rm = TRUE),
      rastMin = raster_extract(input[[i]], polygon, fun = min, na.rm = TRUE)
    )
  data_frame %>%
    st_set_geometry(NULL) %>%
    knitr::kable()
  
}



training_val <-
  polygon %>% mutate(
    rastMean = raster_extract(input[[1]], polygon, fun = mean, na.rm = TRUE),
    rastMax = raster_extract(input[[1]], polygon, fun = max, na.rm = TRUE),
    rastMin = raster_extract(input[[1]], polygon, fun = min, na.rm = TRUE)
  )
training_val %>%
  st_set_geometry(NULL) %>%
  knitr::kable()





# stringr::str_trunc(file_list, 21)

site_name <- "Hm_box1"
flight_datetime <- "08-25-2022"

# directories to be created in this script
L2_geo_dir <- file.path("VI", site_name, flight_datetime)

# image1 = stack(paste0(wd,"/","ortho_hm_box1.tif"))
# 
if(!dir.exists(L2_geo_dir)) {
  dir.create(L2_geo_dir, recursive = TRUE)
}

main_rgb = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/ortho_hm_box1.tif")
dense_point_cloud <- lidR::readLAS("hm_box1_8-25-2022-6-9.laz")
cut_rgb = crop(main_rgb,dense_point_cloud)

for (i in 1:length(names_to_cloud)) #length(names_to_cloud)
{
  print(i)
  
  # cropped_dense_point_cloud_fname <- file.path(paste0(file_list[i]))
  # 
  # print(cropped_dense_point_cloud_fname)
  # 
  # dense_point_cloud <- lidR::readLAS(cropped_dense_point_cloud_fname)
  # 
  # crop_img = crop(image1,dense_point_cloud)
  #crop_dsm = crop(main_dsm,dense_point_cloud)
  
  crop_img = raster(file.path(paste0(file_list[i])))
  
  texture_image <- file.path("VI", site_name,flight_datetime, paste0( names_to_cloud[i],"VIs.tif"))
  
  #cropped_chm_fname <- file.path("data2", site_name,flight_datetime,paste0(names_to_cloud[i], "_chm2.tif"))
  
  
  # texture = glcm(crop_img[[1]], n_grey = 32, window = c(9, 9), shift = c(3, 1), statistics =
  #                  c("mean_chm", "variance_chm", "homogeneity_chm", "contrast_chm", "dissimilarity_chm", "entropy_chm",
  #                    "second_moment_chm", "correlation_chm"), min_x=NULL, max_x=NULL, na_opt="any",
  #                na_val=NA, scale_factor=1, asinteger=FALSE)
  
  
  VI = function(img,i,j,k){
    bi = img[[i]]
    bj = img[[j]]
    bk = img[[k]]
    r = bi/(bi+bj+bk)
    g = bj/(bi+bj+bk)
    b = bk/(bi+bj+bk)
    EXR = 1.4*bi - bj
    VARI = (bj-bi)/(bj+bi-bk)
    GRVI = (bj-bi)/(bj+bi)
    MGRVI = (bj^2-bi^2)/(bj^2+bi^2)
    CIVE = 0.441*bi - 0.881*bj + 0.385*bk + 18.78745
    EXG = 2*bj-bk-bi
    GLA = (2*bj-bk-bi)/(2*bj+bk+bi)
 
    return(stack(bi,bj,bk,r,g,b,EXR,VARI,GRVI,MGRVI,CIVE,EXG,GLA)) #
  }
  
  stack_6_9 = VI(cut_rgb,1,2,3)
  
   writeRaster(stack_6_9, texture_image, format="GTiff")
  
  
}

texture1 = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/texture1/Hm_box1/08-25-2022/hm_box1_8-25-2022-6-9texture1.tif")
texture2 = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/texture2/Hm_box1/08-25-2022/hm_box1_8-25-2022-6-9texture2.tif")
texture3 = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/texture3/Hm_box1/08-25-2022/hm_box1_8-25-2022-6-9texture3.tif")

all_img = stack(stack_6_9,texture1,texture2,texture3)

all_layers <- file.path("VI", site_name,flight_datetime, paste0( names_to_cloud[i],"all_layers.tif"))
writeRaster(all_img, all_layers, format="GTiff")



#####################################
