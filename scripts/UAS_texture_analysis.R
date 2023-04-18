
seed(123)

list.of.packages <- c("tidyverse","lidR","terra","raster","rgdal","ForestTools","RCSF","sp","sf","stars","rgl","glcm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(lidR)
library(terra)
library(raster)
library(rgdal)
library(ForestTools)
library(RCSF)
library(sp)
library(sf)
library(stars)
library(rgl)
library(glcm)

wd = "~/"

setwd(wd)


# file_list = dir("D:/Carbon_dynamics/UAS-processing/Hm_box1/training", pattern = "*.laz", full.names = FALSE, ignore.case = TRUE)
# names_to_cloud = substr(file_list, 1, nchar(file_list)-4)

# stringr::str_trunc(file_list, 21)

site_name <- "texture"
flight_datetime <- "high_M"

# directories to be created in this script
L2_geo_dir <- file.path("testing", site_name, flight_datetime)

# image1 = stack(paste0(wd,"/","ortho_hm_box1.tif"))
# 
if(!dir.exists(L2_geo_dir)) {
  dir.create(L2_geo_dir, recursive = TRUE)
}

RGB_file = "heyman_ortho_10-05-2022.tif"

main_rgb = stack(paste0(wd,RGB_file))

# dense_point_cloud <- lidR::readLAS("D:/Carbon_dynamics/UAS-processing/Heyman/Heyman-10-05-2022/Heyman-10-05-22-8-1.laz")
# 
# cut_rgb = crop(main_rgb,dense_point_cloud)

crop_img = main_rgb

texture1 = glcm(crop_img[[1]], n_grey = 32, window = c(9, 9), shift = c(3, 1), statistics =
                   c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
                     "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any",
                 na_val=NA, scale_factor=1, asinteger=FALSE)

texture2 = glcm(crop_img[[2]], n_grey = 32, window = c(9, 9), shift = c(3, 1), statistics =
                  c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
                    "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any",
                na_val=NA, scale_factor=1, asinteger=FALSE)

texture3 = glcm(crop_img[[3]], n_grey = 32, window = c(9, 9), shift = c(3, 1), statistics =
                  c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
                    "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any",
                na_val=NA, scale_factor=1, asinteger=FALSE)

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

stack_1 = VI(crop_img,1,2,3)

# 
# for (i in 1:length(names_to_cloud)) #length(names_to_cloud)
# {
#   print(i)
#   
#   # cropped_dense_point_cloud_fname <- file.path(paste0(file_list[i]))
#   # 
#   # print(cropped_dense_point_cloud_fname)
#   # 
#   # dense_point_cloud <- lidR::readLAS(cropped_dense_point_cloud_fname)
#   # 
#   # crop_img = crop(image1,dense_point_cloud)
#   #crop_dsm = crop(main_dsm,dense_point_cloud)
#   
#   crop_img = raster(file.path(paste0(file_list[i])))
#   
#   texture_image <- file.path("VI", site_name,flight_datetime, paste0( names_to_cloud[i],"VIs.tif"))
#   
#   #cropped_chm_fname <- file.path("data2", site_name,flight_datetime,paste0(names_to_cloud[i], "_chm2.tif"))
#   
#   
#   texture3 = glcm(crop_img[[3]], n_grey = 32, window = c(9, 9), shift = c(3, 1), statistics =
#                    c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy",
#                      "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any",
#                  na_val=NA, scale_factor=1, asinteger=FALSE)
#   
#   
#   VI = function(img,i,j,k){
#     bi = img[[i]]
#     bj = img[[j]]
#     bk = img[[k]]
#     r = bi/(bi+bj+bk)
#     g = bj/(bi+bj+bk)
#     b = bk/(bi+bj+bk)
#     EXR = 1.4*bi - bj
#     VARI = (bj-bi)/(bj+bi-bk)
#     GRVI = (bj-bi)/(bj+bi)
#     MGRVI = (bj^2-bi^2)/(bj^2+bi^2)
#     CIVE = 0.441*bi - 0.881*bj + 0.385*bk + 18.78745
#     EXG = 2*bj-bk-bi
#     GLA = (2*bj-bk-bi)/(2*bj+bk+bi)
#  
#     return(stack(bi,bj,bk,r,g,b,EXR,VARI,GRVI,MGRVI,CIVE,EXG,GLA)) #
#   }
#   
#   stack_1 = VI(cut_rgb,1,2,3)
#   
#    writeRaster(stack_1, texture_image, format="GTiff")
#   
#   
# }

# texture1 = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/testing/texture1.tif")
# texture2 = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/testing/texture2.tif")
# texture3 = stack("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/testing/texture3.tif")

all_img = stack(stack_1,texture1,texture2,texture3)

all_layers <- file.path(wd,"testing", site_name,flight_datetime, paste0( "Heyman-10-05-22-8-1_","all_layers.tif"))
writeRaster(all_img, all_layers, format="GTiff")



#####################################
