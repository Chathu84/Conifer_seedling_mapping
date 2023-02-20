set.seed(123)
library(corrplot)
library(ggplot2)
library(RColorBrewer)

library(GGally)
library(caTools)
#library(randomForest)
library(caret)
library(varSelRF)
library(leaps)
library(MASS)
library(Metrics)
library(mFD)
library(lidR)
library(gstat)
library(terra)
library(raster)
library(rgdal)

wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training/"

setwd(wd)

lidar_file= file.path(paste0(wd,"/","hm_box1_8-25-2022-6-9.laz"))

point_cld = lidR::readLAS(lidar_file)
  
polygon_file = polygon = st_read(file.path(paste0(wd,"/",polygon_file)))

file.path <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar <- readLAS(file.path)

library(rgdal)
site_spdf <- readOGR(dsn = "...lidR\\extdata", layer = "lake_polygons_UTM17")
site_p <- site_spdf@polygons[[1]]@Polygons[[1]] #sp object of class Polygon

clipped_las = lasclip(lidar, site_p)