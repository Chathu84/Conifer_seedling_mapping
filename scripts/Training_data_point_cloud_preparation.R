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
library(sf)

wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training/"

setwd(wd)

lidar_file= file.path(paste0(wd,"/","hm_box1_8-25-2022-6-9.laz"))

point_cld = lidR::readLAS(lidar_file)
plot(point_cld)
  
polygon_file = "training_seedlings.shp"

polygon = st_read(file.path(paste0(wd,"/",polygon_file)))


ttops = st_read(file.path(paste0(wd,"/",polygon_file)))

site_spdf <- readOGR(dsn = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training/training_seedlings.shp", layer = "training_seedlings")
site_p <- site_spdf@polygons[[1]]@Polygons[[1]] #sp object of class Polygon

clipped_las = clip_roi(point_cld, site_p)



# create empty data frame
point_metrics =  data.frame(matrix(ncol = 9, nrow = 0))

#length(site_spdf$fid_1)

for (i in 1:4){
  print(i)
  
  site_p <- site_spdf@polygons[[i]]@Polygons[[1]]
  
  clipped_las = clip_roi(point_cld, site_p)
  
  plot(clipped_las)
  rgl.snapshot('3dplot.png', fmt = 'png')
  point_density = clipped_las@header$`Number of point records`
  
  #HEIGHT SD
  #height SD, the standard deviation of height values for all points
  #in the plot point cloud
  # vert.sd <- cloud_metrics(clipped_las, sd(Z, na.rm = TRUE)) 
  # 
  # #SD of VERTICAL SD of HEIGHT
  # #rasterize plot point cloud and calculate the standard deviation 
  # #of height values at a resolution of 1 m^2
  # sd.1m2 <- grid_metrics(clipped_las, sd(Z), 1)
  # #standard deviation of the calculated standard deviations 
  # #from previous line 
  # #This is a measure of internal and external canopy complexity
  # sd.sd <- sd(sd.1m2[,3], na.rm = TRUE) 
  # 
  # 
  # #some of the next few functions won't handle NAs, so we need 
  # #to filter these out of a vector of Z points
  # Zs <- clipped_las@data$Z
  # Zs <- Zs[!is.na(Zs)]
  # 
  # #ENTROPY 
  # #entropy, quantifies diversity & evenness of point cloud heights 
  # #by = 1 partitions point cloud in 1 m tall horizontal slices 
  # #ranges from 0-1, with 1 being more evenly distributed points 
  # #across the 1 m tall slices 
  # entro <- entropy(Zs, by = 1) 
  # 
  # #GAP FRACTION PROFILE 
  # #gap fraction profile, assesses the distribution of gaps in the 
  # #canopy volume 
  # #dz = 1 partitions point cloud in 1 m horizontal slices 
  # #z0 is set to a reasonable height based on the age and height of 
  # #the study sites 
  # gap_frac <- gap_fraction_profile(Zs, dz = 1, z0=3) 
  # #defines gap fraction profile as the average gap fraction in each 
  # #1 m horizontal slice assessed in the previous line
  # GFP.AOP <- mean(gap_frac$gf) 
  # 
  # #VAI
  # #leaf area density, assesses leaf area in the canopy volume 
  # #k = 0.5 is a standard extinction coefficient for foliage 
  # #dz = 1 partitions point cloud in 1 m horizontal slices 
  # #z0 is set to the same height as gap fraction profile above
  # LADen<-LAD(Zs, dz = 1, k=0.5, z0=3) 
  # #vegetation area index, sum of leaf area density values for 
  # #all horizontal slices assessed in previous line
  # VAI.AOP <- sum(LADen$lad, na.rm=TRUE) 
  # 
  # point_metrics <- rbind(point_metrics,data.frame(matrix(c(as.numeric(i), point_density, site_spdf$tree_heigh[i], site_spdf$tree_dbh[i], vert.sd, 
  #                                                                        sd.sd, entro,GFP.AOP, VAI.AOP),
  #                                                                      ncol = 9)) )
  # 
}




colnames(point_metrics) <- 
  c("tree_id","point_density", "tree_height_field", "tree_dbh_field", "vert_sd", "sd_raster_1m",
    "entropy", "GFP", "VAI")

write.csv(point_metrics, "point_metrics.csv")

