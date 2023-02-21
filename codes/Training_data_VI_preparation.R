
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

texture_file = "hm_box1_8-25-2022-6-9VIs.tif"

polygon_file = "training_seedlings.shp"

input_VI = stack(file.path(paste0(wd,"/","VI/Hm_box1/08-25-2022/",texture_file)))

polygon = st_read(file.path(paste0(wd,"/",polygon_file)))

test2 = polygon

#input_resamp = raster::focal(input, w = matrix(1,3,3), mean)

#input[raster::getValues(input)<0] <- 0


test2=polygon

for (i in 2:13){
  
  data_frame3 = paste0("training_val_",i)
  print( data_frame3)
  
  data_frame3 <-
    polygon %>% mutate(
      rastMean = raster_extract(input_VI[[i]], polygon, fun = mean, na.rm = TRUE),
      rastMax = raster_extract(input_VI[[i]], polygon, fun = max, na.rm = TRUE),
      rastMin = raster_extract(input_VI[[i]], polygon, fun = min, na.rm = TRUE)
      
    )
  # data_frame %>%
  #   st_set_geometry(NULL) %>%
  #   knitr::kable()
  
  test2 = cbind(test2,data_frame3$rastMean,data_frame3$rastMax,data_frame3$rastMin)
  
}

write.csv(test2, "variables_from_VIs.csv")




#####################################
