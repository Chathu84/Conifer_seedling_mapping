
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



wd = "E:/Twensday-03/LCMAP/"

setwd(wd)

texture_file = "LCMAP_011011_1986_2021_V13_SCLAST.tif"

texture_file2 = "LCMAP_011011_1986_2021_V13_LCACHG.tif"

polygon_file = "MTBS_fires_within_LCMAP11-11_wgs.shp"

input = stack(file.path(paste0(wd,texture_file)))

polygon = st_read(file.path(paste0(wd,"/",polygon_file)))

#input_resamp = raster::focal(input, w = matrix(1,3,3), mean)

input[raster::getValues(input)<0] <- 0

#structural_diversity =  data.frame(matrix(ncol = 18, nrow = 0))

# test=polygon
test=polygon

for (i in 1:36){
  
  data_frame2 = paste0("training_val_",i)
  print( data_frame2)
  
  data_frame2 <-
    polygon %>% mutate(
      rastMean = raster_extract(input[[i]], polygon, fun = mean, na.rm = TRUE),
      rastMax = raster_extract(input[[i]], polygon, fun = max, na.rm = TRUE),
      rastMin = raster_extract(input[[i]], polygon, fun = min, na.rm = TRUE),
      rastsd = raster_extract(input[[i]], polygon, fun = sd, na.rm = TRUE)
      
    )
  # data_frame %>%
  #   st_set_geometry(NULL) %>%
  #   knitr::kable()
  
  test = cbind(test,data_frame2$rastMean,data_frame2$rastMax,data_frame2$rastMin,data_frame2$rastsd)
  
}

write.csv(test, "variables_from_LCMAP_cor.csv", quote=TRUE)
