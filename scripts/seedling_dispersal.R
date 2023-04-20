
wd = "D:/Carbon_dynamics/UAS-processing/Schoonover-9-6-2022/testing/texture/Sch/"
setwd(wd)


model_rf = read_rds("D:/Carbon_dynamics/UAS-processing/Hm_box1/training/model_rf.rds")

input = stack("choonover-9-6-2022-prj-0-3_all_layers.tif")
chm = stack("chm_re_0_3.tif")

input = stack(input,chm)
brick_input = brick(input)


system.time({
  predict_rf <- raster::predict(object = brick_input, overwrite = TRUE,
                                model = model_rf, type = 'raw',filename="predict_rf_0_3.tif")
})

predrf = predict_rf

predrf[predrf !=6] <- NA

plot(predrf)

library(jpeg)

#writeRaster(predrf,"seedling_only_8-25-22-6-9.tif")


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
library(MASS)

# 
# wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/"
# 
# setwd(wd)

# texture_file = "hm_box1_8-25-2022-6-9VIs.tif"

polygon_file = "choonover-9-6-2022-prj-0-3_crowns_cropped2.gpkg"

#input_VI = stack(file.path(paste0(wd,"/","VI/Hm_box1/08-25-2022/",texture_file)))

polygon = st_read(file.path(paste0("D:/Carbon_dynamics/UAS-processing/Schoonover-9-6-2022/data/Schoonover/09-06-2022/",polygon_file)))

test2 = polygon

#input_resamp = raster::focal(input, w = matrix(1,3,3), mean)

#input[raster::getValues(input)<0] <- 0

# 
# test2=polygon
# 
# test2 = polygon[c(383:400),]


  
#data_frame3 = data.frame()

#extract raster cell count (sum) within each polygon area (poly)
ex <- raster::extract(predrf, polygon, fun=sum, na.rm=TRUE, df=TRUE)
df <- data.frame(ex)


polygon = cbind(polygon,df)

polygon$pixel_area = (polygon$layer/6)*res(predrf)[1]*res(predrf)[2] # has to devide by 6 (class_ID for seedling) as above extraction get the sum of all pixels. which means sum of all pixel values.

polygon$poly_area = st_area(polygon)
polygon$percent = polygon$pixel_area*100/polygon$poly_area

library(units)

polygon = drop_units(polygon)

select_seeds = polygon[polygon$percent>=75,]

#write.csv(polygon[,-c(9)], "test_trees_seedlings.csv")

my_sf <- st_as_sf(select_seeds)



st_write(my_sf, dsn = '.', layer = 'seedling_only_schoonover-9-6-2022-prj-0-3.shp', driver = "ESRI Shapefile", overwrite = TRUE)

points <- st_sample(my_sf, size=length(my_sf$poly_area))

# plot(predrf)
# 
# plot(select_seeds, add = TRUE)


xy = st_coordinates(points)

