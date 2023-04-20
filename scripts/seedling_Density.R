polygon_file = "seedlings_only_hm_8-25-22-6-9.shp"

#input_VI = stack(file.path(paste0(wd,"/","VI/Hm_box1/08-25-2022/",texture_file)))

test = st_read(file.path(paste0("D:/Carbon_dynamics/UAS-processing/Hm_box1/",polygon_file)))
my_sf <- st_as_sf(test)

points <- st_sample(my_sf, size=length(my_sf$poly_area))

xy = st_coordinates(points)

de = MASS::kde2d(xy[,1], xy[,2])

dev.new(height=2, width=2)
image(de, main = "Seedling density in HighM fire tile 6-9")
