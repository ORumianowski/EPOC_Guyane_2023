source("001_utils_packages.R")


library("raster")
library("rgeos")
library("dismo")

guyane =  st_read("limites_guyane500.shp",
               ) 

guyane = guyane %>% 
  st_transform(., 2971)

tm_shape(guyane) +
  tm_fill() +
  tm_borders() 



survey.area_buf = st_buffer(guyane, dist = 1000 * 1)

survey.area_buf %>% 
  tm_shape() +
  tm_fill()
  

j <- st_make_grid(survey.area_buf, cellsize = 2000,what="centers") # grid de point espace de 2km
j1 <- st_intersection(j, survey.area_buf) # intersection w/ guyane

plot(j1)
