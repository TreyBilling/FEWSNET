

# Clean fewsnet values ----------------------------------------------------

shp_cleaner <- function(shp) {
  
  if("CS" %in% colnames(shp) == TRUE)
    shp %>% mutate(CS = ifelse(CS > 5, NA, CS),
                   CS = ifelse(CS == 0, NA, CS))
  
  else(
    if("ML1" %in% colnames(shp) == TRUE)
      shp %>% mutate(ML1 = ifelse(ML1 > 5, NA, ML1),
                     ML1 = ifelse(ML1 == 0, NA, ML1))
    else(  
      if("ML2" %in% colnames(shp) == TRUE)
        shp %>% mutate(ML2 = ifelse(ML2 > 5, NA, ML2),
                       ML2 = ifelse(ML2 == 0, NA, ML2))))
  
  
}

# Rasterize ---------------------------------------------------------------

to_raster <- function(x, field, resolution = 0.5, fun = "sum") {
  
  r <- raster(extent(x),
              resolution = resolution,
              crs = st_crs(x)$proj4string)
  
  fasterize(x, r, field = field, fun = fun)
  
}


raster_to_sf <- function(x) {
  library(sp)
  
  as(x, "SpatialPolygonsDataFrame") %>% 
    st_as_sf()
  
}





# Match units -------------------------------------------------------------

finder <- function(data, shp) {
  data %>% 
    map(~st_join(.x, shp, join = st_intersects))
}

