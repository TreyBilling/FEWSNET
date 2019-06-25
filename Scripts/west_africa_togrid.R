library(sf)
library(fasterize)
library(raster)
library(tidyverse)
here::set_here()
source(here::here("Scripts", "funs.R"))

# Get adm1 shapefiles ------------------------------------------------------

# All countries in Africa
afnames <- raster::ccodes() %>% 
  filter(continent == "Africa") %>% 
  select(NAME)
temp1 <- vector("list", 0)
for(i in afnames$NAME) {
  print(i)
  temp1[[i]] <- raster::getData(name = "GADM", 
                                country = i, 
                                download = T, 
                                level = 1, path = here::here("Shapefiles"))
}

# Join into sf frame
joined1 <- list(temp1, makeUniqueIDs = T) %>% 
  purrr::flatten() %>% 
  do.call(rbind, .) %>% 
  st_as_sf()



# Current status ----------------------------------------------------------

# Get file names for current status
wa_cs_files <- list.files(path = here::here("Shapefiles", 
                                            "Projections", 
                                            "West Africa", 
                                            "Holder"), 
                          pattern = "_CS.shp$", full.names = T)  

# Extract current status files
wa_cs <- lapply(wa_cs_files, read_sf)

# File names 
wa_names <- paste0(substr(wa_cs_files, 73, 81), "_", "cs")

# Clean up values
wa_cs <- wa_cs %>% map(shp_cleaner)

# Drop in the date (yearmonth)
for(i in 1:length(wa_cs)) {
  wa_cs[[i]]$date_code <- substr(wa_names[i], 4, 9)
}

for(i in 1:length(wa_cs)) {
  wa_cs[[i]] <- dplyr::select(wa_cs[[i]], CS, geometry, date_code)
}

# Rasterize and convert to sf
wa_cs_grid <- map(wa_cs, ~to_raster(.x, field = "CS")) %>% 
  map(., raster_to_sf)

# Drop in the date (yearmonth)
for(i in 1:length(wa_cs_grid)) {
  wa_cs_grid[[i]]$date_code <- substr(wa_names[i], 4, 9)
  
}

# Match to adm units
wa_cs_grid <- finder(wa_cs_grid, joined1)

# Save 
save(wa_cs_grid, file = here::here("Grids", "wa_cs_grid.Rdata"))


# ML2 ---------------------------------------------------------------------

# Get file names for current status
wa_ml2_files <- list.files(path = here::here("Shapefiles", 
                                            "Projections", 
                                            "West Africa", 
                                            "Holder"), 
                          pattern = "_ML2.shp$", full.names = T)  

# Extract current status files
wa_ml2 <- lapply(wa_ml2_files, read_sf)

# File names 
wa_ml2_names <- paste0(substr(wa_ml2_files, 73, 81), "_", "ml2")

# Clean up values
wa_ml2 <- wa_ml2 %>% map(shp_cleaner)

# Drop in the date (yearmonth)
for(i in 1:length(wa_ml2)) {
  wa_ml2[[i]]$date_code <- substr(wa_ml2_names[i], 4, 9)
}

for(i in 1:length(wa_ml2)) {
  wa_ml2[[i]] <- dplyr::select(wa_ml2[[i]], ML2, geometry, date_code)
}

# Rasterize and convert to sf
wa_ml2_grid <- map(wa_ml2, ~to_raster(.x, field = "ML2")) %>% 
  map(., raster_to_sf)

# Drop in the date (yearmonth)
for(i in 1:length(wa_ml2_grid)) {
  wa_ml2_grid[[i]]$date_code <- substr(wa_ml2_names[i], 4, 9)
  
}

# Match to adm units
wa_ml2_grid <- finder(wa_ml2_grid, joined1)

# Save 
save(wa_ml2_grid, file = here::here("Grids", "wa_ml2_grid.Rdata"))



