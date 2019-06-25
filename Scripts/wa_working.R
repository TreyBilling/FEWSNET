library(sf)
library(fasterize)
library(raster)
library(tidyverse)
here::set_here()
source(here::here("Scripts", "funs.R"))

# Get adm shapefiles ------------------------------------------------------

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
wa_csgrid <- map(wa_cs, to_raster) %>% 
  map(., raster_to_sf)

# Drop in the date (yearmonth)
for(i in 1:length(wa_csgrid)) {
  wa_csgrid[[i]]$date_code <- substr(wa_names[i], 4, 9)
  
}

# Match to adm units

finder <- function(data, shp) {
  data %>% 
    map(~st_join(.x, shp, join = st_intersects))
}

wa_csgridt <- finder(wa_csgrid, joined1)


ggplot(data = wa_csgridt[[20]]) +
  geom_sf(aes(fill = NAME_0)) +
  theme_bw() +
  scale_fill_viridis_d()
  
  
  


