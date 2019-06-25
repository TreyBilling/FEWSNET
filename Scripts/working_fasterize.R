# This script attempts to convert a FEWS NET shapefile to a raster
# Will use as the basis to automate the full set of shapefiles in future


library(sf)
library(fasterize)
library(raster)
library(tidyverse)
here::set_here()
source(here::here("Scripts", "funs.R"))


# Read in fews net shapefiles ---------------------------------------------

# Get file names for current status
wa_cs_files <- list.files(path = here::here("Shapefiles", 
                                   "Projections", 
                                   "West Africa", 
                                   "Holder", 
                                   "west-africa200907"), 
                             pattern = "_CS.shp$", full.names = T)  

# Extract current status files
wa_cs <- lapply(wa_cs_files, read_sf)



wa_cs <- wa_cs %>% map(shp_cleaner)

plot(wa_cs[[1]])



# Shape to raster ---------------------------------------------------------

# Create raster with base parameters
r <- raster()
r
# Match geometry to shapefile
r <- raster(extent(wa_cs[[1]]),
            resolution = 0.5,
            crs = st_crs(wa_cs[[1]])$proj4string)

# Add some values to see what we're working with
values(r) <- 1:ncell(r)
out <- fasterize(wa_cs[[1]], r, field = "CS", fun = "first")
plot(out)


t_poly <- rasterToPolygons(out) %>% 
  st_as_sf()

ggplot(data = t_poly) +
  geom_sf(aes(fill = factor(layer)), 
          color = "transparent", size = 0.10) +
  scale_fill_viridis_d(option = "inferno", 
                       guide = guide_legend(title = "Current status (CS)"),
                       end = 0.95) +
  theme_bw() +
  theme(legend.position = "top")
