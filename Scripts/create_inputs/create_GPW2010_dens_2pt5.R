# R version 3.6.3 (2020-02-29)
# author: Alfonso Martínez Arranz
# last header generated at: 2020-04-15 08:31:24
# Extracting India values from world population density map
# using AC map and carrying out some (very expensive) calculations

library(raster)
library(cptools)

# ac_map_type has to be "pre" or "post" delimitation.

# Import basic AC map----------------------------------------------------------
a_AC_map <- st_read(paste0("Processed/03_admin_maps/clean", ac_map_type, "acmap.gpkg")) %>% 
  dplyr::select(ST_NAME, DIST_NAME, AC_NO)

# World map loading
dens_2pt5min_2010 <- raster(
  paste0(
    "Original/NASA-GPW/",
    "gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals",
    "_rev10_2010_2pt5_min.tif"
  )
)

# Reprojects AC_map in the same reference as the raster map
AC_reproject <- a_AC_map %>% 
  st_transform(projection(dens_2pt5min_2010))

# Selecting a rectangle containing only India 
dens_masked <- dens_2pt5min_2010 %>% 
  # Removes parts of map not in AC_reproject 
  crop(as(AC_reproject, "Spatial")) %>% 
  # Sets all parts outside the borders of AC_reproject to NA
  mask(as(AC_reproject, "Spatial")) 

# This is a very expensive calculation!!!
GPW2010_dens_2pt5 <- rasterToPolygons(dens_masked, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  rename(Density = contains("density"))

write_sf(GPW2010_dens_2pt5, paste0("Processed/03_pop_maps/GPW2010_dens_2pt5_", 
                                   ac_map_type, ".gpkg"),
         delete_layer = TRUE)

detach(package:raster)
detach(package:sp)
