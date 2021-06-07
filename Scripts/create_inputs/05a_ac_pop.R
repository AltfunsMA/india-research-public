# R version 3.6.3 (2020-02-29)
# author: Alfonso Martínez Arranz
# last header generated at: 2020-04-15 16:21:32
# Adding geographical information (distance, urban extent and towns) to AC map 
# Generating the following cleaner maps to help in determining 
# Urban Rural distinction in subsequent files:
## * AC_urbextent - Urban extent
## * NASA_GRUMP_settlementAC - Settlements

library(cptools)

# Variable paths
admin_path <- "Processed/03_admin_maps/"
pop_path <- "Processed/03_pop_maps/"
out_path <- paste0("Processed/04_ac_pop/AC_geodata_", ac_map_type, ".gpkg")
ac_map_path <- paste0(admin_path, "clean_", ac_map_type, "_acmap.gpkg")

# Import basic AC map to use throughout calculations---------------------------

# The merging corrects for some inaccuracies, 4068 distinct ST_NAME and AC_NO
# instead of 4098 for the post-2008 map

acnormal <- st_read(ac_map_path, quiet = TRUE)

a_ac_only_map <- acnormal %>% 
  select(ST_NAME, AC_NO) %>% 
  group_by(ST_NAME, AC_NO) %>% 
  summarise()


a_ac_dist <- acnormal %>% 
  rm_list_cols() %>%
  select(ST_NAME, DIST_NAME, AC_NO) %>%
  group_by(ST_NAME, AC_NO) %>%
  mutate(DIST_NAME = paste0(DIST_NAME, collapse = "_")) %>%
  ungroup() %>%
  distinct()


a_AC_map <- a_ac_dist %>%
  left_join(a_ac_only_map, c("ST_NAME", "AC_NO")) %>%
  st_as_sf() %>%
  st_make_valid()


a_dupes <- duplicheck(a_AC_map)


# Urban area in each AC -------------------------------------------------------

urban_extent <- st_read(paste0("Original/NASA-GRUMP/Urban_Extent_Polygons/", 
                               "global_urban_extent_polygons_v1.01.shp"), 
                        quiet = TRUE) %>% 
  filter(Countryeng == "India") %>% 
  dplyr::select(URBID, URB_SCHNM = SCHNM, URB_NAME = NAME, 
                URB_KM2 = SQKM_FINAL) %>% 
  st_make_valid()

urbext_file <- paste0(pop_path, "GRUMP_urban_extent_cleaned.gpkg")

if(!file.exists(urbext_file)) {write_sf(urban_extent, urbext_file)}

ACmap_calc <- a_AC_map %>% 
  # post-2008 started calling it geometry
  mutate(KM2 = as.numeric(st_area(.)/1000000)) %>% 
  st_make_valid()

# Returns geometries and all attributes of urban-extent polygons
#  subdivided by ACs


AC_urbext_calc <- suppressWarnings(st_intersection(ACmap_calc, urban_extent))


AC_urbext_calc$area <- suppressWarnings(st_area(AC_urbext_calc) /1000000)

# Calculate urban area of each AC and merge with full AC geometries

AC_urbext <- AC_urbext_calc %>% 
  as_tibble() %>% 
  group_by(ST_NAME, AC_NO) %>% 
  summarise(AC_URB_KM2 = sum(area, na.rm = TRUE)) %>% 
  ungroup() %>% 
  right_join(ACmap_calc, by = c("ST_NAME", "AC_NO")) %>%
  mutate(AC_URB_KM2 = replace_na(AC_URB_KM2, 0)) %>%
  mutate_if(is.factor, as.character)
# filter(!is.na(ST_NAME))

# For (backwards) compatibility with CPACUR_cen_geo.R file
write_csv(AC_urbext, paste0(pop_path, 
                            "AC_urbext_", ac_map_type, ".csv")) 
        

## Location of large settlements -------------------------------------------


settle_sf <- 
  st_read("Original/NASA-GRUMP/Urban_settlement_points/indpv1.shp",
          quiet = TRUE) %>% 
  filter(ES00POP > 5000)

useful_settlement_vars <- settle_sf %>% 
  st_set_geometry(NULL) %>% 
  select(c("NAME1", "ES00POP", 
           "LONGITUDE", "LATITUDE"))


towns_in_acs <- a_AC_map[as.numeric(st_within(settle_sf, 
                                                   a_AC_map)),]

settlement_ac <- useful_settlement_vars %>% 
  bind_cols(towns_in_acs) %>%
  dplyr::select(
    ST_NAME,
    DIST_NAME,
    AC_NO,
    CITYNAME = NAME1,
    POPULATION = ES00POP,
    LON_CITY = LONGITUDE,
    LAT_CITY = LATITUDE
  ) %>%
  filter(!is.na(AC_NO))

rm(settle_sf, useful_settlement_vars, towns_in_acs)

write_csv(settlement_ac, 
          paste0(pop_path, "NASA_settlement_by_AC_", 
                 ac_map_type, ".csv"))

# Adding settlement data ------------------------------------------------------
b_AC_towns <- a_AC_map %>%
  st_set_geometry(NULL) %>% 
  left_join(settlement_ac, by = c("ST_NAME", "AC_NO")) %>% 
  replace_xy() %>% 
  group_by(ST_NAME, AC_NO) %>% 
  mutate(POPULATION = replace_na(POPULATION, 0),
         TOWN_POP = sum(POPULATION)) %>%
  mutate(TOWNS = paste(CITYNAME, collapse = ",")) %>% 
  select(-POPULATION, -LON_CITY, -LAT_CITY, 
         -CITYNAME) %>% 
  ungroup() %>% 
  left_join(AC_urbext, by = c("ST_NAME", "AC_NO")) %>% # Adds KM2 and AC_URB_KM2
  replace_xy() %>% 
  distinct() %>% 
  left_join(a_AC_map, c("ST_NAME", "AC_NO", "geom", "DIST_NAME")) %>% 
  st_as_sf()


b_missing <- b_AC_towns %>% 
  select(-TOWNS) %>% 
  odd_val_col(verbose = F)
  
b_dupes <- duplicheck(b_AC_towns)

# Calculate geographic distances ----------------------------------------

sfread(Geonames_settlements)

Ind_power_plants <- st_read("Original/DS_PowerPlant/Ind_power_plants.shp", 
                            quiet = TRUE)


c_bigCity <- Geonames_settlements %>%
  filter(pop > 1000000)

c_capitals <- c("Hyderabad", "Amravati", "Kolkata", "Itanagar", "Guwahati", "Patna", 
                 "Chandigarh", "Silvassa", "Mumbai", "Daman", "New Delhi", "Panaji",
                 "Gandhinagar", "Ahmedabad", "Shimla", "Dharamsala", "Srinagar", "Jammu", 
                 "Bengaluru", "Thiruvananthapuram", "Cochin", "Bhopal", "Nagpur", 
                 "Imphal", "Shillong","Aizawl", "Kohima", "Bhubaneshwar", "Puducherry", 
                 "Cuttack", "Chennai", "Jaipur", "Jodhpur", "Gangtok", "Agartala", 
                 "Lucknow", "Allahabad", "Dehra Dun", "Naini Tal", "Ranchi")

c_capitalCity <- Geonames_settlements %>% 
  filter(asciiname %in% c_capitals)

c_existPowerplants <- Ind_power_plants %>% 
  filter(commission <= wave) %>% 
  filter(capacity_m > 300)

c_newPowerplants <- Ind_power_plants %>% 
  filter(commission > wave) %>% 
  filter(capacity_m > 300)


c_distances <- b_AC_towns %>% 
  # Calculates all distances and then selects minimum
  mutate(KM_TO_BIGCITY = 
           map_dbl(as_tibble(st_distance(c_bigCity, 
                                         st_centroid(.))), 
                   min)/1000) %>%
  mutate(KM_TO_CAPITAL = 
           map_dbl(as_tibble(st_distance(c_capitalCity, 
                                         st_centroid(.))), 
                   min)/1000) %>% 
  mutate(KM_TO_PLANT = 
           map_dbl(as_tibble(st_distance(c_existPowerplants, 
                                         st_centroid(.))), 
                   min)/1000) %>% 
  mutate(KM_TO_NEWPLANT = 
           map_dbl(as_tibble(st_distance(c_newPowerplants, 
                                         st_centroid(.))), 
                   min)/1000) %>% 
  arrange(ST_NAME, AC_NO) %>% 
  fill(KM_TO_PLANT, KM_TO_NEWPLANT, KM_TO_CAPITAL, KM_TO_BIGCITY)

c_missing <- c_distances %>% 
  select(-TOWNS, -DIST_NAME) %>% 
  odd_val_col() # 0 obs

stopifnot(is.null(c_missing))

# stopifnot(c_missing$ST_NAME  %in% c("Goa", "Puducherry", "Jammu & Kashmir"))

# post-2008 no more duplicates due to 
c_duplicated <- duplicheck(c_distances) 

# gaps in small urban_areas of Gujarat and Madhya Pradesh all under same 
# districts
acno_breaks <- find_acno_incomplete(c_distances)

if(!is.null(acno_breaks)) {
  
  breaks_in_right_spots <- any(!acno_breaks$AC_NO %in% c("Madhya Pradesh", 
                                                         "Gujarat", "Sikkim")) || 
    nrow(acno_breaks) < 14
  
  stopifnot(breaks_in_right_spots)
  
}
  
  


write_sf(c_distances, out_path, delete_layer = TRUE)


