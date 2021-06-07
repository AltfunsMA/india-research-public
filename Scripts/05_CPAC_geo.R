# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

# Adding geographical information (distance, urban extent and towns) to AC map 
# Generating the following cleaner maps to help in determining 
# Urban Rural distinction in subsequent files:
## Joining AC NOs with appropriate CP/IHDS DISTRICT NAMES
## * AC_urbextent - Urban extent
## * NASA_GRUMP_settlementAC - Settlements


cat("\n ***** 5 Joining CP and AC maps by state and including geographical info \n")

# Spatial joins of CP and AC by state--------------------------------------------------------
# Only using CP-covered districts may easily lead to misallocations
# through overlaps (particularly for smaller ACs at the border)

suppressPackageStartupMessages({
  library(cptools)
library(sf)
  })


out_path <- paste0(processed_fp, "geo_", wave, ".gpkg")


if(file.exists(out_path)) {
  
  cat("Already processed....\n")
  
  
} else {

elec_path <- paste0(processed_fp, "ACW2014_elec.csv")

geo_path <- paste0(input_fp, "AC_geodata")


distmap <- st_read(paste0(input_fp, "clean_", distmap_str, "_distmap.gpkg"),
                 quiet = TRUE)

AC_elec <- read_csv(elec_path, 
                    col_types = cols())



ac_years <- distinct(AC_elec, ST_NAME, YEAR)

cat("Election year range:", range(ac_years$YEAR), "\n")


ac_main <- st_read(paste0(geo_path, "_", ac_map_type, ".gpkg"),
                          quiet = TRUE) %>% 
    st_make_valid()

## Load geographical data mapped onto assembly constituencies-----

cat("Loading geographical information mapped onto ACs... \n")

# The district names of the AC map are unreliable but the AC_NAMES in the
# elections dataset are ok where available
a_basic_ac_map <- ac_main %>% 
  select(ST_NAME, AC_NO) %>% 
  left_join(select(AC_elec, ST_NAME, AC_NO, AC_NAME))


d_joined_list <- list()

for (st in unique(ac_main$ST_NAME)) { 
  
  ac_st <- include_states(a_basic_ac_map, st) 
  dist_st <- include_states(distmap, st) 
  
  d_joined_list[[st]] <- suppressMessages(st_join(ac_st, dist_st, largest = TRUE))
  
  
}


# bind_rows doesn't work for sf
d_joined_df  <- do.call(rbind, d_joined_list)


# Check overlap (or lack thereof) -----
# ggplot() +
#   geom_sf(data = include_states(a_basic_ac_map, "guj"), aes(fill = AC_NO), colour = "blue") +
#   geom_sf(data = include_states(distmap, "guj"), colour = "black", size = 1.2, fill = NA)


d_missing <- d_joined_df %>%  
  odd_val_col(states = st_exclusions, verbose = FALSE)

# No incomplete sequences
# Post-2008 map has missing values in Gujarat, Madhya Pradesh causing breaks in sequences


d_incomplete <- find_acno_incomplete(d_joined_df)

if(!is.null(d_incomplete)) {
  
  missing_acno_states <- d_incomplete %>%
    exclude_states(st_exclusions) %>% 
    pull(ST_NAME) %>%
    unique()
  
stopifnot(all(missing_acno_states %in% c("Gujarat", "Madhya Pradesh")))


}

cpw_dist <- read_csv(paste0(processed_fp, "CPW", wave, "_dist_", main_fun, ".csv"),
                     col_types = cols())


# Check that all of CP is included
# Chandigarh is not because but its values are still useful for EXPEC_HRS
# Delhi has an odd DISTRICT_NAME distribution in CP

d_cp_missing <- cpw_dist %>% 
  exclude_states(st_exclusions) %>% 
  exclude_states(c("Chandigarh", "Delhi")) %>% 
  anti_join(d_joined_df, by = c("STATE" = "ST_NAME", "DISTRICT_NAME"))

stopifnot(nrow(d_cp_missing) == 0)

# Jharkhand has doubles that are fixed when incorporating CPdist data
d_dupes <- d_joined_df %>% 
  exclude_states(st_exclusions) %>% 
  duplicheck(checkCols = c("ST_NAME", "AC_NO"), fromLast = TRUE)
  

stopifnot(is.null(d_dupes))



# In the GADM map, Karnataka-201 straddles Udupi and Dakshina Kannada and its
# largest part is in Udupi but other maps and the numbering of ACs and other
# clues indicate it belongs to Dakshina Kanada); 


# d_joined_df$DISTRICT_NAME[d_joined_df$AC_NAME == "Moodabidri"] <- "Dakshina Kannada"

# similarly Ramgarh district is composed of two ACs and not one as yielded by
# the maps overlay https://en.wikipedia.org/wiki/Jharkhand_Legislative_Assembly

# d_joined_df$DISTRICT_NAME[d_joined_df$AC_NAME == "Barkagaon"] <- "Ramgarh"




# Import and calculate population density--------------------------------------

# Needs to calculate both 

# Adds Density at 2.5 min resolution. Calculated in AC_map_popdens.R

GPW2010_dens_2pt5 <- st_read(paste0(input_fp, "GPW2010_dens_2pt5_post.gpkg"),
                             quiet = TRUE)

# Adds "Density" with hundreds of obs. per AC
e_dens <- suppressMessages(st_join(d_joined_df, GPW2010_dens_2pt5)) 


e_dens_calc <- e_dens %>%
  st_set_geometry(NULL) %>% # For speed of calculation
  group_by(ST_NAME, DISTRICT_NAME) %>% 
  mutate(DIST_MED_DENSITY = median(Density, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(ST_NAME, AC_NO) %>% 
  mutate(AC_MED_DENSITY = median(Density, na.rm = TRUE)) %>% 
  select(-Density) %>% 
  distinct() %>%
  ungroup() %>% 
  fill(AC_MED_DENSITY) %>%
  left_join(ac_main, by = c("ST_NAME", "AC_NO")) %>%
  st_as_sf() %>% 
  arrange(ST_NAME, AC_NO)

e_dupes <- e_dens_calc %>% 
  exclude_states(st_exclusions) %>% 
  exclude_states('jhar') %>% 
  duplicheck(fromLast = TRUE)

stopifnot(is.null(e_dupes))

find_acno_incomplete(e_dens_calc)


write_sf(e_dens_calc, out_path, delete_layer = TRUE)
}
