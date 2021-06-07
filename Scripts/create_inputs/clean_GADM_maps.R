## R 3.5.2 Alfonso Martinez Arranz # 2 Feb 2019 Processing maps downloaded
## originally from:
##
## there is no date, but based on the districts present (from West Bengal and
## Telangana) it seems that they date from before 2017 while at the same time
## having more districts than the census 2011 maps


# Creating SF objects of Indian state borders and acronym labels -----------

in_maps <- "Original/GADM_raw/"
out_maps <- 'Processed/03_admin_maps/'


### GADM States and border ----------------------------------------------------------

# TODO: FIND WHERE THIS FILE IS NOW AFTER SOME RELOCATIONS
state_type <- read_csv("Processed/CPAC_by_UrbRurDist.csv") %>% 
  select(STATE, STATE_GROUPING) %>% 
  distinct()

GADM_states_raw <- readRDS(paste0(in_maps, "GADM/gadm36_IND_1_sf.rds")) 

GADM_border <- st_union(GADM_states_raw) # Border for the whole country (so it can have different colour)

GADM_states <- GADM_states_raw %>%  
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  ) %>% 
  mutate(acronym = str_replace(HASC_1, "(^.*)\\.(.*$)", "\\2")) %>%
  mutate(acronym = recode(acronym, "OR" = "OD")) %>% 
  select(state = NAME_1, lon, lat, acronym) %>% 
  mutate(state = recode(state, "NCT of Delhi" = "Delhi", "Jammu and Kashmir" = 
                        "Jammu & Kashmir")) %>% 
  rename_all(str_to_upper) %>% 
  left_join(state_type)
  

write_st(GADM_states, paste0(out_maps, "GADM/GADM_states.gpkg"))


### GADM districts ----------------------------------------------------------

GADM_dist_raw <- readRDS(paste0(in_maps, "/GADM/gadm36_IND_2_sf.rds"))




# Fixing missing Delhi districts by replicating the existing one


GADM_dist_to_fix <- raw_GADM_file %>%
  mutate(NAME_1 = recode(
    NAME_1,
    "Jammu and Kashmir" = "Jammu & Kashmir",
    "NCT of Delhi" = "Delhi"
  )) %>%
  rename(STATE = NAME_1, DISTRICT_NAME = NAME_2)


GADM_dist_delhi_only <- GADM_dist_to_fix %>%
  filter(STATE == "Delhi")

GADM_dist_Delhi_reps <-
  do.call(rbind, replicate(7, GADM_dist_delhi_only, simplify = FALSE)) %>%
  mutate(
    DISTRICT_NAME = c(
      "North",
      "North West",
      "South",
      "South West",
      "North - North West - West",
      "North East - East",
      "South - South West - Central - New Delhi"
    )
  )

rbind(GADM_dist_to_fix, GADM_dist_Delhi_reps) %>%
  select(STATE, DISTRICT_NAME)


#Fixing discrepancies in state and district names between Consumer PYramids
#(2014) and GADM (differnet ones by wave)

rename_GADM_dist <- function(df, fix_discrep_file_id) {
  
  # Previously compiled df with discrepancies and corrections
  discrep <- read_csv(paste0(out_maps, "GADM_CPW", fix_discrep_file_id, "_discrep_fixed.csv"))
  
  names(discrep$modify_into) <- discrep$from
  
  df %>% 
    mutate(DISTRICT_NAME = recode(DISTRICT_NAME, !!!discrepList)) 
  
  # Replicate and rename unique Delhi district in GADM to fit CP dataset
  

  
}


# TODO: This section only shows operations after GADM_dist has already been
# processed for CW1 because the original operations done for CPW1 have been lost


GADM_dist <- GADM_dist_raw %>% 
  rename_GADM_dist('1_2014')
  
  rcsv(CPW2019_dist)

rcsv(CPW2014_dist)

mismatched_in_2019 <- CPW2019_dist %>% 
  distinct(STATE, DISTRICT_NAME) %>% 
  anti_join(GADM_dist, by = c("STATE", "DISTRICT_NAME"))


CPW2014_dist %>% 
  distinct(STATE, DISTRICT_NAME) %>% 
  anti_join(GADM_dist, by = c("STATE", "DISTRICT_NAME"))

# Renaming the additional ones from 2019 will not change  those renamed from 2014
CPW2014_dist %>% 
  semi_join(mismatched_in_2019)

write_csv(mismatched_in_2019, paste0(out_maps, "fixing_GADM_CPW18_2019_discrep.csv"))  

GADM_dist %>% 
  semi_join(mismatched_in_2019, by = "STATE") %>% 
  distinct(STATE, DISTRICT_NAME) %>% 
  View()


  
GADM_dist_final <- GADM_dist %>%   
  rename_GADM_dist('18_2019')


CPW2019_dist %>% 
  distinct(STATE, DISTRICT_NAME) %>% 
  anti_join(GADM_dist_final, by = c("STATE", "DISTRICT_NAME"))



write_sf(GADM_dist_final, paste0(out_maps, "clean_gadm_distmap.gpkg"))




