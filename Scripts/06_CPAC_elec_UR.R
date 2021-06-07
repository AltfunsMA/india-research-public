# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021


# (1) adding elections 
# (2) calculating first urban rural split
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# (NB: urban-rural split calculation USES ELECTOR NUMBERS; cannot separate the
# two)##
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# (3) adding Naxal affected areas


cat("\n ******6 Adding election data and determining urban-rural split \n")

# Setup ------

suppressPackageStartupMessages(library(cptools))

out_path <- paste0(processed_fp, "ACW", str_remove(infix, suffix),
                   "_elec_UR.gpkg")

if(file.exists(out_path)) {
  
  
  cat("Already processed... \n")
  
  
} else {

  
AC_elec_raw <- read_csv(paste0(processed_fp, "ACW2014_elec.csv"))

acdist_geo <- st_read(paste0(processed_fp,
                             "geo_", wave,
                             ".gpkg"),
                      quiet = TRUE)





# Telangana split in 2014 is always represented in maps (?)
# In the post-2008 map TELANGANA CONTINUES ITS AC NO SEQUENCE FROM AP, BUT 
# THIS SEQUENCE IS DIFFERENT TO THE ONE PRE-2008 (when Telangana did not exist)
# so that post2008 AP-120 is roughly pre-2008 AP-1 and AP-187 is AP-294

# In the pre-2008 map, there is no Telangana
# but CP may have it already we need to change the state name to match with 
# CP geographically. For the post-map, this is not necessary.
# Below, there are other changes to the AC_NO

if(ac_map_type == "pre") {

  AC_elec <- AC_elec_raw

} else {
  
  AC_elec <- AC_elec_raw %>% 
    mutate(ST_NAME = if_else(ST_NAME == "Andhra Pradesh" & AC_NO < 120,
                                      "Telangana", 
                                      as.character(ST_NAME))) 
  
}


# Add geographical and election data--------------------------------------------

cat("Adding geographical and election data... \n")


c_elec_dens <- acdist_geo %>% 
  left_join(AC_elec, c("ST_NAME", "AC_NO")) %>% 
  # If there are bifurcation issues
  left_join(AC_elec, c("ST_NAME" = 'ALT_ST_NAME', 'AC_NO' = 'ALT_AC_NO')) %>%
  replace_xy() %>%
  select(-any_of(c("ST_NAME.y", "AC_NO.y"))) %>% 
  distinct(ST_NAME, AC_NO, .keep_all = TRUE) %>% 
  arrange(ST_NAME, AC_NO) %>% 
  fill(YEAR)

names(c_elec_dens)[str_detect(names(c_elec_dens), "\\.y")]

c_incomplete <- c_elec_dens %>% 
  exclude_states(st_exclusions) %>% 
  find_acno_incomplete() %>% 
  anti_join(known_gaps)

if(!is.null(c_incomplete)) {
  
  states_w_missing_acno <- c_incomplete %>% 
    exclude_states(st_exclusions) %>% 
    pull(ST_NAME) %>% 
    unique()
  
  if(ac_map_type %in% c("post", "both"))
    {
    stopifnot(states_w_missing_acno %in% c("Madhya Pradesh", "Gujarat"))

  } else (stop("Review gaps"))
  
  
}

c_dupes <- suppressWarnings(c_elec_dens %>%  
  exclude_states("small_rich") %>% 
  duplicheck(c("ST_NAME", "AC_NO"), fromLast = TRUE))


c_missing <- c_elec_dens %>% 
  anti_join(known_gaps) %>% 
  exclude_states(st_exclusions) %>% # Not useful for comparison
  select(ST_NAME, AC_NO, PARTYABBRE) %>% 
  odd_val_col(verbose = FALSE)




# Calculate initial urban rural split------------------------------------------

cat("Calculating urban/rural split... \n")

e_urban_rural <- c_elec_dens %>% 
  mutate(UR_TYPE = if_else(
    # 1. Dense urban cover but no actual separate towns (city centre in other AC)
    (AC_URB_KM2 > as.numeric(KM2/3) & AC_MED_DENSITY > 1.25*DIST_MED_DENSITY) | 
      # 2. Significantly higher density but no other data (e.g. data too old)         
      (AC_MED_DENSITY > 1.5135029*DIST_MED_DENSITY) | 
      # 3. Very small AC almost guaranteed to be Urban area 
      # but sometimes not captured by 1. or 2.
      (KM2 < 100) |
      # 4. Population concentrated in large settlements
      # TOWN_POP refers to population in settlements > 5000 hab. in 2000
      # ELECTORS refers to registered voters in the relevant election years
      (TOWN_POP > ELECTORS/3 & AC_MED_DENSITY > DIST_MED_DENSITY), 
    "Urban", 
    "Rural"))

# Adding Naxalite (left-wing extremism) insurgency figures --------------------

cat("Adding Naxalite data...\n")

Naxal_dist_clean <- rcsv(Naxal_districts_raw, "Input_data/") %>% 
  filter(!is.na(DIST_NAME)) %>% 
  {if(wave > "2000") mutate(., 
                            ST_NAME = recode(ST_NAME, 
                                             'Telangana' = 'Andhra Pradesh'),
                            ) else .} %>% 
  mutate(DIST_NAME = str_replace(DIST_NAME, "\\d{1,3}\\.",""),
         NAXAL = "Y") %>% 
  mutate_if(is.character, str_trim)

add_naxal <- function(df) {
  
  if (wave < 2000) {
    
    df %>% 
      left_join(Naxal_dist_clean) %>% 
      replace_xy("NAXAL")
    
    
  } else { df }
  
}

f_distaNaxal <- e_urban_rural  %>% 
  # Following implies geographical equivalence (i.e. only spelling/name changes)
  #  between DIST_NAME and DISTRICT_NAME, which may not be the case.
  left_join(Naxal_dist_clean, by = c("STATE" = "ST_NAME",
                                     "DISTRICT_NAME" = "DIST_NAME")) %>% 
  # Must be named; otherwise function will throw error about 'geometry' column
  add_naxal() %>% 
  mutate(NAXAL = replace_na(NAXAL, "N"))

cat("Fixing Delhi and other small regions...\n")


# For Delhi, CP does not follow the official district division  which means the
# maps cannot capture it and the DISTRICT_NAME <-> AC_NO correspond
# Puducherry and some other small locations have problems with UR allocations
# because of their size and imprecision in the maps themselves.
  
delhi_pudu_fixes_2014 <- read_csv(paste0(input_fp, "delhi_pudu_fixes_2014.csv")) %>% 
  rename(DISTFIX = DISTRICT_NAME)

delhi_pudu_fixes_2019 <- read_csv(paste0(input_fp, "delhi_pudu_fixes_2019.csv"))

out <- f_distaNaxal %>% 
  left_join(delhi_pudu_fixes_2014) %>% 
  mutate(DISTRICT_NAME = if_else(STATE == "Delhi", DISTFIX, DISTRICT_NAME), 
         UR_TYPE = if_else(!is.na(REGION_TYPE), REGION_TYPE, UR_TYPE)) %>% 
select(-DISTFIX, -REGION_TYPE) %>% 
  left_join(delhi_pudu_fixes_2019, c("STATE", "DISTRICT_NAME", "AC_NO")) %>% 
  mutate(UR_TYPE = if_else(!is.na(REGION_TYPE), REGION_TYPE, UR_TYPE)) %>% 
  select(-REGION_TYPE)


f_Naxal_missing <- anti_join(Naxal_dist_clean, out, 
                             by = c("ST_NAME", "DIST_NAME")) %>% 
  anti_join(out, by = c("ST_NAME" = "STATE",
                                 "DIST_NAME" = "DISTRICT_NAME")) %>% 
  arrange(ST_NAME, DIST_NAME) # Should be 0 for post 2011

names(out)[str_detect(names(out), "\\.y|\\.x")]


states_w_missing_acno <- out %>% 
  exclude_states(st_exclusions) %>% 
  find_acno_incomplete() %>% 
  anti_join(known_gaps) %>% 
  pull(ST_NAME) %>% 
  unique()
  
# Original maps merge some small urban ACs in Madhya and Gujarat, they belong to
# the same district so they can be merged
stopifnot(states_w_missing_acno %in% c("Madhya Pradesh", "Gujarat"))
    

write_sf(out, out_path, delete_layer = TRUE)
  
}
