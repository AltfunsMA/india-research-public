# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

# Calculating mismatch between estimated Urban/rural proportions 
# (using electors as proxy for population)
# and the known proportions as per the census


assign_census11 <- function(fixed_CPAC) {

# Re-coding AC data to the latest, fixed RURAL/URBAN distinction

  if (wave > 2013) {
    
    sfread(combined_cen_gadm, "Input_data")
   
    fixed_CPAC_dist11 <- fixed_CPAC %>% 
      left_join(rm_list_cols(combined_cen_gadm))

    AC_elec_UR_df_dist11 <-  AC_elec_UR_df %>% 
      left_join(rm_list_cols(combined_cen_gadm))
        
    
  } else {
    
  fixed_CPAC_dist11 <- fixed_CPAC
  
  AC_elec_UR_df_dist11 <- AC_elec_UR_df
  
  }
  

cpac_basics <- fixed_CPAC_dist11 %>% 
  select(STATE, DISTRICT_NAME, DIST11, REGION_TYPE, AC_NO)

AC_UR_reassigned <- cpac_basics %>% 
  full_join(AC_elec_UR_df_dist11, by = c("STATE", 'DISTRICT_NAME',
                                  "DIST11", "AC_NO")) %>% 
  mutate(UR_TYPE = if_else(is.na(REGION_TYPE), 
                           UR_TYPE, 
                           REGION_TYPE)) %>% 
  select(STATE, DISTRICT_NAME, DIST11, AC_NO, UR_TYPE, everything(), 
         -REGION_TYPE) %>% 
  distinct()

reassigned_missing_ac_no <- AC_UR_reassigned %>%
  filter(!(DISTRICT_NAME == "Cachar" & AC_NO == 8)) %>%
  find_acno_incomplete()

if(ac_map_type %in% c("post", "both"))
{
  stopifnot(reassigned_missing_ac_no$STATE %in% c("Madhya Pradesh", "Gujarat",
                                            "Puducherry", "Karnataka"))
 # Added 2 Puducherry gaps to known MadPra and Guj.
  
} else (stop("Review gaps"))


reassigned_missing <- AC_UR_reassigned %>% 
  select(-AC_NAME) %>% 
  check_missing()


# Joining AC and census data -----


simple_census <- census_2011_clean_numbered %>% 
  select(-STATE_NO, -DISTRICT_NAME) # Prevents problems


AC_census <- AC_UR_reassigned %>% 
  mutate(ST_backup = STATE,
    STATE = if_else(STATE == "Telangana", "Andhra Pradesh", STATE)) %>% 
  left_join(simple_census, by = c("STATE",  "DIST11" = "DISTRICT_NO",
                                              "UR_TYPE" = "REGION_TYPE")) %>% 
  select(STATE, DISTRICT_NAME, DIST11, UR_TYPE, everything())

AC_census_missing <- AC_census %>% 
  select(-TOWNS, -AC_NAME) %>% 
  filter(!is.na(STATE) & !is.na(DISTRICT_NAME) & !is.na(DIST11)) %>% 
  # Missing in original data
  filter(!(DISTRICT_NAME == "Karur" & is.na(YEAR))) %>% 
  check_missing() 

if(!is.null(AC_census_missing)) {
  if(interactive()) browser() else { 
    
    warning("Census did not match properly", immediate. = TRUE)
    
    print(AC_census_missing)
}
  
}

AC_census_mismatch <- AC_census %>% 
  group_by(STATE, DISTRICT_NAME) %>% 
  mutate(MULTIMATCH = if_else(length(unique(POPULATION)) > 2, 
                              TRUE, 
                              FALSE)) %>% 
  ungroup() %>% 
  filter(MULTIMATCH)

if(nrow(AC_census_mismatch) > 1) browser()



AC_cenelec_prop <- AC_census %>%
  group_by(STATE, DISTRICT_NAME) %>% 
  mutate(DISTRICT_ELECTORS = sum(ELECTORS, na.rm = TRUE),
         DISTRICT_POPULATION = sum(unique(POPULATION), na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(STATE, DISTRICT_NAME, UR_TYPE) %>%
  mutate(PERCELEC = sum(ELECTORS, na.rm = TRUE)/unique(DISTRICT_ELECTORS),
         PERCPOP = POPULATION/DISTRICT_POPULATION,
         DIFF_POP_ELEC = round(PERCELEC - PERCPOP, 3)) %>% 
  ungroup() %>%
  select(STATE, DISTRICT_NAME, AC_NO, DIST11,
         UR_TYPE, ELECTORS, DISTRICT_ELECTORS,
         POPULATION, DISTRICT_POPULATION, DIFF_POP_ELEC, 
         contains("backup")) %>%
  mutate(STATE = ST_backup)


fixed_CPAC_dist11 %>% 
  left_join(AC_cenelec_prop, by = c("STATE",
                                    "DISTRICT_NAME",
                                    "DIST11",
                                    "AC_NO",
                                    "ELECTORS",
                                    "REGION_TYPE" = "UR_TYPE")) %>%
  distinct()



}
