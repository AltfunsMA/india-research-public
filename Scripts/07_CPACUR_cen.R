# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

# Merging of CP and AC data and compensating 
# for deviations from Urban/Rural estimates 

cat("\n ****** 7 Joining CP and election dataset and correcting deviations \n")

out_path <- paste0(processed_fp, "/CPACUR_W", infix, ".csv")

if(file.exists(out_path)) {
  
  
  cat("Already processed... \n")
  
  
} else {


# LOAD AND SET-UP ##############################


suppressPackageStartupMessages(library(cptools))

ur_fixes <- "Processed/ur_fixes_helpmaps/" # Not used



check_missing <- function(df) {
  
# At this stage, there are quite a few justifiable missing values that
# "odd_val_col" captures
  
  df %>%
    exclude_states("Delhi") %>% # Because CP districts are sui generis
    exclude_states("Chandigarh") %>% # No Vidhan Sabha --> no election data
    anti_join(known_gaps, by = c("STATE" = "ST_NAME", "AC_NO", "YEAR")) %>% 
    # TODO: add following to known gaps
    filter(!(DISTRICT_NAME == "Baramula" & AC_NO == 47),
           !(DISTRICT_NAME == "Cachar" & AC_NO == 8),
           !(DISTRICT_NAME == "Bangalore" & AC_NO == 173)) %>%
    select(-any_of(
      c("AC_NAME", # Missing for some election data but good as backup
        "TOWNS", # Not every AC has a town, but useful to keep for checks
        "DIST_NAME" # Some missing in original election data w/o implication
    ))) %>% 
  odd_val_col(refCols = c(1:5), oddValues = NA, states = st_exclusions,
              verbose = FALSE)

}


is_cp_complete <- function(df){
  
  CP_dist %>% 
    exclude_states("Chandigarh") %>%
    exclude_states(st_exclusions) %>% 
    anti_join(df)
  
  
}


# To  be used in the next script to generate maps at AC level
AC_elec_UR_full <- st_read(paste0(processed_fp, "ACW", str_remove(infix, suffix), "_elec_UR.gpkg"),
                      quiet = TRUE) %>% 
  select(-matches("_dist$"))

# To be used here for debugging if necessary
AC_elec_UR <-  AC_elec_UR_full %>% 
  mutate(STATE = if_else(is.na(STATE), ST_NAME, STATE)) %>% 
  select(-ST_NAME, -DIST_NAME, -ALT_ST_NAME, -ALT_AC_NO) %>% 
  exclude_states(st_exclusions)

CP_dist <- read_csv(paste0(processed_fp, "/CPW", wave, 
                           "_dist_", main_fun, ".csv")) %>% 
  # These have problems with their AC boundaries crossing through the middle of 
  # district boundaries
  filter(!str_detect(DISTRICT_NAME, c("Ramgarh|Chirang|Baksa|South - South West"))) %>%
  exclude_states(st_exclusions)


# INITIAL JOIN #####################################

cat("Initial join of electoral and district-level variables... \n")


AC_elec_UR_df <- rm_list_cols(AC_elec_UR) %>% 
  mutate(STATE = if_else(STATE == "Andhra Pradesh" & AC_NO < 120, 
                           "Telangana",  # For compatibility
                           as.character(STATE))) %>% 
# TODO: add following to known gaps
filter(!(DISTRICT_NAME == "Baramula" & AC_NO == 47),
       !(DISTRICT_NAME == "Cachar" & AC_NO == 8),
       !(DISTRICT_NAME == "Bangalore" & AC_NO == 173))

  
a_CPAC <- CP_dist %>% 
  left_join(AC_elec_UR_df, by = c("STATE", "DISTRICT_NAME",
                                  "REGION_TYPE" = "UR_TYPE")) %>% 
  select(STATE, DISTRICT_NAME, AC_NO, REGION_TYPE, 
         AC_MED_DENSITY, HRS_ACCESS, SAME_AS_GOV, everything()) %>% 
  arrange(STATE, AC_NO) %>% 
  distinct()


# Generate objects in case fixes are needed--------------
# TODO: PERHAPS USING MAPVIEW WOULD BE MORE EFFICIENT _IF_ I COULD INTRODUCE
# SOME DENSITY BACKGROUND VALUES 

AC_urban_rural <- AC_elec_UR %>%
  dplyr::select(STATE, DISTRICT_NAME, AC_NO, UR_TYPE) %>%
  exclude_states(st_exclusions) 

AC_distmap <- AC_elec_UR %>%
  select(STATE, DISTRICT_NAME) %>%
  exclude_states(st_exclusions) %>% 
  group_by(STATE, DISTRICT_NAME) %>%
  summarise()

elec_year_df <- AC_elec_UR_df %>% 
  distinct(STATE, YEAR)


# MISSING URBAN/RURAL TYPE FIXES  #####################################
# Missing values are the result of all ACs being allocated to either rural
# (most common) or urban regions, but the CP-data containing reference to the
# other one.


discrep_path <- paste0(paste0(input_fp, "miss_ur_type_w", wave, '_', 
                       ac_map_type, 
                       ".csv"))

if(!file.exists(discrep_path)) {
  
  cat(discrep_path, "not found \n")
  
  a_missing <- check_missing(a_CPAC)

  fp <- list.files(input_fp, 
                   pattern = paste0("fixed_miss_ur_type_w\\d+_",
                                                   ac_map_type),
                   full.names = T)
  
  # Perhaps this should be done only if distmap and ac_map_type match
  fixed_w_same_map_type <- map_dfr(fp, read_csv, col_types = c("cccc"))
   
  if(nrow(fixed_w_same_map_type) != 0) {
    
    a_still_missing <- a_missing %>%
      select(STATE, DISTRICT_NAME, AC_NO) %>%
      mutate(AC_NO = as.character(AC_NO)) %>% 
      left_join(fixed_w_same_map_type, by = c("STATE", "DISTRICT_NAME")) %>%
      replace_xy() %>% 
      distinct()
    
    
    write_csv(a_still_missing, discrep_path)
  
  } else { 
    
    a_still_missing <- a_missing
    
    a_still_missing %>% 
      select(any_of(c("STATE", "DISTRICT_NAME", "DIST11",
                      "REGION_TYPE", "AC_NO"))) %>% 
      left_join(elec_year_df, by = "STATE") %>% # adds year for  
      distinct() %>% 
      write_csv(discrep_path) 
    
    }
  
  cat("Printing maps for checking missing values \n")
  
  source("Scripts/create_inputs/plot_UR_split.R")
  
  
  problem_DISTR <- a_still_missing %>% 
    filter(is.na(AC_NO)) %>% 
    distinct(STATE, DISTRICT_NAME)
  
  
  districts_ur_split <- map(a_still_missing$DISTRICT_NAME,
                         ~ plot_UR_split(filter_district = .x))
  
  map_PDF(districts_ur_split,
          folder = paste0("first_ur_split_by_district_w", infix),
          multiple = TRUE)
  
  # states_ur_split <- map(unique(a_CPAC$STATE),
  #                           ~ plot_UR_split(filter_state = .x))
  # 
  # map_PDF(states_ur_split,
  #         folder = paste0("first_ur_split_by_state_w", infix),
  #         multiple = TRUE)

  stop("Need to manually introduce the AC_NO in the 'fixing' file: \n",
       discrep_path)
  

}

# FIRST REASSIGNMENT OF REGION_TYPEs / MISSING VALUE fixes #########

cat("First reassignment of ACs to Urban-rural region... \n")

source("Scripts/07b_reassign_region.R", local = job)

b_reassigned <- reassign_region(a_CPAC, str_replace(discrep_path, "miss",
                                                    "fixed_miss"))

# If still missing, need to adjust the miss_ur_type file
b_missing <-  check_missing(b_reassigned)

stopifnot(nrow(b_missing) == 0)

b_dupes <- duplicheck(b_reassigned, checkCols = c("STATE", "AC_NO"), 
                      refCols = "DISTRICT_NAME")


# If still absent, may need to adjust the miss_ur_type file
# Also possible that the districts are not properly allocate through the maps
b_cp_absent <- is_cp_complete(b_reassigned)

stopifnot(nrow(b_cp_absent) == 0)



# ADDING CENSUS 2011 TO FURTHER VERIFY Urban-rural SPLIT #######################

source("Scripts/07a_assign_census11.R", local = job)

cat("Adding census data... \n")
  
rcsv(census_2011_clean_numbered, "Input_data/") # Pre-processed and tidied census data

c_CP_AC_UR_prop <- assign_census11(b_reassigned)


c_missing <- check_missing(c_CP_AC_UR_prop)

stopifnot(nrow(c_missing) == 0)

c_duplicheck <- duplicheck(c_CP_AC_UR_prop)


d_summary <- c_CP_AC_UR_prop %>% 
  filter(REGION_TYPE == "Rural") %>% 
  select(STATE, DISTRICT_NAME, DIFF_POP_ELEC) %>% 
  distinct()

create_boxplot(d_summary, "STATE", "DIFF_POP_ELEC") +
  ylab ("Proportion of rural electors - proportion of rural population in Census")


quantile(d_summary$DIFF_POP_ELEC, c(0.1, 0.9), na.rm = TRUE)

# FIXES FOR OUTLYING VALUES IN URBAN/RURAL SPLIT ---------------------------------

cat("Adjusting urban/rural split to minimise outliers wrt census... \n")

outlier_fixpath <- paste0(paste0(input_fp, "outlier_w", wave, "_",
                          ac_map_type, ".csv"))

if(!file.exists(outlier_fixpath)) {

  d_outlier_dist <- c_CP_AC_UR_prop %>%
    filter((abs(DIFF_POP_ELEC) > 0.20)) %>%
    mutate(TOWN_POP = scales::comma(TOWN_POP),
      ELECTORS = scales::comma(ELECTORS),
          POPULATION = scales::comma(POPULATION)
           ) %>%
    select(STATE, DISTRICT_NAME, DIST11, AC_NO, TOWN_POP, ELECTORS, POPULATION,
           REGION_TYPE, DIFF_POP_ELEC) %>%
    left_join(elec_year_df, by = "STATE") %>% 
    mutate(map_type = if_else(YEAR < 2008, "pre", "post"),
           .keep = "unused") %>%
    arrange(STATE, desc(abs(DIFF_POP_ELEC)))
  

    fp <- list.files(input_fp, pattern = paste0("outlier_formatted_wave\\d+", "_",
                                      ac_map_type),
                     full.names = T)
  
    already_fixed_outliers <- map_dfr(fp, read_csv, col_types = c("cccc")) 
    
    afo_joinvars <- c("STATE", "DISTRICT_NAME", "AC_NO")
    
  afo <- already_fixed_outliers %>% 
    {if (any(grepl(";", .$AC_NO))) mutate(., AC_NO = strsplit(AC_NO, ";")) else . } %>% 
    unnest(AC_NO) %>%  # necessary after strsplit
    mutate_if(is.character, str_squish) %>% 
    mutate(AC_NO = as.numeric(AC_NO))
  

  d_still_outliers <- d_outlier_dist %>%
    full_join(afo, by = afo_joinvars) %>% 
    filter(!is.na(DIST11))
  

  
  write_csv(d_still_outliers, outlier_fixpath)
  
  stop("Look at the outlier file just created under ", outlier_fixpath)
  
  
  # Facilitating solution
  
  # Print maps for observation
  source("Scripts/Maps/plot_UR_split.R")
  
  AC_urban_rural <- AC_elec_UR %>%
    left_join(select(c_CP_AC_UR_prop, STATE, DISTRICT_NAME, AC_NO, REGION_TYPE)) %>%
    mutate(REGION_TYPE = if_else(is.na(REGION_TYPE), UR_TYPE, REGION_TYPE)) %>% 
    st_as_sf() %>%
    dplyr::select(STATE, DISTRICT_NAME, AC_NO, REGION_TYPE)

  select_regions <- d_still_outliers %>% 
    filter(is.na(NEW_REGION_TYPE)) %>% 
    # In case testing is needed, altering the regex allows to select only a few regions
    #  "\\w" selects all
    filter(grepl("\\w", DISTRICT_NAME, ignore.case = T)) %>% 
    pull(DISTRICT_NAME) %>% 
    unique()
    
  
  districts_ur_census <- map(select_regions,
                         ~ plot_UR_split(updated_version = TRUE, 
                           filter_district = .x,
                           legend_omit = TRUE
                         ))
  
  
  map_PDF(districts_ur_census, 
          folder = paste0("outliers_w", infix),
          multiple = TRUE)
  
  # # Load and format the file after the fixes have been done:
  
  d_outlier_properfix <- read_csv(outlier_fixpath) %>%
    arrange(STATE, AC_NO) %>%
    fill(DIST11, .direction = "updown") %>%
    mutate(NEW_REGION_TYPE = if_else(is.na(NEW_REGION_TYPE),
                                 REGION_TYPE, NEW_REGION_TYPE)) %>%
    select(STATE, DISTRICT_NAME, DIST11, AC_NO, NEW_REGION_TYPE)
  
  odd_val_col(d_outlier_properfix, verbose = F)

  }



# SECOND REASSIGNMENT #########################################################

cat("Second reassignment... \n")

# # save properly formatted version so that reassign_region function can load...
outlier_final_path <- str_replace(outlier_fixpath, "outlier", "fixed_outlier")

if(!file.exists(outlier_final_path)) write_csv(d_outlier_properfix, 
                                               outlier_final_path)
 

e_CP_AC_re_regioned <- reassign_region(c_CP_AC_UR_prop, outlier_final_path)


e_duplicated <- duplicheck(e_CP_AC_re_regioned, refCols = "DISTRICT_NAME", 
                            checkCols = c("STATE", "AC_NO"))

e_missing <- e_CP_AC_re_regioned %>% 
  check_missing()

stopifnot(nrow(e_missing) == 0)

e_cp_absent <- is_cp_complete(e_CP_AC_re_regioned)


stopifnot(nrow(e_cp_absent) == 0)

# Test again the deviation against the census------------------------

f_CPACUR_cen <- assign_census11(e_CP_AC_re_regioned)

f_missing <- check_missing(f_CPACUR_cen)

stopifnot(nrow(f_missing) == 0)

f_dupes <- duplicheck(f_CPACUR_cen)

f_cp_absent <- is_cp_complete(f_CPACUR_cen) 

stopifnot(nrow(f_cp_absent) == 0)

f_summary <- f_CPACUR_cen %>% 
  filter(REGION_TYPE == "Rural") %>% 
  select(STATE, DISTRICT_NAME, DIFF_POP_ELEC) %>% 
  distinct()

create_boxplot(f_summary, "STATE", "DIFF_POP_ELEC") +
  ylab ("Proportion of rural electors - proportion of rural population in Census")


# For a small number of districts the discrepancy cannot be driven any lower
e_xcessive_deviation <- f_summary %>% 
  filter(abs(DIFF_POP_ELEC) > 0.25)


stopifnot(nrow(e_xcessive_deviation) < nrow(f_summary)*0.1)


write_csv(f_CPACUR_cen, out_path)

}
