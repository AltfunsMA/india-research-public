# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

## Merge AC variables into the district-region level, rearrange variables

# Produces slightly different files for analysis (only CP districts) 
# and for map plotting (all districts and in gpkg format)

cat("\n ****** 8 Creating files that are summarised by district-region \n")

# Import from previous------

A1_CPACUR_cen_geo <- read_csv(paste0(processed_fp, "CPACUR_W", infix, ".csv")) %>% 
  select(-POPULATION) %>% 
  mutate(NAXAL = ifelse(AC_NO == 122 & STATE == "Bihar", 
                        "N", as.character(NAXAL))) %>% 
  select(STATE, everything()) %>%
  anti_join(known_gaps) %>%   
  # TODO: add following to known gaps
  filter(!(DISTRICT_NAME == "Baramula" & AC_NO == 47),
       !(DISTRICT_NAME == "Cachar" & AC_NO == 8),
       !(DISTRICT_NAME == "Bangalore" & AC_NO == 173)) 
  
  


a1_missing <- A1_CPACUR_cen_geo %>% 
  select(-TOWNS, -AC_NAME, -DIFF_POP_ELEC) %>%
  odd_val_col(refCols = c("STATE", "DISTRICT_NAME", "REGION_TYPE"),
              verbose = F)

stopifnot(is.null(a1_missing))


A1_dupes <- duplicheck(A1_CPACUR_cen_geo)

stopifnot(unique(A1_dupes$STATE) %in% c("Delhi"))

a1_naxal <- count(A1_CPACUR_cen_geo, NAXAL)

stopifnot(a1_naxal$n[2] > 100)


stopifnot(between(range(A1_CPACUR_cen_geo$PERCWIN), 0, 1))

stopifnot(between(range(A1_CPACUR_cen_geo$P_PERCMARG), 0, 1))

skimr::skim(A1_CPACUR_cen_geo$YEAR)



# Calculates electoral variables on margins and others -------
# for a total of 2872 obs. 76 variables (25 July 2019)

cat("Calculating electoral values and margins... \n")

A2_CPACUR_calc <- A1_CPACUR_cen_geo %>% 
  mutate(AC_TYPE = if_else(AC_TYPE != "GEN", TRUE, FALSE),
         P_TOTVOTES = rccmisc::psum(BJP_NDA, INC_UPA, 
                                    # SHIFTING,
                                    THIRD_FRONT, OTHERS),
         P_SEAT_STATUS = case_when(
               P_PERCMARG > 0.07 & SAME_AS_GOV == TRUE ~ "P_SAFEGOV_SEATS",
               P_PERCMARG > 0.07 & SAME_AS_GOV == FALSE ~ "P_SAFEOPP_SEATS",
               P_PERCMARG < 0.07 & SAME_AS_GOV == TRUE ~ "P_MARGIGOV_SEATS",
               P_PERCMARG < 0.07 & SAME_AS_GOV == FALSE ~ "P_MARGIOPP_SEATS"
               )) %>% 
         mutate(
          # Start ad hoc solutions for Delhi problems
         STATE_NO = if_else(STATE == "Delhi", 7, as.numeric(STATE_NO)),
         DISTRICT_NO = if_else(STATE == "Delhi", 0, as.numeric(STATE_NO)),
         DISTRICT_POPULATION = ifelse(REGION_TYPE == "Urban",
                                      replace_na(DISTRICT_POPULATION, 16333916),
                                      replace_na(DISTRICT_POPULATION, 419319)),
         DISTRICT_ELECTORS = replace_na(DISTRICT_ELECTORS, 0),
         
         # End ad_hoc solutions
         P_ALLIANCE = paste0(P_ALLIANCE, "_SEATCOUNT")) 

 # The following are assumed to be spread out in the averaging below
A2_dupes <- duplicheck(A2_CPACUR_calc)


# AC_level sf object for maps (election and to build others)-----------------------------------------

acdist_elec_ur <- st_read(paste0(processed_fp, "ACW", 
                                 str_remove(infix, suffix),
                                 "_elec_UR.gpkg"), 
                          quiet = T)
  

# Updates the REGION_TYPE and all election information for areas outside of the
# socio-economic 

CPACUR_ACmap <- acdist_elec_ur %>% 
  select(-AC_TYPE) %>%  # Causes incompatible type error in join below
  left_join(A2_CPACUR_calc, by = c("STATE", "DISTRICT_NAME", "AC_NO")) %>% 
  replace_xy() %>% 
  mutate(REGION_TYPE = if_else(is.na(REGION_TYPE), UR_TYPE, REGION_TYPE), 
         .keep = "unused")



# MERGE BY CP DISTRICT/REGION---------------------------------------------

cat("Merging into districts... \n")


A4_CPACUR_merged <- A2_CPACUR_calc %>%
  # Drops variables that are impossible to merge at the district/region level
  select(-AC_NAME, -AC_NO, -PARTYNAME, -PARTYABBRE) %>%
  # Groups by all CP variables as well as others already at the DISTRICT level
  group_by_at(
    vars(
      STATE:SCHEDBACK_CASTE, 
      GOV_PARTIES, GOV_ALLIANCE,
      GOV_LEAD_ALLIANCE, YEAR, NAXAL
    )
  ) %>%
  # % Marginal or Safe or other kinds of seats as percentage of total seats
  mutate(
    NO_SEATS = n(),
    val_status = sum(is.character(P_SEAT_STATUS)) / NO_SEATS,
    val_coalition = sum(is.character(P_ALLIANCE)) / NO_SEATS
  ) %>%
  spread(key = P_SEAT_STATUS, value = val_status, fill = 0) %>%
  spread(key = P_ALLIANCE, value = val_coalition, fill = 0) %>%
  group_by(
    NO_SEATS,
    P_SCHEDBACK_SEATS = sum(AC_TYPE) / NO_SEATS,
    P_GOV_SEATS = sum(SAME_AS_GOV) / NO_SEATS,
    URB_KM2 = sum(AC_URB_KM2),
    KM2 = sum(KM2),
    ELECTORS = sum(ELECTORS),
    TOTVOTES = sum(P_TOTVOTES),
    PARTICIPATION = TOTVOTES / ELECTORS,
    TOWNPOP_2000 = sum(TOWN_POP),
    .add = TRUE
  ) %>%
  mutate_at(
    vars(BJP_NDA, INC_UPA, 
         THIRD_FRONT, OTHERS,
      contains("SAFE"), contains("MARGI"),contains("SEATCOUNT")),
    sum
  ) %>%
  group_by_at(
      vars(BJP_NDA, INC_UPA, 
           THIRD_FRONT, OTHERS,
           contains("SAFE"), contains("MARGI"),contains("SEATCOUNT")),
    .add = TRUE
  ) %>%
  summarise_at(
    vars(
      AC_MED_DENSITY,
      AC_MED_URB_KM2 = AC_URB_KM2,
      P_MED_PERCMARG = P_PERCMARG,
      MED_PERCWIN = PERCWIN,
      MED_KM_TO_CAPITAL = KM_TO_CAPITAL,
      MED_KM_TO_BIGCITY = KM_TO_BIGCITY,
      MED_KM_TO_PLANT = KM_TO_PLANT,
      MED_KM_TO_NEWPLANT = KM_TO_NEWPLANT,
      # These 3 only bc of Delhi
      DISTRICT_POPULATION,
      DISTRICT_ELECTORS,
      DIST_MED_DENSITY
    ),
    median
  ) %>%
  ungroup() %>%
  mutate(
    P_DOMINANT_ALLIANCE = case_when(
      BJP_NDA_SEATCOUNT > 0.5 ~ "BJP_NDA",
      INC_UPA_SEATCOUNT > 0.5 ~ "INC_UPA",
      Independent_SEATCOUNT > 0.5 ~ "Others",
      THIRD_FRONT_SEATCOUNT > 0.5 ~ "Third Front",
      TRUE ~ "None"
    )
  ) %>%
  distinct() %>%
  mutate(DISTRICT_ELECTORS = na_if(DISTRICT_ELECTORS, 0)) %>%
  group_by(STATE, DISTRICT_NAME, REGION_TYPE) %>%
  mutate(
    UR_POP_SHARE = ELECTORS / DISTRICT_ELECTORS,
    G_POP_ESTIMATE = UR_POP_SHARE * DISTRICT_POPULATION,
    COVERAGE_RATIO = HH_COUNT / G_POP_ESTIMATE
  ) %>%
  ungroup() %>% 
  distinct()


A4_CPACUR_duplic <- duplicheck(A4_CPACUR_merged, 
                               checkCols = c("STATE", "DISTRICT_NAME",
                                             "REGION_TYPE"))


# Renames and carrying out some presentation-related adjustments------

cat("Renaming variables... \n")

CPACUR <- A4_CPACUR_merged %>% 
  mutate_if(is.numeric, round, 4) %>% 
  rename_all(list(~str_replace_all(., "DISTRICT", "DIST"))) %>% 
  rename_all(list(~str_replace_all(., "EXPENSE_ON", "EXP"))) %>%
  # Next regex selects any characters after "HAS_" 
  # before the first following underscore (thanks to '?') 
  rename_all(list(~str_replace_all(., "(HAS_)(.*?)_(.*$)", "PROP_W_\\2"))) %>% 
  rename_at(vars(MED_PERCWIN, 
                 ELECTORS, DIST_ELECTORS, TOTVOTES, NO_SEATS, PARTICIPATION), 
            list(~paste0('P_', .))) %>%
  rename_at(vars(BJP_NDA, INC_UPA, 
                 THIRD_FRONT, OTHERS), 
            list(~paste0('P_ALLY_', ., "_VOTES"))) %>% 
  mutate_at(vars(contains("P_ALLY_")), list(~`/`(., P_TOTVOTES))) %>%
  mutate_at(vars(contains("TOTAL")), list(~./1000)) %>% # Re-scaling variable
  rename_at(vars(contains("seatcount")), 
            list(~paste0("P_ALLY_", str_replace_all(., "SEATCOUNT", "SEATS")))
            ) %>% 
  rename_at(vars(AC_MED_DENSITY, AC_MED_URB_KM2, URB_KM2, KM2, 
                 DIST_POPULATION, DIST_MED_DENSITY, TOWNPOP_2000,
                 contains("_KM_")), 
            list(~paste0('G_', .))) %>% 
  rename(P_ELEC_YEAR = YEAR, 
         GINI = DIST_GINI,
         P_STGOV_PARTIES = GOV_PARTIES,
         P_STGOV_ALLYLEAD = GOV_LEAD_ALLIANCE,
         P_STGOV_ALLIANCES = GOV_ALLIANCE,
         PROP_LITERATE = LITERACY,
         PROP_SCHEDBACK = SCHEDBACK_CASTE) %>% 
  # Organise CP vars
  select(STATE,
         DIST_NAME,
         REGION_TYPE, COVERAGE_RATIO, 
         POWER_HRS = HRS_ACCESS, 
         PEAK_POWER = PEAK_WATTS, 
         HH_COUNT, NAXAL,
         contains("PROP"),
         contains("TOTAL"),
         everything()) %>% 
  # Organise other vars
  select(STATE:TOTAL_EXPENDITURE, GINI, matches("^P_"), matches("^P_DIST"), 
         matches("_SEATS"), matches("ALLY"), 
         contains("EXP_"), matches("^G_"))




# Prepare district level sf object for calculating expec_hrs-----------------------------

cat("Preparing calculation of expected hours... \n")

# Only need those variables valid for EXPEC_HRS

B3_CPACUR_map <- CPACUR_ACmap %>%
  select(STATE, DISTRICT_NAME, REGION_TYPE, HRS_ACCESS, NAXAL, KM_TO_PLANT) %>% 
  group_by(STATE, DISTRICT_NAME, REGION_TYPE, NAXAL) %>% 
  summarise(G_MED_KM_TO_PLANT = median(KM_TO_PLANT)) %>% 
  ungroup() %>% 
  left_join(CPACUR, by = c("STATE", "DISTRICT_NAME" = "DIST_NAME", 
                           "REGION_TYPE", "NAXAL")) %>% 
  replace_xy()

# Calculate length of perimeter touching other states ----

touch_df_path <- paste0(processed_fp, "touch_perimeter_df_",
                        distmap_str, "_",
                        ac_map_type, ".csv")

if(!file.exists(touch_df_path)) source("Scripts/08a_touch_perimeter.R")

# Replace 'touching' number with its corresponding district-region HRS_POWER value

touch_df <- read_csv(touch_df_path)

stopifnot(nrow(touch_df) > (nrow(B3_CPACUR_map)*3))

touch_df <- touch_df %>% 
  mutate(touching = na_if(touching, 0))


hrs_touching <- B3_CPACUR_map$POWER_HRS[touch_df$touching]

hrs_touch_df <- touch_df %>% 
  mutate(HRS_TOUCH = hrs_touching) %>% 
  select(ID = origin, t.pc, HRS_TOUCH)



impute_hrs <- function(df, method = impute_method) {
  
  
  if(method == "Iqtl") {
    
    df %>% 
      group_by(STATE) %>% 
      mutate(
        top_hrs = quantile(POWER_HRS, 1, na.rm = TRUE),
        high_hrs = quantile(POWER_HRS, 0.8, na.rm = TRUE),
        med_hrs = mean(POWER_HRS, na.rm = TRUE),
        low_hrs = quantile(POWER_HRS, 0.1, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(ID) %>%
      mutate( HRS_TOUCH =  case_when(
          G_MED_KM_TO_PLANT < 300 & is.na(HRS_TOUCH) & NAXAL == "N" ~ high_hrs,
          G_MED_KM_TO_PLANT > 300 & is.na(HRS_TOUCH) & NAXAL == "N" ~ med_hrs,
          G_MED_KM_TO_PLANT < 300 & is.na(HRS_TOUCH) & NAXAL == "Y" ~ low_hrs,
          G_MED_KM_TO_PLANT > 300 & is.na(HRS_TOUCH) & NAXAL == "Y" ~ 0,
          TRUE ~ as.numeric(HRS_TOUCH)
        ),
        # Fraction of distreg polygon touching other Indian distreg polygons
        TOUCH_FRAC = t.pc/sum(t.pc),
        # Fraction of potential hours 'input'
        HRS_TOUCH_FRAC = HRS_TOUCH*TOUCH_FRAC,  
        EXPEC_HRS = sum(HRS_TOUCH_FRAC, na.rm = TRUE)) %>% 
      ungroup() %>% 
      select(-ID, -HRS_TOUCH, -HRS_TOUCH_FRAC, -TOUCH_FRAC, -t.pc, -high_hrs, -low_hrs, 
             -med_hrs, -top_hrs)
    
  } else if (method == "noI") {
    
    
    df %>%
      mutate(
        TOUCH_FRAC = t.pc / sum(t.pc[!is.na(HRS_TOUCH)]),
        # Fraction of potential hours 'input'
        HRS_TOUCH_FRAC = HRS_TOUCH * TOUCH_FRAC,
        EXPEC_HRS = sum(HRS_TOUCH_FRAC, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(-HRS_TOUCH, -ID, -HRS_TOUCH_FRAC, -TOUCH_FRAC, -t.pc)
    
    
  }
  
  else {error("Must select an imputation method")}
  
}


# Preparing rescaling ----


rescale_final_dfs <- function(df) { 

  # Rescaling so that all variables are in a comfortable 10-1000 range
  # Important for algorithms to converge
  
  df %>% 
    mutate_at(vars(contains("EXP_"), G_KM2, G_URB_KM2, G_TOWNPOP_2000), 
              list(~./1000)) %>%
    mutate_at(vars(contains("_SEATS"), 
                   contains("PROP"), 
                   contains("PERC"), 
                   contains("_VOTES")), 
              list(~.*100)) %>%
    mutate(P_NO_SEATS = P_NO_SEATS/100) %>% 
    mutate_at(vars(contains("KM_TO_")), list(~./100)) %>% 
      # Creating variables on coincidence with main
    # Government controlled does not exactly coincide with 'from same alliance' 
    # for states where there is an 'alliance of alliances' in power, 
    # see political figures below
    mutate(P_GOV_CONTROL = if_else(P_GOV_SEATS > 50, 
                              "Yes", "No"),
      P_ALLIANCE_MATCH = case_when(
        (P_DOMINANT_ALLIANCE == "Third Front" & 
           P_STGOV_ALLIANCES %in% "THIRD_FRONT") ~ "Yes",
        str_detect(str_to_lower(P_STGOV_ALLIANCES), 
                   str_to_lower(P_DOMINANT_ALLIANCE)) ~ "Yes",
        TRUE ~ "No"),
      # Is the state government ruled by a coalition
      P_COALITION_STGOV = if_else(str_detect(P_STGOV_PARTIES, ","), 
                                  TRUE, FALSE))

}

# Carry out expec_hrs imputation and rescaling

cat("Imputing expected hours and rescaling... \n")

CPACUR_distmap <- B3_CPACUR_map %>% 
  st_set_geometry(NULL) %>% 
  mutate(ID = 1:nrow(B3_CPACUR_map)) %>% 
  left_join(hrs_touch_df) %>% 
  impute_hrs() %>% 
  distinct() %>% 
  rescale_final_dfs() %>% 
  left_join(select(B3_CPACUR_map, STATE, DISTRICT_NAME, REGION_TYPE)) %>% 
  st_as_sf() %>% 
  select(STATE, DIST_NAME = DISTRICT_NAME, everything())

mapview::mapview(CPACUR_distmap, zcol = 'EXPEC_HRS')


CPACUR_final <- st_set_geometry(CPACUR_distmap, NULL) %>% 
  right_join(CPACUR, by = c("STATE", "DIST_NAME", "REGION_TYPE", "NAXAL")) %>% 
  replace_xy(ending_to_keep = ".y") %>% 
  rescale_final_dfs() %>% 
  select(STATE, DIST_NAME, REGION_TYPE, 
         EXPEC_HRS, NAXAL, everything()) %>% 
  mutate(ID = 1:nrow(.)) %>% 
  mutate(COVERAGE_RATIO = if_else(STATE == "Delhi" & COVERAGE_RATIO == Inf, 0, COVERAGE_RATIO))




cat("Final tests...")

final_tests <- with(CPACUR_final, c(
  "output dims have changed" =
  nrow(CPACUR) != nrow(CPACUR_final),
  "duplicates exist" =
   !is.null(duplicheck(CPACUR_final, c("STATE", "DIST_NAME", "REGION_TYPE"))),
  "missing values" = 
  !is.null(odd_val_col(CPACUR_final, refCols = c("STATE", "DIST_NAME", "REGION_TYPE"),
                       verbose = F)),
  "too few Naxal regions" =
  count(CPACUR_final, NAXAL)$n[2] < 40,
  "Impossible win percentage" =
  !any(between(range(P_MED_PERCWIN), 0, 100)),
  "Impossible margin percentage" =
  !any(between(range(P_MED_PERCMARG), 0, 100))
))


if(any(final_tests)) {

  error_type <- names(final_tests)
  
  print(error_type[final_tests])
  
  
  print(odd_val_col(CPACUR_final))
  
  
CPACUR_distmap %>% 
  st_set_geometry(NULL) %>% 
  select(EXPEC_HRS, PROP_W_POWER, P_MED_PERCMARG, 
         P_DOMINANT_ALLIANCE, P_SAFEGOV_SEATS, P_ALLIANCE_MATCH) %>% 
  skimr::skim()


CPACUR_final %>% 
  select(EXPEC_HRS, PROP_W_POWER, P_MED_PERCMARG, 
         P_DOMINANT_ALLIANCE, P_SAFEGOV_SEATS, P_ALLIANCE_MATCH) %>% 
skimr::skim()
  
stop("Review ouput")

}

# Save information-------------

# out <- CPACUR_distmap %>% 
#   select(STATE, POWER_HRS, EXPEC_HRS, HH_COUNT) %>% 
#   mapview::mapview(zcol = c("EXPEC_HRS", "POWER_HRS"))

export_path <- paste0(processed_fp, "erss_w", infix)

write_csv(CPACUR_final, paste0(export_path, ".csv"))


cat("\n#################\nKey values: ", infix, "\n||||| DONE |||||| \n\n")



