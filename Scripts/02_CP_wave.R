# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

## Extracting variables from one wave into format

library(cptools)

cat("\n ******** 2 Combining Consumer Pyramids files... \n")

cp_filepath <- "Input_data/DS_CP_full"

files <- list.files(cp_filepath, full.names = T, recursive = T)

out_path <- paste0(processed_fp, "CPW", wave, "_doubles.csv")


if(file.exists(out_path)) {
  
  
  cat("Already processed... \n")
  
} else {

A1_people_raw <- str_subset(files, paste0('Wave ', wave)) %>% 
  str_subset("People") %>%
  read_csv()

A2_people <- A1_people_raw %>% 
  select(HH_ID, STATE, DISTRICT_NAME, SURVEY_STATUS,
         TOWN_VILLAGE, REGION_TYPE, MEM_WEIGHT_ALL_HR_REGTYPE_STRATA_SAMPLE_WAVE, 
         NON_RESPONSE_FACTOR_MEM_ALL_HR_REGTYPE_STRATA_WAVE, 
         MEM_WEIGHT_EXTN_ALL_HR_REGTYPE_STRATA_SAMPLE_WAVE, MEM_ID, GENDER, AGE_YRS, 
         RELATION_WITH_HOH, RELIGION, CASTE_CATEGORY, CASTE, 
         LITERACY, EDUCATION, HAS_MOBILE) %>% 
  mutate_if(is.factor, fct_relabel, trimws) %>% #removes trailing spaces while keeping factors
  mutate_if(is.character,trimws) %>%
  filter(SURVEY_STATUS == "Accepted") %>% 
  select(-SURVEY_STATUS)

rm(A1_people_raw)


A3_people_HoH <- A2_people %>%
  mutate_at(vars(HAS_MOBILE), list(~ recode(., "Y" = 1, "N" = 0, "Not Applicable" = 0))) %>% 
  group_by(HH_ID) %>% 
  mutate(MEMS_WEIGHT = 
           n()*MEM_WEIGHT_ALL_HR_REGTYPE_STRATA_SAMPLE_WAVE*NON_RESPONSE_FACTOR_MEM_ALL_HR_REGTYPE_STRATA_WAVE/10000,
         OWNED_MOBILES_HH = sum(HAS_MOBILE),
         TOTAL_MEM_IN_HH = n()
  ) %>%
  filter(RELATION_WITH_HOH == "HOH") %>%  #Using Head of HH values as representative
  select(-HAS_MOBILE, -RELATION_WITH_HOH, 
         -MEM_WEIGHT_ALL_HR_REGTYPE_STRATA_SAMPLE_WAVE, 
         -NON_RESPONSE_FACTOR_MEM_ALL_HR_REGTYPE_STRATA_WAVE, 
         -MEM_WEIGHT_EXTN_ALL_HR_REGTYPE_STRATA_SAMPLE_WAVE, -MEM_ID) %>%
  ungroup()


A4_income_raw <- str_subset(files, paste0('Wave ', wave)) %>% 
  str_subset("Household - Income") %>% 
  read_csv()


A5_income <- A4_income_raw %>%
  select(HH_ID, SURVEY_STATUS, MONTH, TOTAL_INCOME) %>%
  mutate_if(is.factor, fct_relabel, trimws) %>% #removes trailing spaces while keeping factors
  mutate_if(is.character,trimws) %>%
  filter(SURVEY_STATUS == "Accepted") %>%
  select(-SURVEY_STATUS) %>% 
  group_by(HH_ID) %>% 
  summarise(TOTAL_INCOME = sum(TOTAL_INCOME)) %>%
  ungroup()


rm(A4_income_raw)


A6_expenses_raw <- str_subset(files, paste0('Wave ', wave)) %>% 
  str_subset("Household Expense") %>% 
  read_csv()

A7_expenses <- A6_expenses_raw %>%
  select(HH_ID, SURVEY_STATUS, MONTH, TOTAL_EXPENDITURE, EXPENSE_ON_FOOD, 
         EXPENSE_ON_EDUCATION, EXPENSE_ON_INTOXICANTS, 
         EXPENSE_ON_CLOTHING_AND_FOOTWEAR, EXPENSE_ON_WATER_CHARGES, 
         EXPENSE_ON_POWER_AND_FUEL, EXPENSE_ON_COOKING_FUEL, 
         EXPENSE_ON_PETROL_AND_DIESEL, EXPENSE_ON_ELECTRICITY) %>%
  mutate_if(is.factor, fct_relabel, trimws) %>% #removes trailing spaces 
                                                # while keeping factors
  mutate_if(is.character,trimws) %>%
  filter(SURVEY_STATUS == "Accepted") %>%
  select(-SURVEY_STATUS) %>%
  group_by(HH_ID) %>% 
  summarise_at(vars(-MONTH), list(sum)) %>% 
  ungroup()

rm(A6_expenses_raw)


A8_amenities_raw <- str_subset(files, paste0('Wave ', wave)) %>% 
  str_subset("Amenities") %>% 
  read_csv()

A9_amenities <- A8_amenities_raw %>% 
  # Remove trailing spaces while keeping factors
  mutate_if(is.factor, fct_relabel, trimws) %>% 
  mutate_if(is.character,trimws) %>%
  
  # Filter only successful ones  
  filter(SURVEY_STATUS == "Accepted") %>% 
  select(-COUNTRY, -SURVEY_STATUS, -REASON_FOR_NON_RESPONSE)  %>%
  
  # Fixes mistake in original dataset   
  mutate(STATE = str_replace(STATE, "Himachal Prades", "Himachal Pradesh")) %>% 
  mutate(STATE = str_replace(STATE, "Himachal Pradeshh", "Himachal Pradesh")) %>% 
  mutate(STATE = as.factor(STATE)) %>% 
  select(HH_ID:STATE,  #NB: STATE_HR had a data entry error so I dropped it
         REGION_TYPE, HAS_POWER_ACCESS, HRS_POWER = POWER_AVAILABILITY_NO_HRS, HAS_TOILET_IN_HOUSE,
         contains("_WATER_"),
         contains("BOUGHT_ASSET"),
         contains("NO_OF_UNITS_OWNED"),
         contains("INTENDS_TO"))

rm(A8_amenities_raw)



A9_exp_people <- inner_join(A7_expenses, A3_people_HoH, 
                            by = c("HH_ID"))

A9_exp_peep_inc <- inner_join(A9_exp_people, A5_income, 
                              by = c("HH_ID"))

A9_cleaned <- inner_join(A9_exp_peep_inc, A9_amenities, 
                       by = c("HH_ID", "STATE", "REGION_TYPE"))






rm(A2_people, A9_amenities, A5_income, A7_expenses, A3_people_HoH,
   A9_exp_peep_inc, A9_exp_people)

# OPERATIONS ON CLEANED DATASET -----------------------------------------


### Basic renaming


B1_renamed <- A9_cleaned %>% 
  # Allowing easier numerical operations and variable selection by recoding
  mutate_at(vars(contains("INTENDS_TO")), list(~recode(., "Y" = 1, "N" = 0))) %>%
  rename_at(vars(contains("INTENDS_TO")), list(~str_replace_all(.,"(INTENDS_TO_BUY_ASSET_)(.*)", 'TOBUY_\\2'))) %>%
  rename_at(vars(contains("NO_OF_UNITS")), list(~str_replace_all(.,"(^.*)(_.*$)", 'OWNED\\2')))

### Calculating peak consumption by HH (PEAK_WATTS) --------------------------------------------------  

# Allowing for variable names like OWNED_2 or OWNED_HOUSE


if ("OWNED_2" %in% colnames(B1_renamed)) {
  ## Estimating HH demand in watts on the basis of appliances owned and intended to buy. 
  ## House lighting calculated below.
  
  B2_peak_watts <- B1_renamed %>%
    group_by(HH_ID) %>% 
    mutate(PEAK_WATTS = sum(OWNED_MOBILES_HH*5,
                            OWNED_2*400, # Refrigerator
                            OWNED_3*2000, # AC
                            OWNED_4*200, # Cooler
                            OWNED_5*1500, # Washing Machine
                            OWNED_6*50, # Television
                            OWNED_7*100, # Computer
                            TOBUY_HOUSE*200, # Assumed second home, rather well-off
                            TOBUY_REFRIGERA_1*400, 
                            TOBUY_AC*2000,
                            TOBUY_COOLER*200,
                            TOBUY_WASHING_M_1*1500,
                            TOBUY_TELEVISION*50,
                            TOBUY_COMPUTER*100)
    ) %>% 
    mutate(PEAK_WATTS =
             case_when(
               # HH has large/many appliances, it must also have additional lighting regardless of income
               OWNED_1 > 0 & PEAK_WATTS > 1000 & HRS_POWER > 20 ~ PEAK_WATTS + OWNED_1*500,
               
               # HH 1/5 of the 1st income quartile are likely to have extremely limited lighting 
               # even if they own their house and have power
               OWNED_1 > 0 & TOTAL_INCOME < 5500 & HAS_POWER_ACCESS == "Y" ~ PEAK_WATTS + 16,
               
               #HH between 1/5 of 1st quartile and 1st quartile must have a bit more lighting
               OWNED_1 > 0 & TOTAL_INCOME < 28000 & TOTAL_INCOME > 5500 ~ PEAK_WATTS + 50,
               
               #HH owns two houses or more but already must have some lighting
               OWNED_1 > 1 ~ PEAK_WATTS + OWNED_1*100,
               
               TRUE ~ PEAK_WATTS
             )
    ) %>% 
    ungroup() %>%
    select(-contains("OWNED_"), -contains("BUY_"), -contains("BOUGHT"), -CASTE) %>% 
    droplevels()
} else {
  B2_peak_watts <- B1_renamed %>%
    group_by(HH_ID) %>% 
    mutate(PEAK_WATTS = sum(OWNED_MOBILES_HH*5,
                            OWNED_REFRIGERATOR*400, # Refrigerator
                            OWNED_AC*2000, # AC
                            OWNED_COOLER*200, # Cooler
                            OWNED_MACHINE*1500, # Washing Machine
                            OWNED_TELEVISION*50, # Television
                            OWNED_COMPUTER*100, # Computer
                            TOBUY_HOUSE*200, # Assumed second home, rather well-off
                            TOBUY_REFRIGERATOR*400, 
                            TOBUY_AC*2000,
                            TOBUY_COOLER*200,
                            TOBUY_WASHING_MACHINE*1500,
                            TOBUY_TELEVISION*50,
                            TOBUY_COMPUTER*100)
    ) %>% 
    mutate(PEAK_WATTS =
             case_when(
               # HH has large/many appliances, it must also have additional lighting regardless of income
               OWNED_HOUSE > 0 & PEAK_WATTS > 1000 & HRS_POWER > 20 ~ PEAK_WATTS + OWNED_HOUSE*500,
               
               # HH 1/5 of the 1st income quartile are likely to have extremely limited lighting 
               # even if they own their house and have power
               OWNED_HOUSE > 0 & TOTAL_INCOME < 5500 & HAS_POWER_ACCESS == "Y" ~ PEAK_WATTS + 16,
               
               #HH between 1/5 of 1st quartile and 1st quartile must have a bit more lighting
               OWNED_HOUSE > 0 & TOTAL_INCOME < 28000 & TOTAL_INCOME > 5500 ~ PEAK_WATTS + 50,
               
               #HH owns two houses or more but already must have some lighting
               OWNED_HOUSE > 1 ~ PEAK_WATTS + OWNED_HOUSE*100,
               
               TRUE ~ PEAK_WATTS
             )
    ) %>% 
    ungroup() %>%
    select(-contains("OWNED_"), -contains("BUY_"), -contains("BOUGHT"), -CASTE) %>% 
    droplevels()
  
}


### Transform into factors --------------------------------------------------


CP_factors_raw <- B2_peak_watts %>% 
  mutate(SCHEDBACK_CASTE = if_else(CASTE_CATEGORY %in% c("OBC", "SC", "ST"), "Y", "N")) %>%
  mutate(
    EDUCATION = 
      fct_collapse(EDUCATION,
                   "None" = "No Education",
                   "Primary" = c("1st Std. Pass", "2nd Std. Pass", "3rd Std. Pass", 
                                 "4th Std. Pass", "5th Std. Pass", "6th Std. Pass"),                        
                   "Secondary" = c("7th Std. Pass", "8th Std. Pass", "9th Std. Pass", 
                                   "10th Std. Pass", "11th Std. Pass", "12th Std. Pass"),
                   "Tertiary" = c("Diploma / certificate course", "Graduate", "Ph.D / M.Phil","Post Graduate")
      ),
    EDUCATION = factor(EDUCATION, levels = c("None", "Primary", "Secondary", "Tertiary"))
  ) %>%
  rename(WEIGHTING = MEMS_WEIGHT) %>% 
  mutate_at(vars(c(HRS_POWER, NO_OF_DAYS_WATER_ACCESS)), list(~ recode(.,`-99` = 0, `-100` = 0))) %>% 
  mutate_at(vars(c(HRS_POWER, NO_OF_DAYS_WATER_ACCESS)), list(~ replace_na(., 0)))


# Testing --- 


CP_factors <- CP_factors_raw %>% 
  mutate(RELIGION = recode(RELIGION, 
                           `Not Applicable` = "Religion not stated"),
         CASTE_CATEGORY = recode(CASTE_CATEGORY, 
                                 `Not Applicable` = "Not Stated"))

odd_val_CP <- odd_val_col(CP_factors)


# FINAL EXPORT ------------------------------------------------------------


CP_doubles <- CP_factors %>%
  mutate(LITERACY = recode(LITERACY, "Y" = 1, "N" = 0, "Not Applicable" = -99)) %>% # Recode does not accept different types of values, NA
  mutate_at(vars(contains("HAS")), list(~ recode(., "Y" = 1, "N" = 0))) %>%
  mutate(SCHEDBACK_CASTE = recode(SCHEDBACK_CASTE, "Y" = 1, "N" = 0))


CP_doubles[CP_doubles == -99] <- NA

write_csv(CP_doubles, out_path)


}
