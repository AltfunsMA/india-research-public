# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

## Merging CP data by district, respecting the urban/rural distinction

cat("\n ******** 3 Aggregating data at the district-region level... \n")

in_path <- paste0(processed_fp, "CPW", wave, "_doubles.csv")
out_path <- paste0(processed_fp, "CPW", wave, "_dist_", main_fun, ".csv")

if(file.exists(out_path)) {
  
  cat("Already processed... \n")
  
} else {

library(cptools)
library(standardize)
library(laeken) # contains Gini and weightedMedian functions, 
  # may mask dplyr::select by introducing MASS
  


# "Doubles" file has a number of factors recoded into 
# integers for ease of calculation below
CP_doubles <- read_csv(in_path)


## Calculations for each district

CP_dist <- CP_doubles %>% 
  group_by(STATE, DISTRICT_NAME, REGION_TYPE) %>% 
  mutate(hh_per_district = n()) %>% 
  # We weight the OLS regression so not weighting here
  group_by(HRS_ACCESS = main_aggreg_FUN(HRS_POWER, na.rm = TRUE), 
           DIST_GINI = gini(TOTAL_INCOME, WEIGHTING)[[1]],
           HH_COUNT = n(),
           DISADVANTAGE = sum(LITERACY, HAS_TOILET_IN_HOUSE, HAS_WATER_ACCESS, 
                              na.rm = TRUE),
           .add = TRUE) %>% 
  # Legacy weighting...
  mutate_at(vars(contains("EXPENSE"), PEAK_WATTS, TOTAL_INCOME, TOTAL_EXPENDITURE), 
    list(~weightedMedian(., weights = WEIGHTING))) %>% 
  group_by_at(vars(contains("EXPENSE"), PEAK_WATTS, TOTAL_INCOME, TOTAL_EXPENDITURE), 
    .add = TRUE) %>% 
  summarise_at(vars(contains("HAS"), LITERACY, SCHEDBACK_CASTE), 
    list(~sum(., na.rm = TRUE)/n())) %>% 
  ungroup() %>% 
  mutate(REGION_TYPE = str_to_title(REGION_TYPE),
         DISTRICT_NAME = str_replace_all(DISTRICT_NAME, "Delh$", "Delhi")) %>% 
  filter(HH_COUNT > 1)   # Otherwise Gini is nonsensical and our other calculations are too biased for
# the relevant district. We report all HH_COUNT values.

CP_dist_weird <- odd_val_col(CP_dist, oddValues = "negatives")
stopifnot(is.null(CP_dist_weird))

CP_dist_missing <- odd_val_col(CP_dist)
stopifnot(is.null(CP_dist_missing))

write_csv(CP_dist, out_path)

}
