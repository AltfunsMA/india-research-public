# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

# combining output from foregoing pipeline and presenting some of the results of the paper

library(cptools)
library(lmtest)
library(sandwich)
library(foreign)

# Loading main datasets with new mean-based variables and old median-based ones -----

load_infix <- str_remove(suffix, main_fun)

median_df <- read_csv(paste0(processed_fp, "erss_w2014_", load_infix, "median.csv")) %>% 
  rename_with(~paste0("MEDIAN_", .x), contains('_HRS'))

median_df_2019 <- read_csv(paste0(processed_fp, "erss_w2019_", load_infix, "median.csv")) %>% 
  select(STATE, DIST_NAME, REGION_TYPE, HH_COUNT2019 = HH_COUNT, 
         MEDIAN_POWER_HRS_2019 = POWER_HRS)

mean_df <- read_csv(paste0(processed_fp, "erss_w2014_", load_infix, "mean.csv")) %>% 
  rename_with(~paste0("MEAN_", .x), contains('_HRS')) %>% 
  select(STATE, DIST_NAME, REGION_TYPE, MEAN_EXPEC_HRS)


mean_df_2019 <- read_csv(paste0(processed_fp, "erss_w2019_", load_infix, "mean.csv")) %>% 
  select(STATE, DIST_NAME, REGION_TYPE, HH_COUNT2019 = HH_COUNT, POWER_HRS_2019 = POWER_HRS) %>% 
  rename_with(~paste0("MEAN_", .x), contains('_HRS'))


odd_val_col(median_df)

odd_val_col(mean_df_2019)



# Initial joins

combined_df <-   median_df %>% 
  left_join(mean_df) %>% 
  left_join(median_df_2019) %>% 
  mutate(NEW_PLANTS_WITHIN_300K = if_else(G_MED_KM_TO_NEWPLANT < 3, 1, 0)) %>% 
  rename_all(str_to_lower)

# Rename as in submitted file


submitted <- read_csv("G:/Shared drives/IEP Key Data/ERSS_paper/ERSS_India_energy_resubmission_May2021/Energy_India_ERSS_replication_files/ERSS_India_energy_replication.csv") %>% 
  mutate(state = recode(state, "Jammu and Kashmir" = "Jammu & Kashmir"),
         alliancematch2 = if_else(alliancematch2 == 1, "Yes", "No"),
                stgov_alliance = str_to_upper(str_replace_all(stgov_alliance, 
                                                              c(" " = "_", "-" = "_"))))


final_df <- combined_df %>% 
  filter(!is.na(median_power_hrs), !is.na(median_power_hrs_2019)) %>% 
  select(state,
         dist_name,
         region_type,
         prop_schedback,
         total_income,
         stgov_alliance = p_stgov_allylead,
         perc_marg = p_med_percmarg,
         p_dominant_alliance,
         p_stgov_parties,
         p_stgov_alliances,
         alliancematch2 = p_alliance_match,
         new_plants_within_300k,
         hh_count,
         hh_count2019,
         mean_expec_hrs,
         median_power_hrs,
         median_power_hrs_2019) %>% 
  mutate(improvemed1419 = median_power_hrs_2019 - median_power_hrs,
         med14is24 = if_else(median_power_hrs == 24, 1, 0),
         hh_min = min(hh_count, hh_count2019)) 


<<<<<<< HEAD
=======

general_formula <- paste0("improvemed1419 ~ alliancematch2 * perc_marg + stgov_alliance +",
       "mean_expec_hrs + median_power_hrs + prop_schedback + total_income + region_type +",
       "new_plants_within_300k")

by_state_gov_formula <- paste0("improvemed1419 ~ alliancematch2 * perc_marg + ",
            "mean_expec_hrs + median_power_hrs + prop_schedback + total_income + region_type +",
            "new_plants_within_300k")



run_approximate_replica <- function(DF) {
  
  st_group <- c("_", "BJP_NDA", "INC_UPA", "THIRD_FRONT")
  form_type <- c(general_formula, rep(by_state_gov_formula, 3))
  
  mod_final <- map2(form_type, st_group,
                    ~lm(.x, 
                        data = filter(DF, str_detect(stgov_alliance, .y))))
  
  mod_robust <- map(mod_final, ~coeftest(.x, vcov. = vcovHC(.x, type = 'HC1')))
  
  stargazer::stargazer(mod_robust, type = "text",  report = "vcsp*", 
                       title = paste("data = ", deparse(substitute(DF))),
                       column.labels = str_replace(st_group, "_", "-"),
                       dep.var.labels = "improvemed1419")
  
}


final_non24 <- final_df %>% 
  filter(med14is24 == 0)

submitted_non24 <- submitted %>% 
  filter(med14is24 == 0)


run_approximate_replica(final_non24)

run_approximate_replica(submitted_non24)





bind_rows(list("new" = final_df, "submitted" = submitted_non24), .id = "df") %>% 
  filter(med14is24 == 0) %>% 
  ggplot() +
  geom_bar(aes(alliancematch2, colour = df))


final_non24 %>% 
  anti_join(submitted_non24, c("state", "dist_name", "region_type", "alliancematch2"))

submitted_non24 %>% 
  anti_join(final_non24, c("state", "dist_name", "region_type", "alliancematch2"))


bind_rows(list("submitted" = submitted, "final" = final_df), .id = "df") %>% 
  mutate(alliancematch2 = if_else(alliancematch2 == "Yes", 1, 0)) %>% 
  filter(med14is24 == 0) %>% 
  pivot_longer(c(perc_marg, alliancematch2, mean_expec_hrs, median_power_hrs,
                 improvemed1419,
                 new_plants_within_300k),
               names_to = "key_vars") %>% 
  ggplot() +
  geom_freqpoly(aes(value, colour = df)) +
  facet_wrap(~key_vars, scales = "free")



submitted %>% 
  anti_join(final_df, by = c("state", "dist_name", "region_type")) %>% 
  View("Missing in final, present in submitted")


final_df %>% 
  anti_join(submitted, by = c("state", "dist_name", "region_type")) %>% 
  View("Missing in submitted, present in final")


>>>>>>> aeb6709bc1681dfe4b92082f08a0e45b6b1f5515
# Export to State for exact main_fixes
write.dta(final_df, paste0(processed_fp, "erss_", load_infix, ".dta"), 
          version = 10)
