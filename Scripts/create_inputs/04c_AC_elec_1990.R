# R version 3.6.3 (2020-02-29)
# author: Alfonso Martínez Arranz
# 2020-04-13 18:36:03
## Processing the full election dataset to obtain longitudinal variables
## such as number of party changes

library(cptools)

a_full <- read_csv("Data/AC_elec_full.csv", 
                         col_types = cols(partyname = "c"))

# Apart from INC, other parties are more difficult to trace back
# the economic policy of India also changed significantly after
# the demise of the Communist bloc
fall_berlin_wall <- 1989 

b_pre_bifurc <- a_full %>%  
  filter(year > fall_berlin_wall) %>% 
  # Bihar Feb 2005 election was repeated in Nov 2005
  filter(!(st_name == "Bihar" & year == 2005.0)) %>% 
  mutate(year = as.integer(year))


check_acnos_by_yr_w_fun <- function(df, FUN, cols) {
  
  df %>%
    select(all_of(cols), year) %>%
    split(df$year, drop = TRUE) %>%
    map(~select(.x, -year)) %>% 
    map(FUN, cols) %>%
    compact() %>%
    bind_rows(.id = "year")
  
}

normal_cols <- c("st_name", "ac_no")

b_pre_gaps <- check_acnos_by_yr_w_fun(b_pre_bifurc, find_acno_incomplete,
                                      normal_cols)

b_pre_dupes <- b_pre_bifurc %>% 
  distinct(st_name, year, ac_no, ac_name) %>% 
  check_acnos_by_yr_w_fun(duplicheck, normal_cols)


# In the original data, Karnataka-174 and -218 AC NO had a "smear" over 
# to KA-175 and KA-219.
rcsv(original_dupes_to_be_removed)

b_pre_corrected <- b_pre_bifurc %>% 
  anti_join(original_dupes_to_be_removed)

# ST_NAME       year 
# 1 Uttar Pradesh 1991 \\ 383, 393, 394, 396-398
# 2 Rajasthan     1993 \\ 77
# 3 Uttar Pradesh 1993 \\ 233, 279, 394 
# 4 Assam         1996 \\ 52, 97
# 5 Uttar Pradesh 1996 \\ 385
# 6 Uttarakhand   2007 \\ 59
# 7 Delhi         2008 \\ 39
# 8 Karnataka     2018 \\ 173


# Adapt to the bifurcations that took place in 2000----------------------------
# Done to allow mergers with both AC_map_pre08 and CP. 
# Uttarakhand is not attempted because there was a delimitation
# that followed its bifurcation.

bifurcation_year <- 2000

un_bifurcate <- function(elec_df, fix_df_str, start_year, end_year) {

  fix_df <- read_csv(paste0("Data/Checks/fix_bifurcations/", 
                            fix_df_str,
                            "_fixedupes.csv"), col_types = cols()) %>% 
    select(-ac_name, -contains("type"), -contains("clue")) 
  
  
  out <- elec_df %>%
    left_join(fix_df, by = c("st_name" = "from_st_name", 
                             "ac_no" = "from_ac_no")) %>% 
    mutate(
      alt_st_name = if_else(!is.na(to_state) & between(year, 
                                                   start_year,
                                                   end_year), 
                        to_state, 
                        alt_st_name),
      alt_ac_no = if_else(!is.na(to_ac_no) & between(year, 
                                                 start_year,
                                                 end_year), 
                      to_ac_no, 
                      alt_ac_no)
      ) %>%
    select(-to_state, -to_ac_no)
  

}

delim_year <- 2007 # dplyr::between is inclusive at both ends

# Need to retrospectively assign bifurcated ACs their 
# new state name and sequences, e.g. call parts of MadhyaP "Chhattisgarh" 
# even prior to 2000, and for reverting the rump state's AC NO 
# to pre-bifurcation sequences
# order is important: first rename bifurcated; then renumber old ACs
c_post_bifurc <- b_pre_corrected %>% 
  # Changes need to be cumulative so they have to apply to same two 
  # columns
  mutate(alt_st_name = st_name,
         alt_ac_no = ac_no) %>% 
  un_bifurcate("bi_jh_corresp", 
               start_year = fall_berlin_wall, 
               end_year = bifurcation_year) %>% 
  un_bifurcate("ch_mp_corresp", 
               start_year = fall_berlin_wall, 
               end_year = bifurcation_year) %>% 
  un_bifurcate("new2old_bihar", 
               start_year = bifurcation_year+1, # Election in Bihar
               end_year = delim_year) %>% 
  un_bifurcate("new2old_madhya", 
               start_year = bifurcation_year,
               end_year = delim_year) %>% 
  un_bifurcate("new2old_up", 
               start_year = bifurcation_year,
               end_year = delim_year) %>% 
  select(st_name, ac_no, contains("alt_"), everything())

# Checking for completeness across all years-----------------------------------

altCols = c("alt_st_name", "alt_ac_no")

c_post_gaps <- check_acnos_by_yr_w_fun(c_post_bifurc, 
                                       FUN = find_acno_incomplete, 
                                       cols = altCols)

c_post_dupes <- c_post_bifurc %>% 
  distinct(alt_st_name, year, alt_ac_no, ac_name) %>% 
  check_acnos_by_yr_w_fun(FUN = duplicheck, cols = altCols)

stopifnot(nrow(c_post_dupes) == 0) 

# Most gaps are due to missing original data
# There are 2 gaps in Bihar and 3 gaps in Madhya Pradesh 
# as a result of the bifurcation fixes, which the check above
# translates into:

c_gaps_diff <- anti_join(c_post_gaps, b_pre_gaps) %>% 
  rename_all(str_to_upper)

stopifnot(nrow(c_gaps_diff) == 25)


# Check for uniqueness of correspondence (was a problem with a mistake
# in the new2old madhya_fixedupes but seems
# solved and no time to look at it now)
# (one old AC_NO to one new AC_NO per year)------------------------------------

# c_post_bifurc %>% 
#   group_by(st_name, year) %>% 
#   (ac_no)
#   
#   
#   mutate(ac_dupes = length(unique(ac_no)) == length(unique(alt_st_name))) %>%
#   ungroup() %>% 
#   distinct(st_name, year, ac_dupes) %>% 
#   View()



c_ivotes_st_names <- c("Telangana", "Bihar", 
                        "Madhya Pradesh", "Rajasthan", 
                        "Chhattisgarh") 

c_missing <- c_post_bifurc %>%
  # Postponed elections in two TN ACs, so data is blank in source
  # No available data on IndiaVotes as of 30 March 2020
  filter(st_name != "Tamil Nadu", !ac_no %in% c(134, 174)) %>% 
  # J&K partynames are missing in original for many states; but
  # except for J&K, they all have partyabbre to compensate
  filter(!is.na(partyname)) %>% 
  # IVotes also has no partyabbre for candidates 
  # (I added for winning ones so no problems here)
  filter(!(st_name %in% c(c_ivotes_st_names, "Jammu & Kashmir",
                          "Jharkhand") & # for IVotes' Bihar after bifurcation fix 
           is.na(partyabbre))) %>% 
  filter(!(st_name == "Maharashtra" & is.na(ac_name))) %>% 
  odd_val_col(state_col_name = "st_name")

  # The IVotes states (Telangana, Bihar, Madhya Pradesh, 
  # Rajasthan, Chhattisgarh)  have no  abbreviations 
  # for parties not in AC_govs02_cleaned. These should not be 
  # any where they have one

stopifnot(nrow(c_missing) == 0)

write_csv(c_post_bifurc, "Data/AC_elec_post1990.csv")


# Check problematic AC NOs before and after bifurcation------------------------

# The exact same rows are missing in secondary fields
# pre_missing <- odd_val_col(b_pre_bifurc) 
# 
# post_missing <- odd_val_col(c_post_bifurc)
# 
# 
# filter_check <- function(df, check_df) {
#   
#   df %>% 
#     semi_join(mutate(check_df, YEAR = as.numeric(YEAR)),
#               by = c("st_name" = "ST_NAME", "ac_no" = 'AC_NO', 
#                      "year" = "YEAR")) 
#   # %>% 
#   # bind_rows(filter(df, st_name %in% c("Jharkhand", "Chhattisgarh",
#   #                                         "Uttarakhand"),
#   #                  between(year, 2000, 2008))) %>% 
#   # distinct(st_name, year, ac_no, ac_name)
#   
#   
# }
# 
# 
# l <- cross2(list(a_full, c_post_bifurc), 
#             list(c_post_dupes, c_gaps_diff))
# 
# d_fix <- map(l, ~elec_check(.x[[1]], .x[[2]]))
# 
# names(d_fix) <- c("original_dupes", "bifur_dupes", "original_gaps",
#                   "bifur_gaps")
# 
# check_path <- "Checks/fix_bifurcations"
#
# Some Uttar Pradesh missing ACs and misleading ac_names led to duplicates
# as a result of  after the reallocation are not reliable;
# I had to rename UP ac_name Lakhna to Lakhana and add Shikarpur 
# election results from 1997 postponed elections
# 
# d_fix[["original_dupes"]] %>% 
#   wcsv(check_path)
# 
# write_csv(paste0("Data/", check_path, "/bifur_gaps.csv"))
# 

# Add partynames for missing variables and select only post-XXXX elections----

# Fixing partynames at this stage creates all sorts of problems, 
# best to fix them at the next step 05 AC_elec
# when only those in government are included
# however, J&K parties are not listed in partyabbre 
# so partyname still needs to be passed along


