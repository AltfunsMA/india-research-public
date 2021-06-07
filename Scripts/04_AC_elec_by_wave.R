# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

## Calculating the election data relevant for each wave taking into account
## the different AC maps available

library(cptools)

cat("\n ******** 4 Selecting elections by wave.... \n")


# SET-UP ------------------------------------------


out_path <- paste0(processed_fp, "/ACW2014_elec.csv")

if(file.exists(out_path)) {
  
  cat("Already processed \n")
  
}  else {

# From AC_add_elec.R with all new years
a1_original <- read_csv(paste0(input_fp, "AC_elec_post1990.csv"), 
                        col_types = cols(partyname = "c")) %>% 
  exclude_states(st_exclusions)


if(ac_map_type %in% c("pre", "post")) {
  delimitation_year <- 2008
  
  relevant_years <- a1_original %>% 
    select(year) %>% 
    filter(compare_func(delimitation_year, year))
  
  cat("Map is", ac_map_type, "-delimitation, and years selected are in range", 
      range(unique(relevant_years$year)), "and searching years", comp_lab, 
      buffer_func(wave, buffer_years), "\n")
  
} else { cat("Full range of elections in use. Searching years", comp_lab, 
             buffer_func(wave, buffer_years), "\n") }


### Removing duplication of abbreviations-------------------------------------- 
### and introducing some subtle differences in naming  

suppressWarnings({
a2_abbrev <- a1_original %>%
  mutate(partyabbre = fct_collapse(partyabbre,
                                   AAP = "AAAP",
                                   AIADMK = c("ADK", "ADMK", "AIADMK", 
                                              "ADK(JR)", "ADK(JL)"),
                                   INC = c("INC(I)", "INC(U)"),
                                   BJP = c("JP", "JNP"),
                                   IUML = c("IML", "MUL"),
                                   JMM = c("JMM(M)", "JMM(S)"),
                                   # "Kerala Congress" was sometimes called Kerala Congress (Joseph)
                                   # officially, it retained the name and acronym (KEC) and was originally 
                                   # an CPI(M)-LDF ally
                                   # It should be left alone and not listed int he government prior to 2010
                                   # However, KEC joined KC(Mani) in 2010 and the merged group stayed as KC(M) 
                                   # and joined INC-UPA/UDF Kerala Congress (Jacob) has always been a part of 
                                   # INC-UPA/UDF and thus equivalent to KC(M). Kerala Congress (B) seems to be 
                                   # the same as Kerala Congress (Pillai Group), part of INC-UPA/UDF
                                   # Other Kerala Congresses (secular, nationalist) exist but seem to be very 
                                   # small and have no representation
                                   
                                   `KC(M)` = c("KCM", "KEC(M)", "KCJ", "KEC"), 
                                   `CPI(M)` = "CPM",
                                   JKNC = "JKN",
                                   `UKD(P)` = c("UKD", "UKDP")
  ),
  # RSP is not allied with INC in West Bengal but it is in Kerala since 2006
  partyabbre = if_else(st_name == "Kerala", 
                       as.character(recode(partyabbre,
                                           RSP = "RSP(K)",
                                           NCP = "NCP(K)")), 
                       as.character(partyabbre)
  )
  )
})

### Fixing J&K partyabbre entries (can skip for many elections) ------------

# Can't use mutate easily when checking another column 
# for only a subset of cases (i.e. combined with if_else), 
# e.g. extract_acronyms would be sent the entire column, 
# as opposed to the corresponding subset.

extract_acronyms <- function(full_names) {
  
  str_extract_all(full_names, 
                  "([A-Z\\)\\(]+)", simplify = FALSE) %>% 
    # output is list of vectors of capital letters and brackets
    map_chr(~str_c(.x, collapse= "")) %>% 
    recode(I = "IND", S = "SHS") #common abbre for Independents and Shivsena
  
  
}

a3_fixed_acronym <- a2_abbrev %>% 
  mutate(partyabbre = case_when(
    
    is.na(partyabbre) & st_name == "Jammu & Kashmir" 
    ~extract_acronyms(partyname),
    TRUE ~ partyabbre
    
  ))

### Add data on party(ies) in state gov_parties (only for states in CP) -------
# Parties in state governments from 1990 onwards 

rcsv(st_gov_parties_by_year, input_fp) # New

# Put into a mergeable format  
a3_gov_data <- st_gov_parties_by_year %>% 
  group_by(st_name, year) %>%
  mutate(gov_parties = paste0(gov_partyabbre, collapse = ", "),
         gov_alliance = p_alliance) %>%
  dplyr::select(-gov_partyabbre, -gov_partyname, -p_alliance) %>%
  distinct() %>%
  ungroup() %>% 
  arrange(st_name, year)


a4_named <- left_join(a3_fixed_acronym, a3_gov_data, 
                      by = c("st_name", "year"))



### Join party names and party types ------------------------------------------

# Only those parties in at least one gov_parties (as per external file)
# get their own names; all others are marked as NA, 
# which is transformed below to "Others"

a5_partynames <- st_gov_parties_by_year %>% 
  exclude_states(st_exclusions) %>% 
  distinct(gov_partyname, year, gov_partyabbre, p_alliance)




a5_cleaned <- left_join(a4_named, a5_partynames,
                        by = c("partyabbre" = "gov_partyabbre",
                               "year")) 

# a5_missing <- odd_val_col(a5_cleaned)


# Count votes by party, No. of elections and of changes in 
# parties holding each seat, and their percwin


# Otherwise, historical results compare wrong AC NOs on account of delimitation
# changes

filter_years <- function(df) {  
  
  relevant_years <- df %>% 
    {if(ac_map_type != "both") filter(., compare_func(delimitation_year, year)) else . } %>% 
    # to select years before/after relevant dataset
    filter(comp_buff(year, buffer_func(wave, buffer_years)))
  
  
  suppressWarnings(relevant_years %>%
                     group_by(st_name, year) %>% 
                     nest() %>%                # nest everything which is not st_name or year  
                     group_by(st_name) %>%     # We want to have top years by st_names
                     # Picks the "largest" year
                     top_n(n = 1, year) %>%    
                     unnest() %>%
                     ungroup() %>% 
                     droplevels())
  
}


# First winners
a6_winners <- a5_cleaned %>%
  group_by(st_name, year, ac_no) %>%
  mutate(percwin = max(totvotpoll)/sum(totvotpoll),
         vote_diff = nth(totvotpoll, -1,
                         order_by = min_rank(totvotpoll)) -
           nth(totvotpoll, -2,
               order_by = min_rank(totvotpoll)),
         p_percmarg = vote_diff/sum(totvotpoll)) %>%
  # Unless winner is not filtered first here, we get number of candidates below
  filter(totvotpoll == max(totvotpoll)) %>%
  ungroup() %>%
  filter_years() %>% 
  distinct()


# a6_winners %>% 
#   filter(st_name == "Uttar Pradesh", ac_name == "Nawabganj") %>% 
#   select(contains("_name"), contains("_no"), contains("part"), 
#          party_changes, historical_alliance, same_as_history_gov) %>% 
#   View("Test election data calculations")


a6_dupes <- duplicheck(a6_winners)

if(!is.null(a6_dupes)) {
  
  stopifnot(a6_dupes$ALT_AC_NO == c(121, 70))
  
}



a6_composition <- a6_winners %>% 
  distinct(st_name, year)

cpdist <- read_csv(paste0(processed_fp, "CPW", wave, "_dist_", main_fun, ".csv"),
                   col_types = cols()) %>% 
  exclude_states(st_exclusions) %>% 
  {if (with(a6_composition, year[st_name == "Andhra Pradesh"]) < 2014) exclude_states(., 'tel') else .}

stopifnot(length(setdiff(unique(a6_composition$st_name), unique(cpdist$STATE))) == 0)


# If differences in votes are so close to zero, 
# something is extremely peculiar about that district-region
# or there has been a problem upstream

a6_weird_votediff <- a6_winners %>% 
  filter(vote_diff < 2)

if(nrow(a6_weird_votediff) > 10) {a6_weird_votediff %>% 
    select(st_name, year, electors, 
           ac_no, vote_diff, p_percmarg) %>% 
    View(title = "Too many voting differences.")}


# Record the votes for each alliance in one row per AC and left_join -------
# with the party information for winning candidates
#############################

a6_Nr_votes <- a5_cleaned %>%
  filter_years() %>% 
  group_by(st_name, ac_no) %>% 
  mutate(n = totvotpoll) %>% 
  mutate(id = row_number()) %>%
  # ID needed if there is a chance coincidence in the number of votes 
  # for two candidates (frequently Independents coded as "NA"). Otherwise
  # 'spread' does not have a unique set of values
  # to work with (rather than 'unique key' as per the error message)
  pivot_wider(names_from = p_alliance, values_from = n, values_fill = 0) %>%
  select(-id) %>% 
  rename_all(str_to_upper) %>% 
  rename("OTHERS" = "NA") %>% 
  # All but the parties
  summarise(across(!c(YEAR, ELECTORS, TOTVOTPOLL,
                      matches("^gov|^st_|^ac_|^alt_|^party")), 
                   ~sum(., na.rm = TRUE))) %>% 
  rename_all(str_to_lower) %>% # to avoid problems below...
  ungroup()



a7_win_votes <- left_join(a6_winners, a6_Nr_votes, by = c("st_name", "ac_no"))


a7_missing <- a7_win_votes %>% 
  select(-ac_name, -gov_partyname) %>% 
  odd_val_col(verbose = F)

# Non-government parties have to be less than half of all the winners
stopifnot(nrow(a7_missing) < nrow(a7_win_votes)/2)

### Clean-up: Remove unusued variables and non-CP state rows,  ----------------
# recode parties and alliances, formatting names to fit other DS 


a8_clean <- a7_win_votes %>%
  dplyr::select(-totvotpoll) %>%
  mutate(partyname = if_else(is.na(gov_partyname), 
                             partyname, 
                             gov_partyname)) %>% 
  select(-gov_partyname) %>%
  mutate(p_alliance = ifelse(partyabbre == "IND", 
                             "Independent", 
                             as.character(p_alliance))) %>% 
  mutate(partyname = ifelse(partyabbre == "IND", 
                            "Independent", 
                            as.character(partyname))) %>% 
  replace_na(list(partyname = "(NA, see abbreviation)", class = "Regional", 
                  p_alliance = "Others")) %>% 
  mutate(same_as_gov = if_else(str_detect(as.character(gov_parties), 
                                          fixed(as.character(partyabbre))), 
                               TRUE, 
                               FALSE)) %>% 
  rename_all(str_to_upper) %>% # to fit with other datasets
  distinct()



# Calculate the government in power with the highest votes

AC_elec <- a8_clean %>% 
  group_by(ST_NAME, P_ALLIANCE) %>%
  mutate(ALLIANCE_NO_SEATS = n()) %>%  
  arrange(ST_NAME, desc(ALLIANCE_NO_SEATS)) %>%
  select(-ALLIANCE_NO_SEATS) %>% 
  ungroup() %>% 
  group_by(ST_NAME) %>%  
  # Extracts gov_alliance names in the order of P_ALLIANCE, 
  # i.e. higher to lower number of seats by state
  mutate(GOV_ALLIANCE = str_c(na.omit(str_extract(
    unique(GOV_ALLIANCE),
    str_to_upper(unique(P_ALLIANCE))
  )),
  collapse = ", "), 
  GOV_LEAD_ALLIANCE = str_split(GOV_ALLIANCE, ", ")[[1]][[1]]) %>%
  ungroup() %>% 
  distinct()

cat("Final checks \n")
missing_values <- AC_elec %>% 
  select(-AC_NAME) %>% 
  filter(ST_NAME != "Jammu & Kashmir",
         !AC_NO %in% c(47,48)) %>% 
  # Totvotpoll is 0 for Nobra and Leh ACs in original dataset probably due to 
  # some election day issues. That triggers NaN when calculating various things
  #  at a6_winners. Did not attempt fix.
  odd_val_col()

stopifnot(nrow(missing_values) == 0)


missing_states <- setdiff(unique(cpdist$ST_NAME), unique(AC_elec$ST_NAME))

stopifnot(length(missing_states) == 0)

stopifnot(between(range(AC_elec$PERCWIN), 0, 1))

stopifnot(between(range(AC_elec$P_PERCMARG), 0, 1))

gaps <- find_acno_incomplete(AC_elec)


if(!is.null(gaps)) {
  
  if(wave == 2014 && ac_map_type == "post") {
    
    states_w_missing_acno <- gaps %>% 
      exclude_states(st_exclusions) %>% 
      pull(ST_NAME) %>% 
      unique()
    
    # 
    stopifnot(states_w_missing_acno %in% c("Madhya Pradesh", "Gujarat"))
    
  } else (stop("Review gaps"))
  
  
}



# -----

write_csv(AC_elec, out_path)

}

