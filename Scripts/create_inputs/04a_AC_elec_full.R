## R 3.5.2
## Alfonso Martinez Arranz # Last updated: 1 August 2019
## MEMSI-India
## Adding post-2015 data absent from IndiaVotes.com and
## from Electoral College India
## and merging with Harvard Dataverse dataset (prior to ~2015)

library(cptools)

# Extracting and merging verified IndiaVotes.com data from csv files -----

# Since AC names are not sufficiently reliable, 
# 1. pre-verified winning candidate name and party in "checked" csv files 
# per state 
# 2. import those files into "only_winners" objects, 
# 3. match those winner details with the same in "Detailed" files 
# and incorporate the other candidates into "full" objects, so that 
# calculations of margin and others can be carried out.


# Read the winner files
add_elec_path <- "Original/DS_elections/Latest_elections/"

A0_IVotes_st_names <- c("Telangana", "Bihar", 
                        "Madhya Pradesh", "Rajasthan", 
                        "Chhattisgarh") 

names(A0_IVotes_st_names) <- A0_IVotes_st_names

A0_IVotes_only_winners_raw <- map_dfr(A0_IVotes_st_names,   # 884 obs 18 vars
                                      ~read_csv(paste0(add_elec_path, 
                                                       "IndiaVotes_", 
                                                       .x, 
                                                       "AC_checked.csv"), 
                                                col_types = cols()), 
                                      .id = "st_name")


A0_IVotes_only_winners <- A0_IVotes_only_winners_raw %>%    # 884 obs 9 vars  
  select(st_name, ac_name = `AC Name`, ac_no = `AC No.`, ac_type = Type, 
         cand_name = `Winning Candidate`, partyname = Party,
         electors = `Total Electors`, partyabbre, Margin)


# Read all the full files

A0_IVotes_full_filenames <- unlist(map(A0_IVotes_st_names, 
                                       ~list.files(paste0(add_elec_path, 
                                                          .x, 
                                                          "_Detailed_IndiaVotes"),
                                                   full.names = TRUE)))
# List of 882 elements
A0_IVotes_full_files <- map(A0_IVotes_full_filenames, 
                            ~read_csv(.x, col_types = cols()))   


# Assign ac_no to individual AC dataframes (imported from .csv files)
# by matching candidate names and party names

assign_acno <- function(ac_list, state) {
  
  # Extract the relevant info
  
  extract_acno <- function(ac, list_name) {
    
    if(!all(c("Name","Position","Party", "Votes") %in% names(ac))) 
    {browser()} # Capture poorly formatted input files
    
    input_ac_no <- A0_IVotes_only_winners %>% 
      filter(st_name == state, 
             cand_name == ac$Name[ac$Position == 1],
             partyname == ac$Party[ac$Position == 1], 
             Margin == (ac$Votes[ac$Position == 1] - 
                          ac$Votes[ac$Position == 2])) %>% 
      pull(ac_no)
    
    if(length(input_ac_no) != 1) { 
      
      # capture those which throw an error below on number of rows for debugging
      
      print(ac$Name[ac$Position == 1])
      print(ac$Party[ac$Position == 1])
      print(list_name)
      print(input_ac_no)
      
      browser()
    }
    
    ac %>% 
      mutate(ac_no = input_ac_no,
             st_name = state) %>% 
      select(
        st_name,
        ac_no,
        cand_name = Name,
        partyname = Party,
        totvotpoll = Votes
      )
    
  }
  
  imap(ac_list, ~extract_acno(.x, .y))
  
}

# Bind individual AC dataframes by rows

# 11732 obs 5 vars
# Using state string to select relevant full file, and to fill in missing 
# name field inside it
A0_IVotes_full <-
  map(A0_IVotes_st_names, 
      ~ assign_acno(
        A0_IVotes_full_files[str_detect(names(A0_IVotes_full_files),
                                        .x)],
        .x)
  ) %>% 
  flatten() %>%           
  bind_rows()

# 11732 obs 9 vars
A1_IVotes_merged <- full_join(A0_IVotes_only_winners, A0_IVotes_full) %>%  
  arrange(st_name, ac_no) %>% 
  filter(!is.na(ac_no)) %>% # Removes two superfluous empty rows for
  # Manohar Thana, Rajasthan and Konta, Chhattisgarh
  fill(ac_name, ac_type, electors) %>% 
  select(st_name, everything(), -Margin)

# Check votes
A1_IVotes_missing <- A1_IVotes_merged %>% filter(is.na(totvotpoll)) # 0 obs


# Add the abbreviations that were previously compiled and fix others

rcsv(st_gov_parties02_by_alliance)


A1_IVotes <- A1_IVotes_merged %>%  # 11732 obs 9 vars
  left_join(st_gov_parties02_by_alliance, 
            by= c("partyname" = "gov_partyname")) %>% 
  mutate(partyabbre = if_else(is.na(gov_partyabbre), partyabbre, 
                              gov_partyabbre)) %>% 
  select(-p_alliance, -class, -gov_partyabbre) 

# Extracting and merging official ECI data from csv ----

A2_ECI_raw <- map(c("Ganesh", "Himachal Pradesh", "Punjab"), # 23726 obs 
                  # 16 vars
                  ~read_csv(paste0(add_elec_path, "ECI/", 
                                   .x, 
                                   "_DetailedResults.csv"), 
                            col_types = cols()
                  )
) %>% 
  bind_rows(.id = "alt_st_name")

A2_ECI_renamed <- A2_ECI_raw %>% # 20374 obs 8 vars
  filter(!State %in% c("Manipur", "Meghalaya", "Nagaland", "Tripura"), 
         !`Party Name` == "NOTA") %>% 
  mutate(State = if_else(is.na(State), alt_st_name, State),
         State = recode(State, 
                        `2` = "Himachal Pradesh", 
                        `3` = "Punjab", 
                        Pondichery = "Puducherry")) %>% 
  select(st_name = State, ac_no = `Constituency No.`, 
         ac_name = `Constituency Name`, 
         cand_name = `Candidate Name`, 
         cand_sex = `Candidate Sex`, 
         partyabbre = `Party Name`,
         totvotpoll = `Total Valid Votes`, 
         electors = `Total Electors`)

# Unclear what this was meant to check
# A1_IVotes_merged %>% 
#   filter(st_name == "Bihar", 
#          ac_no %in% c(125, 142, 148, 149, 151,152, 159) ) %>% 
#   View()


# Add AC type for ECI states from previous years

# State-level election data (from the Harvard Dataverse) 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/26526
A3_Dataverse <- read_tsv("Original/DS_elections/Dataverse_IND_state_w_corrections.tab", 
                         col_types = cols()) %>% 
  mutate(st_name = recode(st_name, 
                          "National Capital Territory Of Delhi" = "Delhi"))



A2_type_ac <- A3_Dataverse %>% # 4120 obs 3 vars
  select(st_name, year, ac_no, ac_type) %>% 
  group_by(st_name, year) %>% 
  nest() %>%
  group_by(st_name) %>% 
  top_n(n = 1, year) %>% 
  unnest() %>% 
  select(-year) %>% 
  distinct()


A2_ECI <- left_join(A2_ECI_renamed, A2_type_ac) # 20374 obs 9 vars


# Two Tamil Nadu ACs had a postponed election until March 2019; 
# at moment of analysis the detailed results are not available
A2_ECI %>% filter(is.na(totvotpoll)) %>% distinct(st_name, ac_no) 

# Processing Puducherry elections -----

A4_Puducherry_aggregate <- read_csv(paste0(add_elec_path, # 60 obs 5 vars
                                           "ECI/Puducherry_AggregateResults_2006_2011.csv"), 
                                    col_types = cols())

A4_Puducherry <- A4_Puducherry_aggregate %>% # 405 obs 10 vars
  full_join(read_csv(paste0(add_elec_path, 
                            "ECI/Puducherry_DetailedResults_2006_2011.csv"), 
                     col_types = cols()))


# Merge ECI and IVotes data, Puducherry elections, and Dataverse dataset ----

AC_elec_full <- full_join(A2_ECI, A1_IVotes) %>% 
  # There is only one state per year of election; 
  # this seemed like an efficient recoding process
  mutate(year = st_name, 
         year = recode(
           year,
           Assam = 2016,
           Bihar = 2015,
           Chhattisgarh = 2018,
           Goa = 2017,
           Gujarat = 2017,
           Karnataka = 2018,
           Kerala = 2016,
           `Madhya Pradesh` = 2018,
           `Himachal Pradesh` = 2017,
           Punjab = 2017,
           Puducherry = 2016,
           Rajasthan = 2018,
           Telangana = 2018,
           `Tamil Nadu` = 2016,
           `Uttar Pradesh` = 2017,
           Uttarakhand = 2017,
           `West Bengal` = 2016
         )) %>% 
  full_join(A3_Dataverse) %>% 
  select(st_name, year, ac_no, ac_name, everything(), 
         -cand_sex, -cand_name) %>% 
  # use grepl because dplyr::filter drops rows when condition evaluates to NA
  filter(!grepl("NOTA", partyabbre))

wcsv(AC_elec_full)
