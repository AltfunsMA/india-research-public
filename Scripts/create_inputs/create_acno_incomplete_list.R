# Determining missing and odd values in original AC election dataset

full_elec <- read_csv("Processed/02_elections/AC_elec_full.csv", 
                      col_types = cols(partyname = "c"))

# Haryana is the main location without election year
full_elec %>% 
  filter(is.na(year)) %>% 
  View()


full_elec %>% 
  filter(st_name == "Haryana") %>% 
  group_by(st_name, year) %>% 
  summarise(sum(totvotpoll))

full_elec %>% 
  distinct(year) %>% 
  arrange(year) %>% 
  as.data.frame()

full_elec %>% 
  mutate(year = if_else(st_name == "Haryana", 
                        replace_na(year, 1972),
                        year), .keep = "used") %>% 
  View()


full_elec %>% 
  mutate(year = if_else(st_name == "Haryana", 
                        replace_na(year, 1972),
                        year)) %>% 
  write_csv("Processed/02_elections/AC_elec_full.csv")

# Create missing and NA value record ------

elec1990 <- read_csv("Processed/02_elections/AC_elec_post1990.csv", 
                      col_types = cols(partyname = "c")) %>% 
  mutate(year = as.character(year))

missing_keyval <- elec1990 %>% 
  select(st_name, ac_no, year, totvotpoll, electors) %>% 
  odd_val_col(refCols = c('ST_NAME', 'AC_NO', 'YEAR')) %>% 
  select(-TOTVOTPOLL, -ELECTORS)

mimic_isacnocomplete <- rename_with(missing_keyval, tolower) %>% 
  mutate(gap = TRUE)

gap_df <- elec1990 %>%
  distinct(st_name, ac_no, year) %>% 
  semi_join(mimic_isacnocomplete, by = c("st_name", "year")) %>% 
  left_join(mimic_isacnocomplete, by = c('st_name', 'year', 'ac_no')) %>% 
  arrange(st_name, year, ac_no)


v <- which(gap_df$gap == TRUE) 

around_vals <- c(v - 2, v - 1, v, v + 1)

aroundvals_df <- slice(gap_df, around_vals) %>% 
  arrange(st_name, ac_no) %>% 
  select(-gap) %>% 
  distinct() %>% 
  rename_with(toupper)
  
year_list <- unique(elec1990$year)

incomplete <- map(year_list, ~find_acno_incomplete(filter(elec1990, year == .x)))

names(incomplete) <- year_list

incomplete_df <- bind_rows(incomplete, .id = "YEAR")


  
incomplete_df %>% 
  bind_rows(aroundvals_df) %>% 
  mutate(STATE = ST_NAME) %>% 
  write_csv("Processed/02_elections/acno_incomplete.csv")




