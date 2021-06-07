# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

## Reassigning region; separated from main CPACUR_cen_geo section


reassign_region <- function(df, fixed_file) {
  
  # Join problems with DIST11 fixed here may be fixed by joining combined_cen11_gadm map early on 
  # a but perhaps better restricted to census assignment

  joinvars <- c("STATE", 'DISTRICT_NAME', "AC_NO")
  
  
  # Import fixed file (STATE, DISTRICT_NAME, NEW_REGION_TYPE, AC_NO)
  fixed_df <- read_csv(fixed_file)
  
  # Process from raw fixed file
  fixed_df_longform <- fixed_df %>% 
    {if (any(grepl(";", .$AC_NO))) mutate(., AC_NO = strsplit(AC_NO, ";")) else . } %>% 
    unnest(AC_NO) %>%  # as a result of strsplit
    mutate_if(is.character, str_squish) %>% 
    mutate(AC_NO = as.numeric(AC_NO))
    
  
  # Reassigning REGION_TYPES and removing 
  # (a) all CP vars because those to be reassigned currently have inherited wrong info
  # (b) all AC vars to facilitate adding new ones
  
  CP_vars <- colnames(CP_dist)[!colnames(CP_dist) %in% 
                                 c("STATE", "DISTRICT_NAME",
                                   "REGION_TYPE")]
  
  df_AC_UR_reassigned <- full_join(df, fixed_df_longform, 
                                   by = c("STATE", "DISTRICT_NAME", 
                                          "AC_NO")) %>%
    mutate(REGION_TYPE = if_else(!is.na(NEW_REGION_TYPE), 
                                 as.character(NEW_REGION_TYPE), 
                                 as.character(REGION_TYPE))) %>% 
    select(STATE, DISTRICT_NAME, REGION_TYPE, AC_NO) %>%
    # Filter below removes rows which had CP variables only. 
    # These were created due to the REGION_TYPE mismatches and are 
    # compensated for further below
    filter(!is.na(AC_NO)) 
  
  # Adding CP variables to reassigned ACs
  df_CP_AC_rejoin_UR <- df_AC_UR_reassigned %>% 
    left_join(CP_dist) %>% 
    filter(!is.na(HRS_ACCESS)) %>% 
    left_join(AC_elec_UR_df, 
              by = joinvars) %>% 
    select(-UR_TYPE) %>% 
    distinct()
  

  df_missing <- check_missing(df_CP_AC_rejoin_UR)  

  
  if (!is.null(df_missing)) {
    
    df_CP_AC_recovered <- df_missing %>% 
      select(STATE, DISTRICT_NAME, AC_NO) %>% 
      left_join(fixed_df_longform) %>%  
      left_join(AC_elec_UR_df, 
                by = joinvars) %>%  # Adds AC variables
      select(STATE, DISTRICT_NAME, AC_NO, REGION_TYPE = NEW_REGION_TYPE, 
             everything(), -UR_TYPE) %>% 
      left_join(CP_dist)   # Adds CP variables 
    
    
    # This seems to work... but I'm not sure how
    df_mismatch <- mismatch(CP_dist$DISTRICT_NAME,
                            df_CP_AC_rejoin_UR$DISTRICT_NAME)
    
    # Binding them to the remainder of the dataset
   df_CP_AC_rejoin_UR %>% 
      # Remove the rows that will be bound below  
      filter(!(DISTRICT_NAME %in% df_mismatch$DISTRICT_NAME)) %>%  
      bind_rows(df_CP_AC_recovered) %>% 
     distinct()
  }
  
  else {
    
    df_CP_AC_rejoin_UR
    
  }
  
}
