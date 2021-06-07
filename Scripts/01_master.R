#! /usr/bin/Rscript
# author: Alfonso Martínez Arranz
# R version 4.0.3
# 31 May 2021

# Master replication script for Martínez Arranz et al 2021 "The Uneven Expansion
# of Electricity Supply in India", ERSS, Volume 78, August 2021, 102126,
# https://doi.org/10.1016/j.erss.2021.102126

# Setup (values are as appropriate for ERSS 2021 replication) ------


# Please see the README file for some sources that are not freely available most
# notably Consumer Pyramids (CP)

library(cptools) 
library(sf)

input_fp <- "Input_data/"

processed_fp <- "Processed_data/"


if(!dir.exists(processed_fp)) {dir.create(processed_fp)}

# Other versions use maps pre 2008 delimitation
ac_map_type <- "post" 

if(ac_map_type == "pre") {compare_func <- `>`} else {compare_func <- `<=`}

distmap_str <- "gadm" # Later versions use other district maps

st_exclusions <- "not_in_cpw1" # Remove states not in CP Wave 1 (e.g. Sikkim)


known_gaps <- read_csv(paste0(input_fp, "/acno_incomplete.csv"))

# Buffer in years between the CP data we're exploring and the election in the region


buffer_years <- 0
buffer_func <- `-`
if(identical(buffer_func, `-`)) {
  
  buffer_lab <- "-"
  comp_lab <- "<"
  comp_buff <- `<`

  } else {
    
    buffer_lab <- "+"
    comp_lab <- ">"
    comp_buff <- `>`
    
    }

# Method of imputation for missing values in the 
# Options
# Iqtl = by quantiles of the relevant state
# noI = no imputation

impute_method <- "Iqtl"



echo = FALSE
job <- TRUE # TRUE if this entire master file is to be run as local job, 
# otherwise, false, although it doesn't change much for various scripts


# reviewer requested to calculate variables with mean
params <- cross2(c("median", "mean"), c(2014, 2019))


persistent_vars <- c("input_fp", "processed_fp", "wave", "main_aggreg_FUN", "FUN_str", 
                     "files", "job", "echo", "buffer_years", 
                     "buffer_func", "buffer_lab", "distmap_str", "comp_lab",
                     "compare_func", "comp_buff", "impute_method",
                     "ac_map_type", "persistent_vars", "suffix", "st_exclusions",
                     "infix", "known_gaps", "params", 'main_fun') 


rm(list = ls()[!ls() %in% persistent_vars])

# Run loop #######

for (p in params) {
  
    main_fun <- p[[1]]
    main_aggreg_FUN <- match.fun(main_fun)
    suffix <- paste0("_replication_", main_fun)
    
    wave <- p[[2]]
    
    
    infix <- paste0(wave, "_", suffix)
    
    cat("\n ******** 1 Key values:", infix,  ".... \n")
  
    
    rm(list = ls()[!ls() %in% persistent_vars])
    
  # SuppressMessages() captures tidyverse verbose descriptions of import column
  # types, joining variables, etc.

    suppressMessages(source("Scripts/02_CP_wave.R", local = job,
                            echo = echo))

    rm(list = ls()[!ls() %in% persistent_vars])

    suppressMessages(source("Scripts/03_CP_dist.R", local = job,
                            echo = echo))

    rm(list = ls()[!ls() %in% persistent_vars])


    suppressMessages(source("Scripts/04_AC_elec_by_wave.R", local = job,
                                                      echo = echo))

    rm(list = ls()[!ls() %in% persistent_vars])

    suppressMessages(source("Scripts/05_CPAC_geo.R", local = job,
                            echo = echo))
    rm(list = ls()[!ls() %in% persistent_vars])

    suppressMessages(source("Scripts/06_CPAC_elec_UR.R", local = job,
                            echo = echo))
    rm(list = ls()[!ls() %in% persistent_vars])


    suppressMessages(source("Scripts/07_CPACUR_cen.R", local = job,
                            echo = echo))
      rm(list = ls()[!ls() %in% persistent_vars])

    suppressMessages(source("Scripts/08_expec_final.R", local = job,
                            echo = echo))

    rm(list = ls()[!ls() %in% persistent_vars])
    
}


suppressMessages(source("Scripts/09_merge_for_replication_df.R",
                local = job,
                echo = echo))


