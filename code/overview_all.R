### File structure / libraries / data 

### Libraries needed
library(here)
library(lubridate)
library(tidyverse)
library(reshape2)
library(data.table)
library(ggplot2)
library(dplyr)
library(zoo)
library(patchwork)
library(readr)
library(RColorBrewer)
library(boot)

options(dplyr.summarise.inform=F) ## to stop new ungrouping error

## Ggplot options
theme_set(theme_bw(base_size = 11))

### Set WD
setwd(here::here())


### DATA WRANGLING
# (1) COCIN
source("code/co_cin_clean.R")

# At Trust and Week level need 
# (2) LoS:    From SUS    using first_los.csv 
source("code/sus_los_ind_trust_explore_all.R") 

# (3) Prop detect:    Using missing infections function    
source("code/trust_proportion_detect_by_week_all.R")

# (4) Proportion COCIN in SUS: COCIN and SUS data in here
source("code/trust_level_comparison_cocin_sus_all.R")
# trusts_include2 => remove odd trusts

# Number of nosocomial cases detect and missed per Trust, time series
source("code/trust_number_noso_all.R")

# Rt from Epiforecasts
source("code/rt_extraction.R")


## MAIN SIMULATION NEEDS THESE FUNCTIONS
# How long until discharged after infection? 
source("code/time_since_infection_discharge.R")
# Calculation of community transmission 
source("code/transmission_function.R")


# MAIN WORKHORSE Function for percentage contribution 
source("code/perc_contribution_function_trust_week.R")
# parameters
source("code/natural_history_parameters.R")

## Main simulation here

## ENGLAND
source("code/eng_results.R")

## Independent Trust level
source("code/indtrust_results.R")


# To run for different discharge times 
#source("code/runn.R")
# More recent - looking at variation across simulations and discharge times
#source("code/trust_contribution_of_nosocomial_eg_disch_time_var_all.R")
#source("code/trust_contrib_regional.R")

