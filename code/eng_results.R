### Simulation file 
## Calculate the contribution of nosocomial cases to the overall epidemic in a single Trust

#### DO FOR ENG & ENG1

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

## Ggplot options
theme_set(theme_bw(base_size = 11))


#### Read in data
# Proportion detected per week 
#nn <- read_csv("output_all_trusts/trusts_prop_detect.csv")[,-1]

# Nosocomial cases 
#cocin_clean <- read.csv("data/cocin_clean.csv")

# Los data
los_o <- read.csv("data/all_los.csv")[,-1]
los_data_o <- los_o %>% group_by(week, place) %>% arrange(.,los.days,.by_group = TRUE) %>% ungroup()# sort by los.days to get 0.5 first

# prop COCIN in SUS
prop_cocin_in_sus <- read.csv("output_all_trusts/prop_cocin_in_sus.csv")

## MAIN SIMULATION NEEDS THESE FUNCTIONS
# How long until discharged after infection? 
source("code/time_since_infection_discharge.R")
# Calculation of community transmission 
source("code/transmission_function.R")


# MAIN WORKHORSE Function for percentage contribution 
source("code/perc_contribution_function_trust_week.R")
# parameters
source("code/natural_history_parameters.R")
# Analysis of source output
source("code/analyse_store.R")

######************************************************************************************************************************************************
######*** RUN *********************************************************************************************************************************************
######************************************************************************************************************************************************

# Function to runn 
source("code/runn_all.R")

# 200 simulation values for 
# (a) proportion of missed infections that get hospitalised
# Generate once
#prop_miss_hosp_v <- runif(200,params$prop_miss_hosp_min, params$prop_miss_hosp_max)
#write.csv(prop_miss_hosp_v, "data/200_miss_hosp.csv")

# (b) proportion of community infections that get hospitalised
#prop_comm_hosp_v <- rnorm(200,params$prop_comm_hosp_mean, params$prop_comm_hosp_sd)
#write.csv(prop_comm_hosp_v,"data/200_comm_hosp.csv")[,-1]


los_data <- list.files(path="data/los",pattern='^ENG_200_.*csv', full.names = TRUE) %>%
  lapply(fread) %>%
  bind_rows 
los_data<- los_data[,-1]
los_data$place <- "ENG"
#ggplot(los_data, aes(x=los.days, y = prop, group = interaction(sample, week))) + geom_line(aes(col = week)) + scale_x_continuous(lim = c(0,20))

# 3 scenarios are the symptom onset to hospitalization
store_scn15 <- run_hosp_traj("ENG", "output_eng_scn1", los_data, nsims = 200, disch_time = 5, so_scen = 1)
store_scn15$scen_so <- 1
write.csv(store_scn15,"output_eng_scn1/ENG_scn15_store.csv")

store_scn11 <- run_hosp_traj("ENG", "output_eng_scn1", los_data, nsims = 200, disch_time = 1, so_scen = 1)
store_scn11$scen_so <- 1
write.csv(store_scn11,"output_eng_scn1/ENG_scn11_store.csv")

store_scn25 <- run_hosp_traj("ENG", "output_eng_scn2", los_data, nsims = 200, disch_time = 5, so_scen = 2)
store_scn25$scen_so <- 2
write.csv(store_scn25,"output_eng_scn2/ENG_scn25_store.csv")

store_scn21 <- run_hosp_traj("ENG", "output_eng_scn2", los_data, nsims = 200, disch_time = 1, so_scen = 2)
store_scn21$scen_so <- 2
write.csv(store_scn21,"output_eng_scn2/ENG_scn21_store.csv")

store_scn35 <- run_hosp_traj("ENG", "output_eng_scn3", los_data, nsims = 200, disch_time = 5, so_scen = 3)
store_scn35$scen_so <- 3
write.csv(store_scn35,"output_eng_scn3/ENG_scn35_store.csv")

store_scn31 <- run_hosp_traj("ENG", "output_eng_scn3", los_data, nsims = 200, disch_time = 1, so_scen = 3)
store_scn31$scen_so <- 3
write.csv(store_scn31,"output_eng_scn3/ENG_scn31_store.csv")



# store_scn15 <- as.data.frame(read_csv("output_eng_scn1/ENG_scn15_store.csv")[,-1])
# store_scn11 <- as.data.frame(read_csv("output_eng_scn1/ENG_scn11_store.csv")[,-1])
# store_scn25 <- as.data.frame(read_csv("output_eng_scn2/ENG_scn25_store.csv")[,-1])
# store_scn21 <- as.data.frame(read_csv("output_eng_scn2/ENG_scn21_store.csv")[,-1])
# store_scn35 <- as.data.frame(read_csv("output_eng_scn3/ENG_scn35_store.csv")[,-1])
# store_scn31 <- as.data.frame(read_csv("output_eng_scn3/ENG_scn31_store.csv")[,-1])

analyse_store(store_scn15,15,"output_eng_scn1")
analyse_store(store_scn11,11,"output_eng_scn1")
analyse_store(store_scn25,25, "output_eng_scn2")
analyse_store(store_scn21,21, "output_eng_scn2")
analyse_store(store_scn35,35, "output_eng_scn3")
analyse_store(store_scn31,31, "output_eng_scn3")

store <- rbind(store_scn15, store_scn11, store_scn25, store_scn21, store_scn35, store_scn31)

write_csv(store, "output_eng/both_scn/ENG_store.csv")
#store <- read_csv("output_eng/both_scn/ENG_store.csv")
analyse_store(store,"both","output_eng/both_scn")
