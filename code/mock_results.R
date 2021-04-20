### Simulation file 
## Calculate the contribution of nosocomial cases to the overall epidemic in a single Trust

## DO FOR MOCK

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

#### Read in data
setwd(here())

# Los data
los_data <- read.csv("data/mock_los.csv")[,-1]

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

# 2 scenarios are the symptom onset to hospitalisation
store_scn15 <- run_hosp_traj("mock", "output_mock_scn1", los_data, nsims = 20, disch_time = 5, so_scen = 1)
store_scn15$scen_so <- 1
write.csv(store_scn15,"output_mock_scn1/mock_scn15_store.csv")
store_scn11 <- run_hosp_traj("mock", "output_mock_scn1", los_data, nsims = 20, disch_time = 1, so_scen = 1)
store_scn11$scen_so <- 1
write.csv(store_scn11,"output_mock_scn1/mock_scn11_store.csv")

store_scn25 <- run_hosp_traj("mock", "output_mock_scn2", los_data, nsims = 20, disch_time = 5, so_scen = 2)
store_scn25$scen_so <- 2
write.csv(store_scn25,"output_mock_scn2/mock_scn25_store.csv")
store_scn21 <- run_hosp_traj("mock", "output_mock_scn2", los_data, nsims = 20, disch_time = 1, so_scen = 2)
store_scn21$scen_so <- 2
write.csv(store_scn21,"output_mock_scn2/mock_scn21_store.csv")

store_scn35 <- run_hosp_traj("mock", "output_mock_scn3", los_data, nsims = 20, disch_time = 5, so_scen = 3)
store_scn35$scen_so <- 3
write.csv(store_scn35,"output_mock_scn3/mock_scn35_store.csv")
store_scn31 <- run_hosp_traj("mock", "output_mock_scn3", los_data, nsims = 20, disch_time = 1, so_scen = 3)
store_scn31$scen_so <- 3
write.csv(store_scn31,"output_mock_scn3/mock_scn31_store.csv")


# store_scn15 <- as.data.frame(read_csv("output_mock_scn1/mock_scn15_store.csv")[,-1])
# store_scn11 <- as.data.frame(read_csv("output_mock_scn1/mock_scn11_store.csv")[,-1])
# store_scn25 <- as.data.frame(read_csv("output_mock_scn2/mock_scn25_store.csv")[,-1])
# store_scn21 <- as.data.frame(read_csv("output_mock_scn2/mock_scn21_store.csv")[,-1])
# store_scn35 <- as.data.frame(read_csv("output_mock_scn3/mock_scn35_store.csv")[,-1])
# store_scn31 <- as.data.frame(read_csv("output_mock_scn3/mock_scn31_store.csv")[,-1])

analyse_store(store_scn15,15,"output_mock_scn1")
analyse_store(store_scn11,11,"output_mock_scn1")
analyse_store(store_scn25,25, "output_mock_scn2")
analyse_store(store_scn21,21, "output_mock_scn2")
analyse_store(store_scn35,35, "output_mock_scn3")
analyse_store(store_scn31,31, "output_mock_scn3")

store <- rbind(store_scn15, store_scn11, store_scn25, store_scn21, store_scn35, store_scn31)
write_csv(store, "output_mock/both_scn/mock_store.csv")
#store <- read_csv("output_mock/both_scn/mock_store.csv")
analyse_store(store,"both","output_mock/both_scn")

