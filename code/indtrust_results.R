### Simulation file 
## Calculate the contribution of nosocomial cases to the overall epidemic in a single Trust

#### DO FOR ALL TRUSTS

library(janitor)
library(foreach)
library(doParallel)
library(tidyverse)
numCores <- parallel::detectCores() - 2
numCores
registerDoParallel(numCores)


#### Read in data
# Proportion detected per week 
#nn <- read.csv("output_all_trusts/trusts_prop_detect.csv")[,-1]

# Nosocomial cases 
#cocin_clean <- read.csv("data/cocin_clean.csv")

# Los data
#los_o <- read.csv("data/all_los.csv")[,-1]
#los_data <- los_o %>% group_by(week, place) %>% arrange(.,los.days,.by_group = TRUE) %>% ungroup()# sort by los.days to get 0.5 first

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

trusts <- as.character(unlist(prop_cocin_in_sus %>% filter(!Procode %in% c("ENG","ENG1")) %>% dplyr::select(Procode) %>% unique() ))
## Note: "R1H" "RAL" "RBZ" "RCU" had no nosocomial COVID cases

foreach (tsts=1:length(trusts)) %dopar% {
  #for(tsts in 1:length(trusts)){
  trust <- trusts[tsts]
  # LOS data
  los_data <- list.files(path="data/los",pattern=paste0('^',trust,'_200_.*csv'), full.names = TRUE) %>%
    lapply(fread) %>%
    bind_rows 
  los_data<- los_data[,-1]
  los_data$place <- trust
  
  # scenarios are the symptom onset to hospitalisation
  # store_scn15 <- run_hosp_traj(trust, "output_indtrusts_scn1", los_data, nsims = 50, disch_time = 5, so_scen = 1)
  # store_scn15$scen_so <- 1
  # write_csv(store_scn15,paste0("output_indtrusts_scn1/indtrusts/",trust,"_scn15_store.csv"))
  
  store_scn25 <- run_hosp_traj(trust, "output_indtrusts_scn2", los_data, nsims = 50, disch_time = 5, so_scen = 2)
  store_scn25$scen_so <- 2
  write_csv(store_scn25,paste0("output_indtrusts_scn2/indtrusts/",trust,"_scn25_store.csv"))
  
}

stopImplicitCluster()

names <- c("place", "cutoff", "detect_date", "n_noso_o", 
           "n_all_o", "n_noso", "n_all", "week_detect", "factor_missed", 
           "prop_ad", "week", "prop_detect","n_missed", "n_missed_old","n_ad", "n_missed_disch", "n_missed_ad_disch", 
           "n_miss_hosp_case", "n_miss_hosp_case_ad", "n_miss_hosp_case_in14", 
           "n_miss_hosp_case_in14_ad", "readmin_nc", "readmin_nc_ad", "fstgen_inf", 
           "fstgen_cases", "scdgen_inf", "scdgen_cases", "trdgen_inf", "trdgen_cases", 
           "fthgen_inf", "fthgen_cases", "fvtgen_inf", "fvtgen_cases", "sxtgen_inf", 
           "sxtgen_cases", "svtgen_inf", "svtgen_cases", "fstgen_inf_lo", 
           "fstgen_cases_lo", "scdgen_inf_lo", "scdgen_cases_lo", "trdgen_inf_lo", 
           "trdgen_cases_lo", "fthgen_inf_lo", "fthgen_cases_lo", "fvtgen_inf_lo", 
           "fvtgen_cases_lo", "sxtgen_inf_lo", "sxtgen_cases_lo", "svtgen_inf_lo", 
           "svtgen_cases_lo", "fstgen_inf_hi", "fstgen_cases_hi", "scdgen_inf_hi", 
           "scdgen_cases_hi", "trdgen_inf_hi", "trdgen_cases_hi", "fthgen_inf_hi", 
           "fthgen_cases_hi", "total_noso_acq_detect", "total_noso_link_detect", 
           "total_noso_link_detect_lo", "total_noso_link_detect_hi", "perc_noso_acq", 
           "perc_noso_linked", "perc_noso_linked_lo", "perc_noso_linked_hi"
)

###### Look at two discharge dates
disch_time <- 5
df <- list.files(path=paste0("output_indtrusts_scn1","/sims",disch_time), full.names = TRUE) %>% 
  lapply(fread) %>%
  bind_rows
store15 <- df
store15$disch_time <- disch_time
store15$detect_date <- as.Date(store15$detect_date)
store15 <- store15[,-c(1,5)]
colnames(store15) <- c("sim_id",names,"r","run_id","disch_time")
write_csv(store15, "output_indtrusts_scn1/indtrusts_scn15_store.csv")

disch_time <- 5
df <- list.files(path=paste0("output_indtrusts_scn2","/sims",disch_time), full.names = TRUE) %>% 
  lapply(fread) %>%
  bind_rows
store25 <- df
store25$disch_time <- disch_time
store25$detect_date <- as.Date(store25$detect_date)
store25 <- store25[,-c(1,5)]
colnames(store25) <- c("sim_id",names,"r","run_id","disch_time")
write_csv(store25, "output_indtrusts_scn2/indtrusts_scn25_store.csv")


#store_scn15 <- as.data.frame(read_csv("output_indtrusts_scn1/indtrusts_scn15_store.csv")[,-1])
#store_scn25 <- as.data.frame(read_csv("output_indtrusts_scn1/indtrusts_scn11_store.csv")[,-1])


############ Aggregate over Trusts: 
###### want to report this not the individual Trust level variation 
# when pivot wider: makes alphabetical so missing some in the combine! 
columns_to_keep <- c("fstgen_cases", "fstgen_cases_hi", "fstgen_cases_lo", 
                     "fstgen_inf", "fstgen_inf_hi", "fstgen_inf_lo", "fthgen_cases", 
                     "fthgen_cases_hi", "fthgen_cases_lo", "fthgen_inf", "fthgen_inf_hi", 
                     "fthgen_inf_lo", "fvtgen_cases", "fvtgen_cases_lo", "fvtgen_inf", 
                     "fvtgen_inf_lo", "n_ad", "n_all", "n_all_o", "n_miss_hosp_case", 
                     "n_miss_hosp_case_ad", "n_miss_hosp_case_in14", "n_miss_hosp_case_in14_ad", 
                     "n_missed", "n_missed_ad_disch", "n_missed_disch", "n_noso", 
                     "n_noso_o", "readmin_nc", "readmin_nc_ad", "scdgen_cases", "scdgen_cases_hi", 
                     "scdgen_cases_lo", "scdgen_inf", "scdgen_inf_hi", "scdgen_inf_lo", 
                     "svtgen_cases", "svtgen_cases_lo", "svtgen_inf", "svtgen_inf_lo", 
                     "sxtgen_cases", "sxtgen_cases_lo", "sxtgen_inf", "sxtgen_inf_lo", 
                     "total_noso_acq_detect", "total_noso_link_detect", "total_noso_link_detect_hi", 
                     "total_noso_link_detect_lo", "trdgen_cases", "trdgen_cases_hi", 
                     "trdgen_cases_lo", "trdgen_inf", "trdgen_inf_hi", "trdgen_inf_lo")


#### For 15 
location_store <- "output_indtrusts_scn1"

files <- list.files(path=paste0(location_store,"/indtrusts"),pattern = "_scn15", full.names = TRUE)
length(files)

ta <- c()
for(i in 1:length(files)){
  print(i)
  t1 <- fread(files[i])
  if(length(t1) > 1){
    t1$detect_date <- as.Date(t1$detect_date)
    t1 <- t1 %>% dplyr::select(sim_id, place, cutoff, detect_date, n_noso_o:week_detect, n_missed:total_noso_link_detect_hi, r, disch_time:scen_so)
    
    ta <- rbind(ta,t1) %>%
      pivot_longer(cols = all_of(columns_to_keep)) %>%
      group_by(sim_id, cutoff, detect_date, week_detect, r, disch_time, scen_so, name) %>%
      summarise(sum = sum(value)) %>%
      pivot_wider(names_from = name, values_from = sum)
    
  }
}
ta$place <- "grouped"
write.csv(ta, "output_indtrusts_scn1/grouped_15.csv")
analyse_store(ta,"grouped15","output_indtrusts_scn1")

#### For 25 
location_store <- "output_indtrusts_scn2"

files <- list.files(path=paste0(location_store,"/indtrusts"),pattern = "_scn25", full.names = TRUE)
length(files)

ta <- c()
for(i in 1:length(files)){
  print(i)
  t1 <- fread(files[i])
  if(length(t1) > 1){
    t1$detect_date <- as.Date(t1$detect_date)
    t1 <- t1 %>% dplyr::select(sim_id, place, cutoff, detect_date, n_noso_o:week_detect, n_missed:total_noso_link_detect_hi, r, disch_time:scen_so)
    
    ta <- rbind(ta,t1) %>%
      pivot_longer(cols = all_of(columns_to_keep)) %>%
      group_by(sim_id, cutoff, detect_date, week_detect, r, disch_time, scen_so, name) %>%
      summarise(sum = sum(value)) %>%
      pivot_wider(names_from = name, values_from = sum)
    
  }
}
ta$place <- "grouped"
write.csv(ta, "output_indtrusts_scn2/grouped_25.csv")
analyse_store(ta,"grouped25","output_indtrusts_scn2")

