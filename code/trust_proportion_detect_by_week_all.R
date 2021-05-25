#### Calculation of proportion detected by Trust by week 

### use nosocomialdetection_functions_v2.R to 
### combine los distribution with a constant hazard
### to calculation the proportion detected in each Trust by week 

setwd(here::here())
library(janitor)
library(foreach)
library(doParallel)
library(tidyverse)
numCores <- detectCores() - 2
numCores
registerDoParallel(numCores)

### PARAMETERS
# Functions to calculate proportion detected
source(file="code/nosocomialdetection_functions_v2.R")

# Probability distribution for LOS 
# will vary by week and Trust
los <- read.csv("data/all_los.csv")[,-1]

trusts <- unique(los$place)

# Max day
maxday<-max(los$los.days)
# Cutoffs for nosocomial definition 
cutoffs <- c(5, 8, 10, 15)

## How many samples? 
nsamps = 200

## Probability distribution for incubation period
#p1 <- time_inf_to_symp_mean 
#p2 <- time_inf_to_symp_sd  

#p1_sd <- time_inf_to_symp_mean_sd #= 0.0640, # Lauer
#p2_sd <- time_inf_to_symp_sd_sd # = 0.0691 # Lauer

# SAMPLE incubation period distribution: just do once
#p1_s_v <- rnorm(200, mean = p1, sd = p1_sd)
#p2_s_v <- rnorm(200, mean = p2, sd = p2_sd)

# save for all other simulations
#write.csv(p1_s_v,"data/200_mean_inc_dist.csv")
#write.csv(p2_s_v,"data/200_sd_inc_dist.csv")

p1_s_v <- read.csv("data/200_mean_inc_dist.csv")[,-1]
p2_s_v <- read.csv("data/200_sd_inc_dist.csv")[,-1]

####*** RUN 
# Run thru each Trust and week
nn <- c()
for(i in trusts){
  nn_trust <- c()
  
  for(wk in 1:53){
    print(c(i,wk))
    los_trust <- los %>% filter(place == i, week == wk) 
    los_trust_values <- rep(los_trust$los.days, los_trust$total_admin)
    
    # SAMPLE LOS distribution: just do once
    slos <- c()
    for(ij in 1:200){
      slos <- rbind(slos,sample(los_trust_values, length(los_trust_values), replace = TRUE))
    }
    
    #for(smple in 1:nsamps){
    nn_trust0 <- foreach (smple=1:nsamps, .combine=rbind) %dopar% {
      nn_trusth <- c()
      print(c(i,wk, smple))
      
      # SAMPLE incubation period distribution
      p1_s <- p1_s_v[smple]
      p2_s <- p2_s_v[smple]
      # Max day
      maxday<-max(los_trust$los.days)
      
      cum_prob_inc <- plnorm(1:maxday,p1_s,p2_s)
      prob_inc <- cum_prob_inc-c(0,cum_prob_inc[1:(maxday-1)])
      #sum(prob_inc * seq(1,maxday,1)) 
      
      # SAMPLE LOS 
      los_sample <- as.numeric(unlist(slos[smple,])) %>% # equally as quick to replicate this at start and sample in
        tabyl()
      colnames(los_sample) <- c("los.days","count","prop")
      los_sample$sample <- smple
      los_sample$week <- wk
      write.csv(los_sample, paste0("data/los/",i,"_200_",wk,"_",smple,"_los_dist.csv"))  
      
      for(c in cutoffs){
        pdd <- nosocomial.detection(los_sample[,c("los.days","prop")], prob_inc, c)
        if(length(pdd)>0){nn_trusth <- rbind(nn_trusth, c(wk,c,pdd$res, pdd$res_bc, pdd$res_ad, smple))
        }else{nn_trusth <- rbind(nn_trusth, c(wk,c,0))}
      }
      nn_trusth
    }
    
    nn_trust <- rbind(nn_trust, nn_trust0)
    
  }
  
  nn_trust <- as.data.frame(nn_trust)
  colnames(nn_trust) <- c("week","cutoff","prop_detect","prop_bc","prop_ad","sample")
  nn_trust$week <- as.numeric(nn_trust$week)
  
  # Expand out to week 1 and to 53: same as first and last values
  minwk <- min(nn_trust$week)
  maxwk <- max(nn_trust$week)
  valmin <- nn_trust %>% dplyr::filter(week == minwk)
  valmax <- nn_trust %>% filter(week == maxwk)
  if(minwk > 1){
    for(ww in (minwk - 1):1){
      valmin$week <- ww
      nn_trust <- rbind(valmin, nn_trust)
    }
  }
  if(maxwk < 53){
    for(ww in (maxwk + 1):53){
      valmax$week <- ww
      nn_trust <- rbind(nn_trust, valmax)
    }
  }
  # visualise
  # nn_trust$prop_detect <- as.numeric(nn_trust$prop_detect)
  # ggplot(nn_trust, aes(x=week, y = prop_detect)) + geom_line(aes(group = cutoff, col = cutoff))
  
  ### Save
  nn_trust <- as.data.frame(nn_trust)
  nn_trust$place = i
  
  ### Save for use in percentage contribution calculation
  write.csv(nn_trust,paste0("output_all_trusts/",i,"_prop_detect.csv"))
  
  nn <- rbind(nn, nn_trust)
}
stopImplicitCluster()

### Output for all settings
nn <- list.files(path="output_all_trusts", pattern = "_prop_detect.csv", full.names = TRUE) %>%
  lapply(fread) %>%
  bind_rows

nn <- as.data.frame(nn[,-1])
colnames(nn) <- c("week","cutoff","prop_detect","prop_bc","prop_ad","sample","place")

### Save for use in percentage contribution calculation
write.csv(nn,"output_all_trusts/trusts_prop_detect.csv")

