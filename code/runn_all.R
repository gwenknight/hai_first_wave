###### RUNNN

### Simulation function
## Calculate the contribution of nosocomial cases to the overall epidemic in a single Trust

run_hosp_traj <- function(trusts_in, location_store, los_data, nsims, disch_time, so_scen = 1){
  ## Input
  # trusts = which trusts / settings to run the analysis on 
  # location_store = folder to store in
  # los_data = los data
  
  ## Conditions 
  onset_cutoffs <- c(5,8,15) # threshold for noso definition
  
  # number of simulations
  df_noso_input <- read_csv(paste0("output_all_trusts/total_numbers/",trusts_in[1],"_noso_numbers_by_trust.csv"))[,c(-1)] # just first for number of simulations
  n_sims = min(nsims, max(df_noso_input$sample))
  
  #### Read in data
  p1_s_v <- read.csv("data/200_mean_inc_dist.csv")[,-1] # incubation period for each simulation
  p2_s_v <- read.csv("data/200_sd_inc_dist.csv")[,-1] # incubation period for each simulation
  
  prop_miss_hosp_v <- read.csv("data/200_miss_hosp.csv")[,-1] # proportion missed to hospital for each simulation
  prop_comm_hosp_v <- read.csv("data/200_comm_hosp.csv")[,-1] # proportion community to hospital for each simulation 
  
  #### Changing transmission
  rt_orig <- read.csv("data/rt.csv")
  rs <- c(0.8,1.2) # r constant values
  
  #### Nosocomial numbers
  #df_noso_eg <- read.csv("output_all_trusts/noso_numbers_by_trust.csv")
  
  ## Weeks to run through
  weeks = c(1, max(los_data$week))
  
  
  ######************************************************************************************************************************************************
  ######*** RUN: DISCH TIME 5 *********************************************************************************************************************************************
  ######************************************************************************************************************************************************
  
  ## Loop through all the conditions needed to explore 
  for(iii in trusts_in){ # places
    
    df_noso_input <- read_csv(paste0("output_all_trusts/total_numbers/",iii,"_noso_numbers_by_trust.csv"))[,-1]
    
    ## Check no 0 factor_missed making 0 n_missed: 
    w<-which(is.na(df_noso_input$n_missed))
    if(length(w)>0){
      df_noso_input[w,c("prop_detect","n_missed","n_ad")] <- 0
    }
    
    for(kkk in onset_cutoffs){ # onset cutoffs
      store_this <- c()
      
      for(hhh in 1:(1+length(rs))){ # r values: 3 constant, last is rt estimate
        print(c("RUN?",iii,kkk,hhh)) 
        
        # real time r? yes if hh < 3
        ifelse(hhh < (1+length(rs)), rt_in <- rs[hhh], rt_in <- rt_orig)
        ifelse(hhh < (1+length(rs)), hh_lab <- rt_in,   hh_lab <- "rt")
        
        # los  
        prob_los_o <- los_data %>% filter(place == iii) 
        
        # Nosocomial cases 
        df_noso <- df_noso_input %>% filter(cutoff == kkk)
        
        ## run model many times to get variation - not actually that much - add on top of the sampling variation from LoS and incubation period
        for( rrg in 1:n_sims){
          print(rrg)
          
          df_nosos <- df_noso %>% filter(sample == rrg)
          # incubation period for this simulation 
          params$time_inf_to_symp_mean <- p1_s_v[rrg]
          params$time_inf_to_symp_sd <- p2_s_v[rrg]
          
          # proportion to hospital for this simulation 
          params$prop_miss_hosp <- prop_miss_hosp_v[rrg]
          params$prop_comm_hosp <- prop_comm_hosp_v[rrg]
          
          # SAMPLE LOS 
          los_sample <- prob_los_o %>% filter(sample == rrg)
          
          if(sum(df_nosos$n_noso_o)> 0){ # If there are any nosocomial cases
            #### Run model 
            p <- perc_nosocomial_trust_week(df_nosos, params, los_sample, rt_in, disch_time, so_scen = so_scen)
            
            ## STORE
            store_this <- rbind(store_this, 
                                cbind(rrg,p,hh_lab,paste(iii,kkk,hh_lab,sep="_")))
            
          }
        }
      }
      
      write.csv(store_this, paste0(location_store, "/sims",disch_time,"/",iii,"_",kkk,"_store_trust_eg_detect_",disch_time,"dayprior.csv"))
      
    }
    #store <- rbind(store, store_this)
    
  }
  
  
  
  # ######************************************************************************************************************************************************
  # ######*** RUNNN NO DELAY *********************************************************************************************************************************************
  # ######************************************************************************************************************************************************
  # 
  # ## Loop through all the conditions needed to explore
  # for(iii in trusts){ # places
  #   
  #   df_noso_input <- read_csv(paste0("output_all_trusts/total_numbers/",iii,"_noso_numbers_by_trust.csv"))[,-1]
  #   
  #   for(kkk in onset_cutoffs){ # onset cutoffs
  #     
  #     store_this <- c()
  #     
  #     for(hhh in 1:length(rs)){ # r values: 3 constant, last is rt estimate
  #       print(c("RUN?",iii,kkk,hhh))
  #       
  #       # real time r? yes if hh < 3
  #       ifelse(hhh < 3, rt_in <- rs[hhh], rt_in <- rt_orig)
  #       ifelse(hhh < 3, hh_lab <- rt_in,   hh_lab <- "rt")
  #       
  #       # los
  #       prob_los_o <- los_data %>% filter(place == iii)
  #       
  #       # Nosocomial cases
  #       df_noso <- df_noso_input %>% filter(cutoff == kkk)
  #       
  #       ## run model many times to get variation
  #       for( rrg in 1:n_sims){
  #         print(rrg)
  #         
  #         df_nosos <- df_noso %>% filter(sample == rrg)
  #         
  #         if(sum(df_nosos$n_noso_o)> 0){ # If there are any nosocomial cases
  #           #### Run model
  #           p <- perc_nosocomial_trust_week(df_nosos, params, prob_los_o, rt_in, 1, so_scen = so_scen)
  #           
  #           ## STORE
  #           store_this <- rbind(store_this,
  #                               cbind(rrg,p,hh_lab,paste(iii,kkk,hh_lab,sep="_")))
  #         }
  #       }
  #     }
  #     write.csv(store_this, paste0(location_store, "/sims1/",iii,"_",kkk,"_store_trust_eg_detect_1dayprior.csv"))
  #   }
  #   #store <- rbind(store, store_this)
  # }
  # 
  
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
  # 
  # ###### Grab all together 
  df <- list.files(path=paste0(location_store,"/sims",disch_time), full.names = TRUE) %>% #,pattern = paste0(trusts_in,"_"), full.names = TRUE) %>%
    lapply(fread) %>%
    bind_rows
  store1 <- df
  store1$disch_time <- disch_time
  
  store1$detect_date <- as.Date(store1$detect_date)
  store1 <- store1[,-c(1,5)]
  colnames(store1) <- c("sim_id",names,"r","run_id","disch_time")
  
  #### Return the data
  return(store1)
}

