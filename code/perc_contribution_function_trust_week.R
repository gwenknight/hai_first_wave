#### FUNCTION to calculate the contribution of hospitals to overall hospital admissions

##### Inputs ****************************
### df_noso = data on number of cases
### params = Natural history parameters
### prob_los_o = length of stay
### rt = r estimates over time for community transmission (can be constant)
### disch_time = how deal with when missing cases are discharged
### so_scen = scenario for symptom onset to hospitalisation 
### ***********

##### FUNCTION FOR CONTRIBUTION OF NOSOCOMIAL TO OVERALL EPIDEMIC

perc_nosocomial_trust_week<- function(df_noso, params, prob_los_o, rt, disch_time, so_scen = 1){
  
  #load params and run simulation
  with(params,{
    
    ## length of each column
    end <- dim(df_noso)[1]
    
    #### Missed infections - assume discharged at disch_time
    df_noso$n_missed_disch <- 0
    df_noso$n_missed_ad_disch <- 0
    # First nosocomial missed case is
    w<-min(which(df_noso$n_missed > 0))
    if(w > disch_time){
      df_noso[1:(end-disch_time+1),"n_missed_disch"] <- df_noso[disch_time:end,"n_missed"]
      df_noso[1:(end-disch_time+1),"n_missed_ad_disch"] <- df_noso[disch_time:end,"n_ad"]
    } 
    
    ### Data adjustments
    # asymptomatic: 20% of those with SO - currently not using
    #df_noso$n_missed_asym <- df_noso$n_missed / 4 # assume 20% asymptomatic
    
    ## When do missed infections become cases? 
    ### _ad = only those after discharge
    df_noso$n_miss_hosp_case <- 0
    df_noso$n_miss_hosp_case_ad <- 0
    df_noso$n_miss_hosp_case_in14 <- 0 # if return in 14 days
    df_noso$n_miss_hosp_case_in14_ad <- 0 # if return in 14 days
    df_noso$readmin_nc <- 0
    df_noso$readmin_nc_ad <- 0

    
    for(i in 1:end){ # dates

      if(df_noso[i,"n_missed_disch"] > 0){
        n_hosp <- sum(rbernoulli(as.numeric(df_noso[i,"n_missed_disch"]),runif(as.numeric(df_noso[i,"n_missed_disch"]),prop_miss_hosp_min, prop_miss_hosp_max)))
      }else{n_hosp = 0} # those infections that get hospitalized for COVID19
      
      prob_los <- as.numeric(unlist(prob_los_o %>% filter(week == as.numeric(df_noso[i,"week"])) %>% dplyr::select(prop)))
      
      if(n_hosp > 0 ){ # if some to be sampled
        times_from_infection_to_discharge <- sample(x=c(0.5,seq(1,length(prob_los)-1,1)),
                                                    size = as.numeric(n_hosp), prob = time.infection.discharge.05(prob_los),  replace = TRUE)
        times_from_infec_to_symp <- rlnorm(as.numeric(n_hosp), time_inf_to_symp_mean,time_inf_to_symp_sd)
        if(so_scen == 1){times_from_symp_to_hosp <- rlnorm(as.numeric(n_hosp), meanlog = time_symp_to_hosp_meanlog, sdlog = time_symp_to_hosp_sdlog)}
        if(so_scen == 2){times_from_symp_to_hosp <- rgamma(as.numeric(n_hosp), shape = time_symp_to_hosp_shape, scale = time_symp_to_hosp_scale)}
        if(so_scen == 3){times_from_symp_to_hosp <- rlnorm(as.numeric(n_hosp), meanlog = time_symp_to_hosp_meanlog3, sdlog = time_symp_to_hosp_sdlog3)}
        times_from_infec_to_hosp <- times_from_infec_to_symp + times_from_symp_to_hosp
        times_from_discharge_to_hosp <- round(pmax(0,times_from_infec_to_hosp - times_from_infection_to_discharge),0)
        time_hosp_case <- table(times_from_discharge_to_hosp)
        if(min(times_from_discharge_to_hosp) == 0){symp_on_dish <- time_hosp_case[1]; time_hosp_case <- time_hosp_case[-1];
        df_noso$n_miss_hosp_case[i] <- as.numeric(symp_on_dish) + df_noso$n_miss_hosp_case[i]}
        w<-which(i + as.numeric(names(time_hosp_case)) <= dim(df_noso)[1]) # cap at dim df_noso
        df_noso$n_miss_hosp_case[i + as.numeric(names(time_hosp_case[w]))] <- as.numeric(time_hosp_case[w]) + df_noso$n_miss_hosp_case[i + as.numeric(names(time_hosp_case[w]))]
        w<-which(as.numeric(names(time_hosp_case)) < 15) # cap at return in 14 days
        df_noso$n_miss_hosp_case_in14[i + as.numeric(names(time_hosp_case[w]))] <- as.numeric(time_hosp_case[w]) + df_noso$n_miss_hosp_case_in14[i + as.numeric(names(time_hosp_case[w]))]
      }
      
      ### BASED ON ONLY THOSE SO / TEST AFTER DISCHARGE
      if(df_noso[i,"n_missed_ad_disch"]>0){
        n_hosp_ad <- sum(rbernoulli(as.numeric(df_noso[i,"n_missed_ad_disch"]),runif(as.numeric(df_noso[i,"n_missed_ad_disch"]),prop_miss_hosp_min, prop_miss_hosp_max))) # those infections that get hospitalized for COVID19
      }else(n_hosp_ad = 0)
      
      if(n_hosp_ad > 0 ){ # if some to be sampled
        times_from_infection_to_discharge <- sample(x=c(0.5,seq(1,length(prob_los)-1,1)),
                                                    size = as.numeric(n_hosp_ad), prob = time.infection.discharge.05(prob_los),  replace = TRUE)
        times_from_infec_to_symp <- rlnorm(as.numeric(n_hosp_ad), time_inf_to_symp_mean,time_inf_to_symp_sd)
        if(so_scen == 1){times_from_symp_to_hosp <- rlnorm(as.numeric(n_hosp_ad), meanlog = time_symp_to_hosp_meanlog, sdlog = time_symp_to_hosp_sdlog)}
        if(so_scen == 2){times_from_symp_to_hosp <- rgamma(as.numeric(n_hosp_ad), shape = time_symp_to_hosp_shape, scale = time_symp_to_hosp_scale)}
        if(so_scen == 3){times_from_symp_to_hosp <- rlnorm(as.numeric(n_hosp), meanlog = time_symp_to_hosp_meanlog3, sdlog = time_symp_to_hosp_sdlog3)}
        times_from_infec_to_hosp <- times_from_infec_to_symp + times_from_symp_to_hosp
        times_from_discharge_to_hosp <- round(pmax(0,times_from_infec_to_hosp - times_from_infection_to_discharge),0)
        time_hosp_case <- table(times_from_discharge_to_hosp)
        if(min(times_from_discharge_to_hosp) == 0){symp_on_dish <- time_hosp_case[1]; time_hosp_case <- time_hosp_case[-1];
        df_noso$n_miss_hosp_case_ad[i] <- as.numeric(symp_on_dish) + df_noso$n_miss_hosp_case_ad[i]}
        w<-which(i + as.numeric(names(time_hosp_case)) <= dim(df_noso)[1]) # cap at dim df_noso
        df_noso$n_miss_hosp_case_ad[i + as.numeric(names(time_hosp_case[w]))] <- as.numeric(time_hosp_case[w]) + df_noso$n_miss_hosp_case_ad[i + as.numeric(names(time_hosp_case[w]))]
        w<-which(as.numeric(names(time_hosp_case)) < 15) # cap at return in 14 days
        df_noso$n_miss_hosp_case_in14_ad[i + as.numeric(names(time_hosp_case[w]))] <- as.numeric(time_hosp_case[w]) + df_noso$n_miss_hosp_case_in14_ad[i + as.numeric(names(time_hosp_case[w]))]
      }
      
      
      
      # Those admitted for other reasons: 14% get emergency readmissions. 90% within 14 days
      # Take to be 14% of those not hospitalised for COVID19 as some of those for C19 would be in the readmissions
      if(df_noso[i,"n_missed_disch"]>0){
        n_readmin_nc <-  prop_readm_nc*sum(rbernoulli(as.numeric(df_noso[i,"n_missed_disch"]),(1 - runif(as.numeric(df_noso[i,"n_missed_disch"]),prop_miss_hosp_min, prop_miss_hosp_max))))
      }else{n_readmin_nc = 0}
      #OLD: as.numeric(round(prop_readm_nc*(1 - runif(1,prop_miss_hosp_min, prop_miss_hosp_max))*df_noso[i,"n_missed_disch"],0))
      
      
      if(n_readmin_nc > 0){
        times_to_readmission <- table(sample(dist_readm_nc[,"day"],n_readmin_nc, replace = TRUE, prob = dist_readm_nc[,"nuff_readm"]))
        w <- which(i + as.numeric(names(times_to_readmission)) <= dim(df_noso)[1])
        df_noso$readmin_nc[i + as.numeric(names(times_to_readmission[w]))] <- as.numeric(times_to_readmission[w]) + df_noso$readmin_nc[i  + as.numeric(names(times_to_readmission[w]))]
      }
      
      # Those admitted for other reasons: 14% get emergency readmissions. 90% within 14 days
      # Take to be 14% of those not hospitalised for COVID19 as some of those for C19 would be in the readmissions
      if(df_noso[i,"n_missed_ad_disch"]>0){
        n_readmin_nc_ad <-  prop_readm_nc*sum(rbernoulli(as.numeric(df_noso[i,"n_missed_ad_disch"]),(1 - runif(as.numeric(df_noso[i,"n_missed_ad_disch"]),prop_miss_hosp_min, prop_miss_hosp_max))))
      }else(n_readmin_nc_ad = 0)
      # OLD: as.numeric(round(prop_readm_nc*(1 - runif(1,prop_miss_hosp_min, prop_miss_hosp_max)) *df_noso[i,"n_missed_ad_disch"],0))
      if(n_readmin_nc_ad > 0){
        times_to_readmission <- table(sample(dist_readm_nc[,"day"],n_readmin_nc, replace = TRUE, prob = dist_readm_nc[,"nuff_readm"]))
        w <- which(i + as.numeric(names(times_to_readmission)) <= dim(df_noso)[1])
        df_noso$readmin_nc_ad[i + as.numeric(names(times_to_readmission[w]))] <- as.numeric(times_to_readmission[w]) + df_noso$readmin_nc_ad[i  + as.numeric(names(times_to_readmission[w]))]
      }
      
    }
    
    ## check when prop_miss_hosp = 1 that these are the same
    #sum(df_noso$n_missed)
    #sum(df_noso$n_miss_hosp_case)
    
    ##### TRANSMISSION
    if(length(rt) != 1){
      if(!"week" %in% colnames(rt)){
        w <- which(as.Date(rt$date,format = c("%Y-%m-%d")) == 
                     min(as.Date(df_noso$detect_date)));
        rtu <- rt[w:(dim(rt)[1]),c("median","lower_50","upper_50")]}
      else{
        w<-which(rt$week == min(df_noso$week))
        rtu <- rt[w:length(rt[,1]),"mrt"]
      }
    } else{
      rtu <- as.data.frame(matrix(0,1,3)); colnames(rtu) <- c("median","upper_50","lower_50")
      rtu[1,c("median","upper_50","lower_50")] <- c(rt, 1.05*rt, 0.95*rt)
    }
    
    fstgen <- gen_trans(df_noso$n_missed*we_inf, rtu$median, so_scen = so_scen)
    df_noso$fstgen_inf <- as.numeric(fstgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fstgen_cases <- as.numeric(fstgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    scdgen <- gen_trans(df_noso$fstgen_inf*we_inf, rtu$median, so_scen = so_scen)
    df_noso$scdgen_inf <- as.numeric(scdgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$scdgen_cases <- as.numeric(scdgen$n_trans_cases[1:length(df_noso$detect_date)])
  
    trdgen <- gen_trans(df_noso$scdgen_inf*we_inf, rtu$median, so_scen = so_scen)
    df_noso$trdgen_inf <- as.numeric(trdgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$trdgen_cases <- as.numeric(trdgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fthgen <- gen_trans(df_noso$trdgen_inf*we_inf, rtu$median, so_scen = so_scen)
    df_noso$fthgen_inf <- as.numeric(fthgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fthgen_cases <- as.numeric(fthgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fvtgen <- gen_trans(df_noso$fthgen_inf*we_inf, rtu$median, so_scen = so_scen)
    df_noso$fvtgen_inf <- as.numeric(fvtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fvtgen_cases <- as.numeric(fvtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    sxtgen <- gen_trans(df_noso$fvtgen_inf*we_inf, rtu$median, so_scen = so_scen)
    df_noso$sxtgen_inf <- as.numeric(sxtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$sxtgen_cases <- as.numeric(sxtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    svtgen <- gen_trans(df_noso$sxtgen_inf*we_inf, rtu$median, so_scen = so_scen)
    df_noso$svtgen_inf <- as.numeric(svtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$svtgen_cases <- as.numeric(svtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    ## Uncertainty due to RT
    fstgen <- gen_trans(df_noso$n_missed*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$fstgen_inf_lo <- as.numeric(fstgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fstgen_cases_lo <- as.numeric(fstgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    scdgen <- gen_trans(df_noso$fstgen_inf*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$scdgen_inf_lo <- as.numeric(scdgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$scdgen_cases_lo <- as.numeric(scdgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    trdgen <- gen_trans(df_noso$scdgen_inf*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$trdgen_inf_lo <- as.numeric(trdgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$trdgen_cases_lo <- as.numeric(trdgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fthgen <- gen_trans(df_noso$trdgen_inf*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$fthgen_inf_lo <- as.numeric(fthgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fthgen_cases_lo <- as.numeric(fthgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fvtgen <- gen_trans(df_noso$fthgen_inf*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$fvtgen_inf_lo <- as.numeric(fvtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fvtgen_cases_lo <- as.numeric(fvtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    sxtgen <- gen_trans(df_noso$fvtgen_inf*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$sxtgen_inf_lo <- as.numeric(sxtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$sxtgen_cases_lo <- as.numeric(sxtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    svtgen <- gen_trans(df_noso$sxtgen_inf*we_inf, rtu$lower_50, so_scen = so_scen)
    df_noso$svtgen_inf_lo <- as.numeric(svtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$svtgen_cases_lo <- as.numeric(svtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fstgen <- gen_trans(df_noso$n_missed*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$fstgen_inf_hi <- as.numeric(fstgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fstgen_cases_hi <- as.numeric(fstgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    scdgen <- gen_trans(df_noso$fstgen_inf*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$scdgen_inf_hi <- as.numeric(scdgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$scdgen_cases_hi <- as.numeric(scdgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    trdgen <- gen_trans(df_noso$scdgen_inf*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$trdgen_inf_hi <- as.numeric(trdgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$trdgen_cases_hi <- as.numeric(trdgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fthgen <- gen_trans(df_noso$trdgen_inf*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$fthgen_inf_hi <- as.numeric(fthgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fthgen_cases_hi <- as.numeric(fthgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    fvtgen <- gen_trans(df_noso$fthgen_inf*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$fvtgen_inf_lo <- as.numeric(fvtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$fvtgen_cases_lo <- as.numeric(fvtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    sxtgen <- gen_trans(df_noso$fvtgen_inf*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$sxtgen_inf_lo <- as.numeric(sxtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$sxtgen_cases_lo <- as.numeric(sxtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    svtgen <- gen_trans(df_noso$sxtgen_inf*we_inf, rtu$upper_50, so_scen = so_scen)
    df_noso$svtgen_inf_lo <- as.numeric(svtgen$n_trans[1:length(df_noso$detect_date)])
    df_noso$svtgen_cases_lo <- as.numeric(svtgen$n_trans_cases[1:length(df_noso$detect_date)])
    
    ### Percentage of CO-CIN COVID cases that are linked to nosocomial transmission 
    # w1<-which(df_noso$detect_date == "2020-03-01")
    # w2<-which(df_noso$detect_date == "2020-04-01")
    # w3<-which(df_noso$detect_date == "2020-05-01")
    # w4<-which(df_noso$detect_date == "2020-05-31")
    df_perc_output <- df_noso %>%
      mutate(total_noso_acq_detect = n_noso + n_miss_hosp_case,
             total_noso_link_detect = n_noso + n_miss_hosp_case + fstgen_cases + scdgen_cases + 
               trdgen_cases + fthgen_cases,
             total_noso_link_detect_lo = n_noso + n_miss_hosp_case + fstgen_cases_lo + scdgen_cases_lo + 
               trdgen_cases_lo + fthgen_cases_lo,
             total_noso_link_detect_hi = n_noso + n_miss_hosp_case + fstgen_cases_hi + scdgen_cases_hi + 
               trdgen_cases_hi + fthgen_cases_hi,
             perc_noso_acq = 100*(total_noso_acq_detect)/n_all, 
             perc_noso_linked = 100*(total_noso_link_detect)/n_all,
             perc_noso_linked_lo = 100*(total_noso_link_detect_lo)/n_all,
             perc_noso_linked_hi = 100*(total_noso_link_detect_hi)/n_all)
    
    return(df_perc_output)
    
  })
}
