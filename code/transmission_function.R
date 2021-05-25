### Transmission function: how much do missed nosocomial infections contribute to ongoing transmission? 

# Transmission function 
# Takes in number of cases time series (n_cases)
# and r time series
# and what so to hospitalisation scenario
gen_trans <- function(n_cases, r, so_scen = 1, params){
  with(params,{
    ll <- length(n_cases)
    n_trans <- matrix(0,ll+100,1) # When did infections occur? 
    n_trans_cases <- matrix(0,ll+125,1) # add in later date could become cases
    
    for(i in 1:ll){
      
      # print(c("herea",n_cases[i], r))
      # Depending on the R assumptions
      ifelse(length(r) == 1,
             n_infs <- round(n_cases[i] * r,0),    ## if single value for r then use this 
             n_infs <- round(n_cases[i] * r[i],0)) ## if time varying r: in per contribution function have checked date lines up
      
      ## When next infection set?
      if(n_infs > 0 ){ # if some to be sampled
        t_inf <- round(rgamma(n_infs, shape = infectious_shape, scale = infectious_scale),0) # generation time: time after case when new infection occurs
        dt_tally <- table(t_inf)
        if(min(t_inf) == 0){symp_on_infection <- dt_tally[1]; dt_tally <- dt_tally[-1];
        n_trans[i] <- as.numeric(symp_on_infection) + n_trans[i]
        }
        n_trans[i + as.numeric(names(dt_tally))] <- as.numeric(dt_tally) + n_trans[i + as.numeric(names(dt_tally))]
      }
      
      
      ## when next cases? 
      #n_next_cases <- sum(rbernoulli(n_infs,rnorm(n_infs,prop_comm_hosp_mean, prop_comm_hosp_sd))) # what proportion will become cases? 
      n_next_cases <- sum(rbernoulli(n_infs,prop_comm_hosp))
      #n_infs * rnorm(1,prop_comm_hosp_mean, prop_comm_hosp_sd),0) 
      if(n_next_cases > 0 ){
        t_inf <- t_inf[sample(seq(1,length(t_inf),1),n_next_cases)] # sample time to infection from above samples
        t_infs_symp <- round(rlnorm(n_next_cases, time_inf_to_symp_mean,time_inf_to_symp_sd)) # time from inf to symp
        
        if(so_scen == 1){t_symp_hosp <- round(rlnorm(n_next_cases, meanlog = time_symp_to_hosp_meanlog, sdlog = time_symp_to_hosp_sdlog),0)}
        if(so_scen == 2){t_symp_hosp <- round(rgamma(n_next_cases, shape = time_symp_to_hosp_shape, scale = time_symp_to_hosp_scale),0)} # time from symp to hosp (i.e. detection)
        if(so_scen == 3){t_symp_hosp <- round(rlnorm(n_next_cases, meanlog = time_symp_to_hosp_meanlog3, sdlog = time_symp_to_hosp_sdlog3),0)}
        
        dt_tally <- table(t_infs_symp + t_symp_hosp + t_inf)
        if(min(t_infs_symp + t_symp_hosp + t_inf) == 0){symp_on_infection <- dt_tally[1]; dt_tally <- dt_tally[-1];
        n_trans_cases[i] <- as.numeric(symp_on_infection) + n_trans_cases[i]
        }
        n_trans_cases[i + as.numeric(names(dt_tally))] <- as.numeric(dt_tally) + n_trans_cases[i + as.numeric(names(dt_tally))]
      }
    }
    
    return(list(n_trans = n_trans, n_trans_cases = n_trans_cases))
  })
}

## PLOT POTENTIAL OUTPUT
#plot(df_noso$n_missed)
#points(fstgen$n_trans, col = "red")
#points(fstgen$n_trans_cases, col = "blue")