###### How many cases by each Trust? Code to calculate this 

### Calculate the contribution of nosocomial cases to the overall epidemic in a single Trust

#### Read in data
# Proportion detected per week 
nn <- read.csv("output_all_trusts/trusts_prop_detect.csv")[,-1]
if(!"ENG1" %in% unique(nn$place)){
  nn2 <- nn %>% filter(place == "ENG")
  nn2$place <- "ENG1"
  nn <- rbind(nn, nn2)
}

# COCIN data 
cocin_clean <- read.csv("data/cocin_clean.csv")[,-1]

# LoS data
los_data <- read.csv("data/first_los.csv")[,-1]

eng2 <- los_data %>% filter(place == "ENG")
eng2$place <- "ENG1"
los_data <- rbind(los_data, eng2)

# prop COCIN in SUS
prop_cocin_in_sus <- read.csv("output_all_trusts/prop_cocin_in_sus.csv")

## Weeks to run through
weeks = c(1, max(los_data$week))
## Trusts
# FIRST RUN 
#trusts <- c("ENG","RTH", "RR8", "RGT")
#trusts <- intersect(unique(los_data$Procode), unique(trust_data$ods))
trusts <- unique(prop_cocin_in_sus$Procode)


# Threshold cutoffs to check for 
onset_cutoffs <- c(5,8,10,14)

# Samples 
nsamp = max(nn$sample)

## Loop thru all the conditions needed to explore 
for(iii in trusts){ # places
  
  ## Where to store
  df_noso_eg <- c()
  
  for(kkk in onset_cutoffs){ # onset cutoffs
    print(c("RUN?",iii,kkk)) 
    
    # los  
    prob_los_o <- los_data %>% filter(place == iii) 
    
    # Nosocomial cases 
    ## Generate the data for this cut_off_date = number of detected nosocomial cases
    if(iii == "ENG"){cocin_use <- cocin_clean}else{cocin_use <- cocin_clean %>% filter(trust == iii)}
    if(iii == "ENG1"){cocin_use <- cocin_clean}
    
    ## Calculate number of nosocomial cases
    cocin_use$on_after_admin <- as.numeric(cocin_use$on_after_admin)
    cocin_use <- cocin_use %>%
      mutate(noso = ifelse(on_after_admin >= kkk,1,0))
    
    # if(dim(cocin_use)[1] > 1){ # R1H only one value
    # if(kkk == 1){ # just one plot per trust
    #   g <- ggplot(cocin_use, aes(x = on_after_admin)) + geom_bar(stat = "count") + 
    #     scale_x_binned("Onset days after admission") + 
    #     ggtitle(paste0(iii))
    #   ggsave(paste0("output_all_trusts/",iii,"_onset_after_admin_dist.pdf"))
    # }}
    
    for(lll in 1:nsamp){
      
      # prop detected for this Trust and cutoff 
      p_detect <- nn %>% filter(place == iii, cutoff == kkk, sample == lll) %>% 
        dplyr::select(week, prop_detect,prop_bc, prop_ad)
      
      ### Convert from COCIN to TOTALS (using prop in sus)
      #prop_cis <- 0.2 # Use to explore impact of this parameter
      #df_noso$multiplier <- 1/prop_cis
      prop_cis <- prop_cocin_in_sus %>% filter(Procode == iii) # What are the proportions of SUS in COCIN? 
      cocin_use$multiplier <- 0
      for(q in 1:max(cocin_use$week_admin)){
        pd <- 1/prop_cis[which(prop_cis$week == q),"prop"]
        w<-which(cocin_use$week_admin == q)
        cocin_use[w,"multiplier"] <- pd # put the correct proportion in each week 
      }
      # Multiply up to get the number of actual cases (if COCIN enrolled all)
      # This uses date of admission as this is the comparison between COCIN and SUS 
      cocin_use$noso_suscoc <- round(cocin_use$noso * cocin_use$multiplier)
      
      ### Detect date is the one to use for proportion detected 
      ## 
      df_noso <- cocin_use %>% group_by(detect_date) %>%
        summarise(n_noso_o = sum(noso),
                  n_all_o = n(),
                  n_noso = sum(noso_suscoc),
                  n_all = round(sum((multiplier)))) %>% # each row = 1 case: sum multiplier is 1 x multiplier for SUS
        mutate(detect_date = as.Date(detect_date)) %>%
        complete(detect_date = seq.Date(as.Date("2020-01-01"), as.Date("2020-10-10"), by="day"), # Complete all the dates
                 fill = list(n_noso = 0, n_all = 0,n_noso_o = 0, n_all_o = 0)) %>% # zero if no cases / information on that date
        mutate(week_detect = lubridate::week(detect_date))
      
      #### Missed infections - by date detect one, how many would be missed? 
      df_noso$factor_missed <- 0
      ### by date detect one, what proportion are missed as test before the cutoff?
      df_noso$prop_ad <- 0
      df_noso$week <- df_noso$week_detect
      for(q in 1:max(df_noso$week_detect)){
        pd <- p_detect[which(p_detect$week == q), "prop_detect"]
        pad <- p_detect[which(p_detect$week == q), "prop_ad"] / (1 - p_detect[which(p_detect$week == q), "prop_detect"])
        w<-which(df_noso$week == q)
        if(pd > 0){df_noso[w,"factor_missed"] <- 1/pd}else{df_noso[w,"factor_missed"] <- 0}
        df_noso[w,"prop_ad"] <- pad
      }
      # Have to subtract off as tracking number missed not total
      df_noso$n_missed <- round(pmax(0,df_noso$factor_missed*df_noso$n_noso - df_noso$n_noso))
      
      # Number after discharge
      df_noso$n_ad <- round(df_noso$prop_ad * df_noso$n_missed)
      
      ## Save for this Trusts
      df_noso_eg <- rbind(df_noso_eg, cbind(iii,kkk,lll,df_noso))
    }
  }
  
  colnames(df_noso_eg)[1:3] <- c("place","cutoff","sample")
  
  # Save for use in simulation 
  write.csv(df_noso_eg, paste0("output_all_trusts/total_numbers/",iii,"_noso_numbers_by_trust.csv")) 
}
