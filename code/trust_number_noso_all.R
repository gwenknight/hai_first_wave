###### How many cases by each Trust? Code to calculate this 

### Calculate the contribution of nosocomial cases to the overall epidemic in a single Trust

#### Read in data
# Proportion detected per week 
nn <- read_csv("output_all_trusts/trusts_prop_detect.csv")[,-1]

if(!"ENG1" %in% unique(nn$place)){
  nn2 <- nn %>% filter(place == "ENG")
  nn2$place <- "ENG1"
  nn <- rbind(nn, nn2)
}

# COCIN data 
cocin_clean <- read.csv("data/cocin_clean.csv")[,-1]

# LoS data
los_data <- read.csv("data/all_los.csv")[,-1] # just for number of weeks

#eng2 <- los_data %>% filter(place == "ENG")
#eng2$place <- "ENG1"
#los_data <- rbind(los_data, eng2)

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
onset_cutoffs <- c(5,8,10,15)

# Samples 
nsamp = max(nn$sample)


# Function for number of binomial trials = number of missed infections
binom.ntrial.pos<-function(x, p, ntrials.prior){
  # function that returns the posterior distribution for the unknown
  # number of binomial trials
  # It takes as input:
  # x, the number of successes in an unknown number of binomial trials
  # p, the probability of success in each binomial trial
  # ntrials.prior, a vector specifying the prior prob for 1,2,3...successes
  n<-length(ntrials.prior)
  if(n < x) stop("need non-zero prior prob for at least x trials")
  ntrials.prior<-ntrials.prior/sum(ntrials.prior) #normalize
  
  # Using Bayes formula p(a|b) = p(b|a)p(a)/p(b)
  # Assuming known p, we want to calculate this
  # p(ntrials|x,p)=p(x|p, ntrials) p(ntrials)/p(x)
  # where p(x)= sum over i: p(x| ntrials=i ) p(ntrials=i)
  prob_x_given_p_and_ntrials<-dbinom(x, size=1:n, p)
  numerator<-prob_x_given_p_and_ntrials*ntrials.prior
  denom<-sum(dbinom(x,1:n,p)*ntrials.prior)
  posterior<-numerator/denom
  return(posterior)
}


## Loop thru all the conditions needed to explore 
for(iii in trusts){ # places
  
  ## Where to store
  df_noso_eg <- c()
  
  for(kkk in onset_cutoffs){ # onset cutoffs
    print(c("RUN?",iii,kkk)) 
    
    # los  
    #prob_los_o <- los_data %>% filter(place == iii) 
    
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
                  n_all = round(sum((multiplier))), .groups = 'drop') %>% # each row = 1 case: sum multiplier is 1 x multiplier for SUS
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
        if(pd > 0){df_noso[w,"factor_missed"] <- 1/pd; df_noso[w,"prop_detect"] <- pd}else{df_noso[w,"factor_missed"] <- 0}
        df_noso[w,"prop_ad"] <- pad
      }
      #### Calculate number of missed infections as the number of "trials"
      ### x = number of nosocomial cases 
      ### p = probability of detection 
      ### prior = flat ok = makes little impact
      df_noso$n_missed <- 0
      for(ib in 1:dim(df_noso)[1]){
        if(df_noso[ib,"n_noso"]>0){
          df_noso[ib,"n_missed"] = sum(binom.ntrial.pos(as.numeric(df_noso[ib,"n_noso"]), as.numeric(df_noso[ib,"prop_detect"]), seq(0.1,1, length = 2000))*seq(1:2000)) - df_noso[ib,"n_noso"]
        }
      }
      
      # Have to subtract off as tracking number missed not total - original version 
      df_noso$n_missed_old <- round(pmax(0,df_noso$factor_missed*df_noso$n_noso - df_noso$n_noso))
      
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


# Output for all settings
df_noso_eg <- list.files(path="output_all_trusts/total_numbers/", pattern = "_noso_numbers_by_trust.csv", full.names = TRUE) %>%
  lapply(fread) %>%
  bind_rows
# Error if already trusts_prop_detect.csv there

df_noso_eg <- as.data.frame(df_noso_eg[,-1])

### Save for use in percentage contribution calculation
write.csv(df_noso_eg,"output_all_trusts/noso_numbers_by_trust.csv")[,-1]



########********#################################################
#### **** PLOTS ****#############################################
########********#################################################
########*
library(dplyr)
library(readr)
df_noso_eg <- list.files(path="output_all_trusts/total_numbers/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write_csv(df_noso_eg, "output_all_trusts/noso_numbers_by_trust.csv") 

#### Look at totals
df_noso_eg$detect_date <- as.Date(df_noso_eg$detect_date)

sum_df_noso <- df_noso_eg %>%   
  filter(detect_date < as.Date("2020-07-31")) %>% 
  group_by(place, cutoff) %>%
  summarise(sum_noso_o = sum(n_noso_o), 
            sum_noso = sum(n_noso), 
            sum_all = sum(n_all),
            sum_miss = sum(n_missed),
            total_noso = sum(sum_noso + sum_miss), .groups = 'drop') %>%
  arrange(cutoff, total_noso) 


# CHECKS
sum_df_noso %>% filter(place == "ENG")
sum_df_noso %>% filter(place == "ENG1")


df_noso_eg %>% filter(place == "ENG") %>% 
  ggplot(aes(x=detect_date, y = n_noso))  + geom_line() + 
  theme(axis.text.x =element_text(angle = 90)) +
  scale_x_date("Detection date",date_breaks = "1 month", date_labels = "%B") 


#### Pivot to plot 
ss <- sum_df_noso %>% pivot_longer(cols = c(sum_noso_o, sum_noso, sum_all, sum_miss,total_noso))
ss$name <- factor(ss$name, levels = c("sum_all", "total_noso", "sum_miss", "sum_noso", "sum_noso_o"))

ggplot(ss %>% filter( place != "ENG"), aes(x=cutoff, y=value, group = interaction(place,name)))  + 
  geom_point() + 
  geom_line(aes(col=name)) + 
  scale_y_continuous("Total number of individuals") + 
  scale_x_continuous("Cutoff threshold") + 
  scale_color_discrete("Stage",labels = c("COCINxSUSfactor: All COVID19",
                                          "Total noso. infections estimate",
                                          "C x Sf x pd: Missed number",
                                          "C x Sf:  Noso. cases",
                                          "C: Original noso. cases")) +
  facet_wrap(~place)
ggsave("output_all_trusts/noso_cases_by_trust_split_by_levels.pdf")

### By week: how do the numbers change?
sum_df_noso <- df_noso_eg %>% group_by(week_detect,place, cutoff) %>%
  summarise(sum_noso_o = sum(n_noso_o), 
            sum_noso = sum(n_noso), 
            sum_all = sum(n_all),
            sum_miss = sum(n_missed),
            total_noso = sum(sum_noso + sum_miss), .groups = 'drop') %>%
  arrange(cutoff, total_noso) %>%
  pivot_longer(cols = c(sum_noso_o, sum_noso,sum_all, sum_miss, total_noso))

sum_df_noso$name <- factor(sum_df_noso$name, levels = c("sum_all", "sum_noso_o", "sum_noso", "sum_miss", "total_noso"))

labels_place <- c(RGT = "Cambridge", RR8 = "Leeds", RTH = "Oxford")
labels_name <- c(sum_noso_o = "Detect noso.", sum_noso = "Est noso cases", sum_all = "All cases",
                 sum_miss = "Missd noso infect",total_noso = "Est noso total")

g11 <- sum_df_noso %>% filter(place != "ENG") %>%
  ggplot(aes(x=week_detect, y = value)) + geom_line(aes(col = factor(cutoff))) + 
  facet_grid(name~place, scales = "free", labeller = labeller(place = labels_place, name = labels_name)) + 
  scale_color_discrete("Cutoff\nthreshold") + 
  scale_y_continuous("Number of individuals") + 
  scale_x_continuous("Week detected", lim = c(10,20))

## How does proportion detect vary over this same time?
g12 <- ggplot(nn %>% filter(place != "ENG"), aes(x=week, y = 1/prop_detect, group = cutoff)) + 
  geom_line(aes(col = factor(cutoff))) + 
  facet_wrap(~place,labeller = labeller(place = labels_place)) + 
  scale_color_discrete("Cutoff\nthreshold") + 
  scale_x_continuous("Week detected", lim = c(10,20)) + 
  scale_y_continuous("Factor multiplication\ndue to non-detection")

## What is the end result? 
g13 <- sum_df_noso %>% group_by(place,cutoff) %>% filter(name == "total_noso") %>%
  summarise(sum_noso = sum(value), .groups = 'drop') %>% filter(place != "ENG") %>%
  ggplot(aes(x=factor(cutoff), y = sum_noso)) + 
  geom_bar(stat = "identity", aes(fill = factor(cutoff))) + 
  facet_wrap(~place,labeller = labeller(place = labels_place)) + 
  scale_fill_discrete("Cutoff\nthreshold") +
  scale_x_discrete("Cutoff threshold") + 
  scale_y_continuous("Total estimated\nnosocomial infections")

g14 <- sum_df_noso %>% filter(place != "ENG") %>% group_by(place,cutoff,name) %>%
  arrange(week_detect) %>%
  mutate(val_cum = cumsum(value)) %>% 
  ggplot(aes(x=week_detect, y = val_cum)) + geom_line(aes(col = factor(cutoff))) + 
  facet_grid(name~place, scales = "free",labeller = labeller(place = labels_place, name = labels_name)) + 
  scale_color_discrete("Cutoff\nthreshold") + 
  scale_y_continuous("Cumulative number of individuals") + 
  scale_x_continuous("Week detected", lim = c(10,20))

(g11  + g14 + 
    plot_layout(guides = "collect"))  / (g12  +  g13) + 
  plot_layout(heights = c(5, 1)) + 
  plot_annotation(tag_levels = 'A')
ggsave("output_all_trusts/vals_cumu_pdf_result_estimated_noso.pdf", width = 10, height = 10)


#### Table of values 
sum_df_noso_table <- df_noso_eg %>% 
  filter(detect_date < as.Date("2020-07-31")) %>% 
  group_by(place, cutoff) %>%
  summarise(sum_noso_o = sum(n_noso_o), 
            sum_noso = sum(n_noso), 
            sum_all_o = sum(n_all_o),
            sum_all = sum(n_all),
            sum_miss = sum(n_missed),
            total_noso = sum(sum_noso + sum_miss), .groups = 'drop') %>%
  arrange(cutoff, total_noso) %>%
  pivot_longer(cols = c(sum_noso_o, sum_noso,sum_all_o,sum_all, sum_miss, total_noso)) %>%
  pivot_wider(names_from = cutoff, values_from = value) %>%
  arrange(name,place)

plot_eg <- df_noso_eg %>% group_by(place, cutoff) %>%
  summarise(sum_noso_o = sum(n_noso_o), 
            sum_noso = sum(n_noso), 
            sum_all_o = sum(n_all_o),
            sum_all = sum(n_all),
            sum_miss = sum(n_missed),
            total_noso = sum(sum_noso + sum_miss)) %>%
  arrange(cutoff, total_noso) 

ggplot(plot_eg %>% filter(place %in% trusts), 
       aes(x=cutoff, y = sum_noso_o, group = place)) + geom_line(aes(col = place)) + 
  scale_color_discrete("Place") + 
  geom_point(aes(col = place)) + 
  scale_x_continuous("Cutoff") + 
  scale_y_continuous("Number of nosocomial cases detect in COCIN")
ggsave("output_all_trusts/Original_noso_numbers.pdf")




aa <- sum_df_noso_table %>% filter(name %in% c("sum_all_o")) %>% dplyr::select("1")
bb <- sum_df_noso_table %>% filter(name %in% c("sum_noso_o")) 
cc <- left_join(aa,bb, by = "place")
write.csv(cc,"output_all_trusts/table_cocin_orig_values.csv")

aa <- sum_df_noso_table %>% filter(name %in% c("sum_all")) %>% dplyr::select("1")
bbn <- sum_df_noso_table %>% filter(name %in% c("sum_noso")) 
bb <- sum_df_noso_table %>% filter(name %in% c("total_noso")) 
cc <- left_join(left_join(aa,bbn,by = "place"),bb, by = "place")
write.csv(cc,"output_all_trusts/table_cocin_final_est_values.csv")

sum_df_noso_table %>% filter(place == "ENG")


#### Detected vs missed
sum_df_noso %>% filter(name %in% c("total_noso","sum_miss","sum_noso")) %>%
  group_by(place,cutoff, name) %>% 
  summarise(sum = sum(value)) %>% filter(place != "ENG") %>%
  ggplot(aes(x=factor(cutoff), y = sum)) + 
  geom_bar(stat = "identity", position = "dodge",aes(fill = name)) + 
  facet_wrap(~place,labeller = labeller(place = labels_place)) + 
  scale_fill_discrete("Nosocomial", labels = c("Detected cases","Missed infections","Total infections")) +
  scale_x_discrete("Cutoff threshold") + 
  scale_y_continuous("Number of individuals") 
ggsave("output_all_trusts/bar_split_final_est_values.pdf", width = 10, height = 7)

sum_df_noso %>% filter(name %in% c("total_noso","sum_miss","sum_noso")) %>%
  group_by(place,cutoff, name) %>% 
  summarise(sum = sum(value)) %>% filter(place == "ENG", cutoff == 8) %>%
  ggplot(aes(x=factor(cutoff), y = sum)) + 
  geom_bar(stat = "identity", position = "dodge",aes(fill = name)) + 
  scale_fill_discrete("Nosocomial", labels = c("Detected cases","Missed infections","Total infections")) +
  scale_x_discrete("Cutoff threshold") + 
  scale_y_continuous("Number of individuals") 
ggsave("output_all_trusts/ENG_bar_split_final_est_values.jpeg", width = 5, height = 3)


