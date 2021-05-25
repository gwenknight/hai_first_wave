#### LoS by Trust SUS from James Stimson
### Grabs the data, sorts out Trust labelling
## Fills in time series for each Trust of LoS by week 

library(tidyverse)
library(MASS)
library(fitdistrplus)


### Data from SUS on los of stay by patients admitted by each week 
los_neg_orig <- read.csv("data/Neg_byProvWeekLos.csv", stringsAsFactors = FALSE)

#### EDIT TRUSTS
w <- which(los_neg_orig$Procode %in% c("RT3","R1K"))
los_neg_orig[w,"Procode"] <- "R13"
w <- which(los_neg_orig$Procode %in% c("RRF","02H"))
los_neg_orig[w,"Procode"] <- "RR0"
w <- which(los_neg_orig$Procode %in% c("RDD","RQ8","RAJ"))
los_neg_orig[w,"Procode"] <- "ESX"

w <- which(los_neg_orig$Procode %in% c("RBA","RH5"))
los_neg_orig <- los_neg_orig[-w,]

### only those in England and Acute
trusts <- read.csv("data/trusts_eng_ac.csv")[,-1]

los_neg_orig_filter <- los_neg_orig %>% filter(Procode %in% trusts)

# Sort out multiple values for los.days with this edited Trust data
ll <- los_neg_orig %>% group_by(Procode, week, los.days) %>% 
  summarise(n = sum(n))## group new trusts together

ll2 <- los_neg_orig_filter %>% group_by(Procode, week, los.days) %>% 
  summarise(n = sum(n)) ## group new trusts together


los_neg <- ll2 %>% group_by(Procode, week) %>%
  mutate(total_admin = sum(n), prop = n / total_admin) %>%
  ungroup() %>%
  group_by(Procode, los.days) %>%
  mutate(datapoints = n(), unique_week = n_distinct(week)) %>%
  #filter(datapoints > 10 & total_admin > 100, unique_week > 5) %>% # don't need now
  filter(los.days >= 0) # can't have negative LoS

los_neg_unfilter <- ll %>% group_by(Procode, week) %>%
  mutate(total_admin = sum(n), prop = n / total_admin) %>%
  ungroup() %>%
  group_by(Procode, los.days) %>%
  mutate(datapoints = n(), unique_week = n_distinct(week)) %>%
  filter(datapoints > 10 & total_admin > 100, unique_week > 5) %>%
  filter(los.days >= 0) # can't have negative LoS


### Long LoS Trusts
gg <- los_neg_unfilter %>% group_by(Procode,week) %>% mutate(level = prop*los.days) %>%
  summarise(mm = sum(level, na.rm = TRUE))
ggplot(gg, aes(mm)) + geom_histogram(binwidth = 1) + 
  scale_x_continuous("Mean length of stay (COVID-NEG.)") + 
  scale_y_continuous("Number of Trusts")

ggplot(gg, aes(mm)) + geom_histogram(binwidth = 1) + 
  scale_x_continuous("Mean length of stay (COVID-NEG.)", lim = c(0,10)) + 
  scale_y_continuous("Number of Trusts")

w <- which(gg$mm <= 7)
uu_notlong <- unique(gg[w,"Procode"])
write.csv(uu_notlong, "data/trusts_notlong_los.csv")

uu_long <- unique(gg[-w,"Procode"])

#### Only ACUTE  (ENGLAND - ok as SUS)
infor <- read.csv("data/subjid_site_info.csv", stringsAsFactors = FALSE)
infor_eng_ac <- infor %>% filter(region %in% c("North West","London","Midlands","South West","North East & Yorkshire",
                                               "South East","East of England")) %>%
  filter(site.type == "Acute")
trusts_parents <- c(unique(infor_eng_ac$parent.code), "R13","RR0","ESX") # add in new SuperTrusts

###### ***************************************************************************************************
#### LOS for Acute not long LoS Trusts
los_all <- los_neg #los_neg %>% filter(Procode %in% (unlist(uu_notlong)),Procode %in% trusts_parents)

### Acute England Trusts
los_eng <- los_neg %>% filter(Procode %in% (unlist(uu_notlong))) %>% 
  filter(Procode %in% trusts_parents) %>% 
  dplyr::select(c("Procode","week","los.days","n")) %>%
  group_by(week, los.days) %>%
  summarise(total_admin = sum(n)) %>%
  ungroup() %>%
  group_by(week) %>% 
  mutate(total_week = sum(total_admin), prop = total_admin / total_week)

los_eng_av = los_eng %>% ungroup() %>% group_by(los.days) %>% summarise(propm = mean(prop))


ggplot(los_eng, aes(x=los.days, y = prop)) + 
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~week, scales = "free") + 
  scale_fill_discrete("Week") + 
  scale_x_continuous("Length of stay (days)") + 
  scale_y_continuous("Proportion of patients admitted that week") 
ggsave("output_all_trusts/eng_los.pdf")

ggplot(los_eng_av, aes(x=los.days, y = propm)) + 
  geom_bar(stat="identity") 


w<-which(los_eng$los.days == 0)
los_eng$los.days <- as.numeric(los_eng$los.days)
los_eng[w,"los.days"] <- as.numeric(unlist(los_eng[w,"los.days"])) + 0.5 # add 0.5 to 0 so that it is actually a time

los_eng %>% mutate(level = prop*(los.days+0.5)) %>% 
  group_by(week) %>%
  summarise(mm = sum(level, na.rm = TRUE), total = sum(prop, na.rm = TRUE)) %>%
  ggplot(aes(x=week, y = mm)) + geom_line() + scale_y_continuous("Mean length of stay (days)")
ggsave("output_all_trusts/eng_los_over_time.jpeg")
# Los = only about 2 days? 

# Check new trusts
# ggplot(los_neg %>% filter(Procode == "ESX"), aes(x=los.days, y = prop)) + 
#   geom_bar(stat="identity", position = "dodge") +
#   #facet_wrap(~week, scales = "free") + 
#   scale_fill_discrete("Week") + 
#   scale_x_continuous("Length of stay (days)") + 
#   scale_y_continuous("Proportion of patients admitted that week")

######************************************************************************
##### Output
los_eng$place <- "ENG"
los_all$place <- los_all$Procode

w<-which(los_all$los.days == 0)
los_all$los.days <- as.numeric(los_all$los.days)
los_all[w,"los.days"] <- as.numeric(unlist(los_all[w,"los.days"])) + 0.5 # add 0.5 to 0 so that it is actually a time
los_all$total_admin <- los_all$n # confusing with England being total admin = total per los day 


los_use <- rbind(los_eng[,c("place","week","los.days","total_admin","prop")],
                 los_all[,c("place","week","los.days","total_admin","prop")])

w<-which(los_use$los.days == 0)
los_use$los.days <- as.numeric(los_use$los.days)
los_use[w,"los.days"] <- as.numeric(unlist(los_use[w,"los.days"])) + 0.5 # add 0.5 to 0 so that it is actually a time


ggplot(los_use, aes(x=los.days, y = prop, fill = factor(week))) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~place, scales = "free") + 
  scale_fill_discrete("Week") + 
  scale_x_continuous("Length of stay (days)", lim = c(0,20)) + 
  scale_y_continuous("Proportion of patients admitted that week")
ggsave("output_all_trusts/los_all_first_trusts.pdf")

ggplot(los_use, aes(x=los.days, y = prop, col = factor(place))) + 
  geom_line() + 
  facet_wrap(~week, scales = "free", ncol = 5) + 
  scale_colour_discrete("Place") + 
  scale_x_continuous("Length of stay (days)", lim = c(0,20)) + 
  scale_y_continuous("Proportion of patients admitted that week")
ggsave("output_all_trusts/los_all_first_trusts_line.pdf", width = 10, height = 5)

#### Expand backward and forward
#### Fill in gaps 
los_trusts <- unique(los_use$place)
los_use_complete <- c()
for(ii in los_trusts){
  print(ii)
  los_here <- los_use %>% filter(place == ii)
  # Fill forward and back 
  minwk <- min(los_here$week)
  maxwk <- max(los_here$week)
  valmin <- los_here %>% filter(week == minwk)
  valmax <- los_here %>% filter(week == maxwk)
  for(i in (minwk-1):1){
    valmin$week <- i
    los_here <- rbind(valmin,los_here)
  }
  for(i in (maxwk+1):53){
    valmax$week <- i
    los_here <- rbind(valmax,los_here)
  }
  u <- unique(los_here$week)
  # Missing any in the middle? 
  if(length(u) < 53){
    s <- setdiff(1:maxwk, u)
    for(ss in s){ # set los to equal that of the week before 
      valhere <- los_here %>% filter(week == (ss-1))
      valhere$week <- ss
      los_here <- rbind(valhere,los_here)
    }
  }  
  
  los_here_complete <- c()
  ### Fill in all possible LOS values to complete 
  for(wk in 1:53){
    los_herew <- los_here %>% filter(week == wk)
    # Are there any in here? 
    ws <- which(los_herew$los.days == 0.5)
    if(length(ws)>0){ # if there is something at 0.5
      if(length(los_herew[,1])>1){ # and after 0.5!
        los_start <- los_herew[ws,] %>%
          dplyr::select(place, week, los.days , total_admin, prop)
        
        los_end <- los_herew[-ws,] %>%
          dplyr::select(place, week, los.days , total_admin, prop) %>%
          complete(week,los.days = 1:max(10,los_herew$los.days),fill=list(prop = 0, total_admin = 0, place = ii)) # make sure have a value for each possible los
        los_herew2 <- rbind(los_start, los_end)
      }else{ # if nothing apart 0.5 then
        los_herew2 <- los_herew %>%
          dplyr::select(place, week, los.days , total_admin, prop) %>%
          complete(week,los.days = 1:max(10,los_herew$los.days),fill=list(prop = 0, total_admin = 0, place = ii)) # make sure have a value for each possible los
      }
      
    }else{los_start <- los_herew[1,]; 
    los_start$los.days = 0.5; los_start$total_admin = 0; los_start$prop = 0
    
    los_end <- los_herew %>% 
      complete(week,los.days = 1:max(10,los_herew$los.days),fill=list(prop = 0, total_admin = 0, place = ii)) # make sure have a value for each possible los
    los_herew2 <- rbind(los_start, los_end)
    }
    los_here_complete <- rbind(los_here_complete, los_herew2)
  }
  
  los_use_complete<- rbind(los_use_complete, los_here_complete)
  
}

los2 <- los_use_complete %>% filter(place == "ENG")
los2$place <- "ENG1"
los_use_complete <- rbind(los_use_complete, los2)


write.csv(los_use_complete,"data/all_los.csv")



# MEAN
los_use_complete$valu = los_use_complete$los.days*los_use_complete$prop


llm <- los_use_complete %>% group_by(place, week) %>%
  summarise(sum = sum(valu))
g <- ggplot(llm %>% filter(place == "ENG"), aes(x=week, y = sum)) + geom_point() +  geom_line(aes(col = place)) + 
  facet_wrap(~place) + 
  scale_x_continuous("Week") + 
  scale_y_continuous("Mean length of stay", lim = c(0,3.6)) + 
  geom_vline(xintercept = minwk, lty = "dashed") +
  geom_vline(xintercept = maxwk, lty = "dashed") 
ggsave("output_figs/mean_los_ENG.jpeg")

g + scale_x_continuous("Week", lim = c(3,35))
ggsave("output_all_trusts/mean_los_sus_eg_trust_zoom.pdf")


llm %>% group_by(place) %>%
  summarise(mean = mean(sum), sd = sd(sum), median = median(sum), iqr =  iqr(sum)) %>% 
  ggplot(aes(x=place, y = mean)) + geom_point()

### save trusts
write.csv(los_trusts,"data/trusts_include.csv")

