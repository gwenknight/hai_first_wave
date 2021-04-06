##### Compare COCIN to SUS

### Aiming for Trust level weekly proportion of C19 positive cases in SUS that are in COCIN
## SUS = week of admission
## COCIN = week of admission


######## DATA #############
###### TRUSTS 
trusts_los <- read.csv("data/trusts_include.csv")[,-1]

##### COCIN 
cocin_clean <- read.csv("data/cocin_clean.csv")[,-1]
cocin_clean$admission_date <- as.Date(cocin_clean$admission_date)
cocin_clean$detect_date <- as.Date(cocin_clean$detect_date)

###### Remove Trusts not in COCIN
t <- unique(cocin_clean$trust)
trusts <- intersect(trusts_los,t) # 1 removed
trusts <- c("ENG","ENG1",trusts)
write.csv(trusts, "data/trusts_include.csv")



######## Site data - instead of grabbing from subjid use Jon's code
data_site_orig <- read.csv("data/subjid_site_info.csv")
# some subjid in multiple times
data_site <- distinct(data_site_orig, subjid, .keep_all = TRUE)

cocin_td <- left_join(cocin_clean, data_site, by = "subjid")

###### SUS
sus0 <- read.csv("data/FirstPos_byProvWeek.csv", stringsAsFactors = FALSE)
dates <- c(as.Date("2020-01-01"), as.Date("2020-01-09") + 0:53 * 7)

# SUS week numbering: LOS_byProv_by_Week_POS: Week 1 covers admissions March 10th to 16th  
sus0$admission_datew <- as.Date(dates[sus0$week])

#### EDIT TRUSTS
w <- which(sus0$Procode %in% c("RT3","R1K"))
sus0[w,"Procode"] <- "R13"
w <- which(sus0$Procode %in% c("RRF","02H"))
sus0[w,"Procode"] <- "RR0"
w <- which(sus0$Procode %in% c("RDD","RQ8","RAJ"))
sus0[w,"Procode"] <- "ESX"

w <- which(sus0$Procode %in% c("RBA","RH5"))
sus0 <- sus0[-w,]

# Sort out multiple values for los.days with this edited Trust data
sus <- sus0 %>% group_by(Procode, week, admission_datew) %>% 
  summarise(n = sum(n)) %>% ## group new trusts together
  ungroup()

# Totals
ggplot(sus %>% group_by(admission_datew) %>% summarise(sum = sum(n)), aes(x=admission_datew, y = sum)) + 
  geom_point()

write.csv(sus, "data/sus_clean.csv")

############################################################################################################################################
####****** ALL TRUSTS ****** #######
####*
####*
####*

prop_cocin_in_sus <- c()
totals_cocin_sus_dates  <- c()
totals_cocin_sus_values  <- c()
ts <- c() # timeseries

for(ii in trusts){
  print(ii)
  
  if(!ii %in% c("ENG","ENG1")){
    ctrust <- cocin_clean %>% filter(trust == ii)  
    
    # Admission data aggregation
    ctrust_grp <- ctrust %>% 
      group_by(admission_date) %>%
      dplyr::summarise(admissions = n()) %>%
      mutate(week = lubridate::week(admission_date)) %>% #, "week", week_start = 1)) %>% #week(admission_date)) %>% 
      group_by(week) %>%
      summarise(week_admin = sum(admissions))
    
    #ggplot(ctrust_grp, aes(x=week, y = week_admin)) + geom_line() + geom_point()
    
    
    ## SUS
    strust <- sus %>% filter(Procode == ii) 
    strust$week = lubridate::week(strust$admission_datew)
    
    # ggplot(strust, aes(x=week, y = n)) + geom_line(col = "blue") + geom_point(col="blue") +
    #   #scale_x_date("Date of admission") +
    #   geom_line(data = ctrust_grp, aes(x=week, y = week_admin), col = "red") +
    #   geom_point(data = ctrust_grp, aes(x=week, y = week_admin), col = "red") +
    #   scale_y_continuous("Admissions") +
    #   ggtitle(paste0(ii,"(SUS = blue, COCIN = red)"))
    # ggsave(paste0("output_all_trusts/",ii,"_sus_cocin.pdf"))
    
    all <- left_join(strust, ctrust_grp, by = "week")
    all$prop = all$week_admin / all$n
    
    #w<-which(all$prop > 1)
    #all[w,"prop"] <- 1 # DON'T let go above one - cap
    
    # ggplot(all, aes(x=week, y = prop)) + geom_bar(stat="identity") +
    #  scale_y_continuous("Proportion of SUS cases in COCIN")
    # ggsave(paste0("output_all_trusts/",ii,"_sus_cocin_prop.pdf"))
    
    # STORE
    all$cocin_admin <- all$week_admin
    all$sus_admin <- all$n
    prop_cocin_in_sus <- rbind(prop_cocin_in_sus, 
                               all[,c("Procode","admission_datew","prop","cocin_admin","sus_admin")])
    
    ts <- rbind(ts, all)
    
    # total dates and numbers
    totals_cocin_sus_dates <- rbind(totals_cocin_sus_dates, c(as.Date(c(min(strust$admission_datew),max(strust$admission_datew),
                                                                        min(ctrust$admission_date),
                                                                        max(ctrust$admission_date)))))
    totals_cocin_sus_values <- rbind( totals_cocin_sus_values, c(ii,sum(strust$n),sum(ctrust_grp$week_admin)))
  }
}

## Output totals
totals_output <-as.data.frame(totals_cocin_sus_values)
colnames(totals_output) <- c("trust","sus","cocin")
totals_output$start_sus <- as.Date(totals_cocin_sus_dates[,1])
totals_output$end_sus <- as.Date(totals_cocin_sus_dates[,2])
totals_output$start_cocin <- as.Date(totals_cocin_sus_dates[,3])
totals_output$end_cocin <- as.Date(totals_cocin_sus_dates[,4])

write.csv(totals_output,"output_all_trusts/cocin_sus_data_summary.csv")

# Take average for ENGLAND
tsus <- sus %>% group_by(admission_datew) %>% summarise(total_sus_all = sum(n))

eng_av <- prop_cocin_in_sus %>% group_by(admission_datew) %>% 
  summarise(cocin_admin = sum(cocin_admin, na.rm = TRUE),
            sus_admin = sum(sus_admin),
            prop_av = mean(prop, na.rm = TRUE), #OLD
            prop = cocin_admin / sus_admin) %>%
  as.data.frame()

ta <- left_join(eng_av, tsus, by = "admission_datew")
ta$prop <- ta$cocin_admin / ta$total_sus_all


eng_av$Procode <- "ENG"
ta$Procode <- "ENG1"

ggplot(eng_av, aes(x=admission_datew, y = prop))  + 
  geom_line() + 
  geom_point(aes(y = prop_av)) +
  scale_y_continuous(lim = c(0,1.1)) + 
  geom_hline(yintercept = 1, lty = "dashed", col = "red") + 
  geom_point(data = ta, col = "red") + 
  ggtitle("black points = average prop \nline = cocin ac eng / sus ac eng\nred dots = cocin / total sus")
### Taking mean overestimates the proportion of cocin in SUS

eng1 <- eng_av[,c("Procode","admission_datew","prop","cocin_admin","sus_admin")]
eng1$Procode <- "ENG"

eng2 <- ta[,c("Procode","admission_datew","prop","cocin_admin","sus_admin")]
eng2$Procode <- "ENG1"# ENG1 has average proportion of COCIN in ALL SUS


prop_cocin_in_sus <- rbind(prop_cocin_in_sus,eng1,eng2)
prop_cocin_in_sus$week <- lubridate::week(prop_cocin_in_sus$admission_datew)


### Extend weeks out
trusts <- unique(prop_cocin_in_sus$Procode)
min_week <- 1
max_week = 53 # all of 2020
input_year <- 2020
input_day_of_week <- 3  # Wednesday = 1st Jan 2020
total_add_wks <- c()

for(t in trusts){
  
  pcs <- prop_cocin_in_sus %>% filter(Procode == t)
  w<-which(!is.na(pcs$prop))
  pcs <- pcs[w,c("Procode","prop","week")]
  fw <- min(pcs$week)
  lw <- max(pcs$week)
  
  # Fill in missing proportions: keep same as first or last
  valf = pcs[which(pcs$week == fw),"prop"]
  vall = pcs[which(pcs$week == lw),"prop"]
  kf <- cbind(t,  valf, 1:(fw-1)); colnames(kf) <- colnames(pcs)
  kl <- cbind(t,  vall, (lw+1):53); colnames(kl) <- colnames(pcs)
  pcs <- rbind(pcs, kf)
  pcs <- rbind(pcs,kl) 
  
  # In middle values: take halfway before and after
  for(i in fw:lw){
    p <- unlist(pcs[which(pcs$week == i),"prop"])
    if(length(p) == 0){
      print(c(t,i))
      p = (as.numeric(pcs[which(pcs$week == (i-1)),"prop"]) + as.numeric(pcs[which(pcs$week == (i-1)),"prop"]))/2
      pcs <- rbind(pcs, c(t,p,i))
    }
    
  }
  
  pcs$week <- as.numeric(pcs$week)
  pcs$prop <- as.numeric(pcs$prop)
  pcs <- pcs %>% arrange(week)
  
  dates <- as.Date("2019-12-30") + 0:53 * 7
  pcs$admission_datew <- as.Date(dates[pcs$week])
  
  total_add_wks <- rbind(total_add_wks, pcs)
}

total_add_wks$admission_datew <- as.Date(total_add_wks$admission_datew)

ggplot(total_add_wks, aes(x=admission_datew, y = prop)) + 
  geom_line(aes(group = Procode, colour = Procode)) + 
  scale_x_date("Admission date (start of week = Wednesday)", lim = as.Date(c("2020-01-01","2020-05-31"))) + 
  scale_y_continuous("Proportion of SUS cases in COCIN") + 
  facet_wrap(~Procode) + 
  theme(legend.position = "none")
ggsave("output_all_trusts/prop_sus_in_cocin_over_time.pdf", width = 20, height = 20)

### Cap at one 
w<-which(total_add_wks$prop > 1)
100*length(w) / dim(total_add_wks)[1] # 16% 
tt <- total_add_wks
tt[w,"prop"] <- 1 # DON'T let go above one - cap

ggplot(tt, aes(x=admission_datew, y = prop)) + 
  geom_line(aes(group = Procode, colour = Procode)) + 
  scale_x_date("Admission date (start of week = Wednesday)", lim = as.Date(c("2020-01-01","2020-05-31"))) + 
  scale_y_continuous("Proportion of SUS cases in COCIN") + 
  facet_wrap(~Procode)
ggsave("output_all_trusts/prop_sus_in_cocin_over_time_capped.pdf", width = 20, height = 20)


### SAVE
write_csv(total_add_wks,"output_all_trusts/prop_cocin_in_sus_nocap.csv")
write_csv(tt,"output_all_trusts/prop_cocin_in_sus.csv")


#### PLOT ENGLAND
total_add_wks <- read.csv("output_all_trusts/prop_cocin_in_sus.csv")
total_add_wks$admission_datew <- as.Date(total_add_wks$admission_datew)

ggplot(total_add_wks %>% filter(Procode == "ENG"), aes(x = admission_datew, y = prop)) + 
  geom_line() + 
  scale_x_date("Admission date", lim = as.Date(c("2020-02-01","2020-07-31"))) + 
  scale_y_continuous("Proportion enrolled", lim = c(0,1))
ggsave("output_all_trusts/proportion_cocin_enrollment.jpeg")

total_add_wks %>% filter(Procode == "ENG", week %in% c(1:32)) %>% summarise(mean(prop))

