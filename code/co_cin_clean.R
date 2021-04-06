#### Clean COCIN
#cocin_clean <- read.csv("data/df_onset_admin_2020-06-03_1257.csv")[,-1]
#cocin_clean <- read.csv("data/df_onset_admin_2020-09-28_1041.csv")[,-1]
cocin_clean <- read_csv("data/df_onset_admin_2020-12-03.csv")[,-1]

cocin_clean$trust <- substr(gsub( "-.*$", "", cocin_clean$subjid),1,3)

### Only ENGLAND and ACUTE
infor <- read.csv("data/subjid_site_info.csv")
infor_eng_ac <- infor %>% filter(region %in% c("North West","London","Midlands","South West","North East & Yorkshire",
                                               "South East","East of England")) %>%
  group_by(parent.code) %>% 
 mutate(all_acute = ifelse(all(site.type == "Acute"),1,0)) %>% 
 filter(all_acute == 1)
trusts_parents <- unique(infor_eng_ac$parent.code)
write.csv(trusts_parents,"data/acute_eng_trusts_cocin.csv")

cocin_clean_eng_ac <- cocin_clean %>% filter(trust %in% trusts_parents)

### Nosocomial dates
cocin_clean_noso <- cocin_clean_eng_ac %>% 
  filter(detect_date < as.Date("2020-07-31")) %>%
  mutate(on_after_admin = as.Date(onset_date) - as.Date(admission_date),
         week_admin = lubridate::week(admission_date)) %>%
  filter(on_after_admin > -150)


#### EDIT TRUSTS
w <- which(cocin_clean_noso$trust %in% c("RT3","R1K"))
cocin_clean_noso[w,"trust"] <- "R13"
w <- which(cocin_clean_noso$trust %in% c("RRF","02H"))
cocin_clean_noso[w,"trust"] <- "RR0"
w <- which(cocin_clean_noso$trust %in% c("RDD","RQ8","RAJ"))
cocin_clean_noso[w,"trust"] <- "ESX"

w <- which(cocin_clean_noso$trust %in% c("RBA"))
cocin_clean_noso <- cocin_clean_noso[-w,]

w <- which(cocin_clean_noso$trust %in% c("RX3")) # No LoS data
cocin_clean_noso <- cocin_clean_noso[-w,] 


write.csv(cocin_clean_noso, "data/cocin_clean.csv")

# ACUTE ENG TRUSTS
trusts <- c("ENG","ENG1",unique(cocin_clean_noso$trust))
write.csv(trusts, "data/trusts_eng_ac.csv")


######## CHECKS ##########

### Original numbers in COCIN
cocin_clean_noso %>%
  filter(detect_date < as.Date("2020-07-31")) %>%
  mutate(noso = ifelse(on_after_admin >= 8,1,0)) %>%
  summarise(total = n(), hosp = sum(noso), comm = total - hosp, perc_noso = 100*hosp / total)

#### Check numbers without taking out England trusts
### Nosocomial dates
cocin_clean %>% mutate(on_after_admin = as.Date(onset_date) - as.Date(admission_date),
                       week_admin = lubridate::week(admission_date)) %>%
  filter(on_after_admin > -150) %>%
  mutate(noso = ifelse(on_after_admin >= 8,1,0)) %>%
  summarise(total = n(), hosp = sum(noso), comm = total - hosp, perc_noso = 100*hosp / comm)


# ##### Just April: to check with Jon
# # ENGLAND
# # Using 28th Sept cocin_clean
# cocin_clean_o <- read.csv("data/df_onset_admin_2020-09-28_1041.csv")[,-1]
# cocin_clean_o$trust <- substr(gsub( "-.*$", "", cocin_clean_o$subjid),1,3)
# 
# infor_eng_ac <- infor %>% filter(region %in% c("North West","London","Midlands","South West","North East & Yorkshire",
#                                                "South East","East of England")) %>% 
#   filter(!is.na(parent.code))
# trusts_parents <- unique(infor_eng_ac$parent.code) # JUST ENGLAND
# 
# c_april_o <- cocin_clean_o %>% filter(trust %in% trusts_parents) %>% # need this filter as not just England - but make sure take out acute bit
#   mutate(on_after_admin = as.Date(onset_date) - as.Date(admission_date),
#          week_admin = lubridate::week(admission_date)) %>%
#   filter(on_after_admin > -150) %>%
#   #filter(admission_date <= as.Date("2020-04-30"), admission_date >= as.Date("2020-04-01")) %>%
#   mutate(noso = ifelse(on_after_admin >= 5,1,0)) 
# 
# 
# # Using 3rd Dec cocin_clean
# c_april<- cocin_clean %>% filter(trust %in% trusts_parents) %>% 
#   mutate(on_after_admin = as.Date(onset_date) - as.Date(admission_date),
#          week_admin = lubridate::week(admission_date)) %>%
#   filter(on_after_admin > -150) %>%
#   #filter(admission_date <= as.Date("2020-04-30"), admission_date >= as.Date("2020-04-01")) %>%
#   mutate(noso = ifelse(on_after_admin >= 5,1,0)) 
# 
# # Compare
# c_april_o %>% summarise(total = n(), hosp = sum(noso), comm = total - hosp, perc_noso = 100*hosp / total)
# c_april %>% summarise(total = n(), hosp = sum(noso), comm = total - hosp, perc_noso = 100*hosp / total)
# 100*829 / 63000
# 
# 
# 
# 
