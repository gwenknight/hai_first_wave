### Function to analyse the output


analyse_store <- function(store, name_o, output_location){
  
  store$detect_date <- as.Date(store$detect_date)
  store$r <- as.character(store$r)
  w<-which(is.na(store$r))
  store[w,"r"] <- "rt"
  
  ####### NEW TRAJECTORIES
  ### Community cases 
  # Prior to this work 
  store$n_comm_orig <- pmax(0,store$n_all_o - store$n_noso_o)
  store$n_hosp_orig <- pmax(0,store$n_noso_o)
  
  # FINAL 
  store$n_coca <- pmax(0,store$n_all - store$n_noso - store$n_miss_hosp_case)
  store$n_hoha <- store$n_noso # assume detect all 
  store$n_coha <- store$n_miss_hosp_case 
  store$n_cohl <- store$fstgen_cases + store$scdgen_cases + store$trdgen_cases + store$fthgen_cases 
  store$n_cohl_lo <- store$fstgen_cases_lo + store$scdgen_cases_lo + store$trdgen_cases_lo + store$fthgen_cases_lo
  store$n_cohl_hi <- store$fstgen_cases_hi + store$scdgen_cases_hi + store$trdgen_cases_hi + store$fthgen_cases_hi
  store$n_cf <- pmax(0,store$n_coca - store$n_cohl)
  store$n_cf_lo <- pmax(0,store$n_coca - store$n_cohl_hi) # switch to give lo values with hi onward
  store$n_cf_hi <- pmax(0,store$n_coca - store$n_cohl_lo)
  
  
  # Over sims
  #### Totals
  cf_all <- store %>%#filter(disch_time == 5) %>% 
    mutate(n_link_infect = fstgen_inf + scdgen_inf + trdgen_inf + fthgen_inf,
           n_link_infect_lo = fstgen_inf_lo + scdgen_inf_lo + trdgen_inf_lo + fthgen_inf_lo,
           n_link_infect_hi = fstgen_inf_hi + scdgen_inf_hi + trdgen_inf_hi + fthgen_inf_hi) %>%
    dplyr::select(sim_id, place, cutoff, disch_time, detect_date, r, scen_so, n_comm_orig, n_hosp_orig,
                  n_all, n_hoha, n_missed,n_link_infect,n_link_infect_lo, n_link_infect_hi,
                  n_coha, n_cf, n_coca, 
                  n_cohl,n_cohl_lo, n_cohl_hi) %>% 
    pivot_longer(cols = c(n_comm_orig:n_cohl_hi)) %>%
    ungroup() %>% 
    group_by(place, r, cutoff, name, sim_id,scen_so, disch_time)%>%
    mutate(mean_7=rollapply(value,7,mean,fill=NA)) %>%
    ungroup() 
  write.csv(cf_all, paste0(output_location,"/",name_o,"totals.csv"))
  #ggplot(cf_all%>% filter(name == "n_coha"), aes(x=detect_date, y = mean_sims, group = cutoff)) + geom_point(aes(col = factor(cutoff) )) + geom_line(aes(x=detect_date, y = mean_7))
  
  tcf_t <- cf_all %>% 
    filter(detect_date < as.Date("2020-07-31")) %>% 
    dplyr::select(-mean_7) %>% 
    group_by(disch_time, scen_so) %>% # only use for plotting
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(n_coca_orig = n_all - n_hoha, 
           total_hosp_acq = n_coha + n_hoha,
           #perc_hosp_acq = 100*(total_hosp_acq)/n_all, # take sum later 
           n_due_to_hosp = total_hosp_acq + n_missed,
           n_comm_ctosus = n_all - n_hoha,
           n_hospcase = n_hoha+n_coha,
           n_coca = n_all - n_hospcase) %>%
    pivot_longer(cols = n_comm_orig:n_hospcase) %>%
    group_by(place,r,cutoff, name, sim_id, disch_time, scen_so) %>%
    summarise(sum = sum(value,na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(place, r, cutoff, name,scen_so) 
  
  tcf_mean <-  tcf_t %>%
    summarise(mean_sum = mean(sum, na.rm = TRUE))
  
  tcf_sd <- tcf_t %>%
    summarise(sd_sum = sd(sum, na.rm = TRUE))
  
  tcf_range <- tcf_t %>%
    summarise(up = quantile(sum,probs=c(.05)),
              down = quantile(sum,probs=c(.95)))
      
    
  tcf <- left_join(left_join(tcf_mean, tcf_sd, by = c("place", "r","cutoff","name","scen_so")), tcf_range, by = c("place", "r","cutoff","name","scen_so"))
  tcf$perc_sd = 100*tcf$sd_sum/tcf$mean_sum
  ggplot(tcf, aes(x=r, y = mean_sum, group = name)) + geom_point(aes(col=name)) + 
    geom_errorbar(aes(ymin = mean_sum - sd_sum, ymax = mean_sum + sd_sum, col = name))
  
  tcf[,c("r","cutoff","name","mean_sum","sd_sum","up","down","scen_so")] %>% filter(r == "rt", name == "n_hoha") %>% mutate(nearest = round(mean_sum,-1))
  
  tcf[,c("r","cutoff","name","mean_sum","sd_sum","up","down","scen_so")] %>% filter(r == "rt", name == "n_missed") %>% mutate(nearest = round(mean_sum,-1))
  
  tcf[,c("r","cutoff","name","sd_sum","scen_so")] %>% filter(r == "rt", name == "n_hoha")
  tcf[,c("r","cutoff","name","sd_sum","scen_so")] %>% filter(r == "rt", name == "n_missed")
  
  tcf[,c("r","cutoff","name","mean_sum","sd_sum","scen_so")] %>% filter(cutoff == 5, r == "rt")
  
  write.csv(tcf, paste0(output_location,"/",name_o,"ENG_cf_values.csv"))
  
  # How much do missed infections contribute? 
  tcf_t[,c("sim_id","r","cutoff","name","sum","scen_so","disch_time")] %>% 
    filter(r == "rt", name %in% c("n_coca_orig", "n_coha")) %>% ungroup() %>%
    pivot_wider(values_from = sum, names_from = name) %>%
    mutate(perc_reassign = 100 * n_coha / n_coca_orig) %>%
    group_by(cutoff) %>%
    summarise(mean = mean(perc_reassign), up = range(perc_reassign)[1], down = range(perc_reassign)[2])
  
  
  
  
  ### Onward transmission  
  tcf_t[,c("sim_id","r","cutoff","name","sum","scen_so","disch_time")] %>% 
    filter(r == "rt", name %in% c("n_coca","n_cohl","n_cohl_lo","n_cohl_hi")) %>% ungroup() %>%
    pivot_wider(values_from = sum, names_from = name) %>%
    mutate(perc_cohl = 100 * n_cohl / n_coca,
           perc_cohl_lo = 100 * n_cohl_lo / n_coca,
           perc_cohl_hi = 100 * n_cohl_hi / n_coca) %>%
    group_by(cutoff,scen_so) %>%
    summarise(mean = mean(perc_cohl), sd = sd(perc_cohl), up = range(perc_cohl)[1], down = range(perc_cohl)[2], 
              mean_lo = mean(perc_cohl_lo), sd_lo = sd(perc_cohl_lo),up_lo = range(perc_cohl_lo)[1], down_lo = range(perc_cohl_lo)[2],
              mean_hi = mean(perc_cohl_hi), sd_hi = sd(perc_cohl_hi),up_hi = range(perc_cohl_hi)[1], down_hi = range(perc_cohl_hi)[2])
  
  tcf_t[,c("sim_id","r","cutoff","name","sum","scen_so","disch_time")] %>% filter(r == "rt", name %in% c("n_cohl","n_cohl_lo","n_cohl_hi",
                                                                                  "n_link_infect","n_link_infect_hi","n_link_infect_lo")) %>% ungroup() %>%
    pivot_wider(values_from = sum, names_from = name) %>%
    group_by(cutoff,scen_so) %>%
    summarise(mean = round(mean(n_cohl),-1), 
              mean_lo = round(mean(n_cohl_lo),-1), mean_hi = round(mean(n_cohl_hi),-1), 
              sd = sd(n_cohl),sd_lo = sd(n_cohl_lo), sd_hi = sd(n_cohl_hi),
              mean_inf = round(mean(n_link_infect),-1), 
              mean_inf_lo = round(mean(n_link_infect_lo),-1), mean_inf_hi = round(mean(n_link_infect_hi),-1), 
              sd_inf = sd(n_link_infect),sd_inf_lo = sd(n_link_infect_lo), sd_inf_hi = sd(n_link_infect_hi)) 
  
  
  
  #ggplot(cf_all%>% filter(sim_id == 1, name %in% c("n_cohl_lo","n_all"), cutoff == 5, r == "rt", detect_date < as.Date("2020-07-31")), aes(x=detect_date, y = value, group = interaction(name,cutoff))) + geom_bar(stat = "identity",pos = "fill", aes(fill = factor(name) )) 
  
  
  ### Supplementary table: additional results
  tcf_t %>% filter(r == 0.8, name %in% c("n_hosp_orig", "n_hoha", "n_missed"), sim_id == 1) %>% # simulation doesn't affect these underlying numbers
    pivot_wider(names_from = name, values_from = sum) 
  
  
  ## Looking at results
  cf_new <- cf_all %>% filter(r %in% c(0.8,1,1.2,"rt"), 
                              name %in% c("n_all", "n_hoha","n_coca","n_cf","n_coha","n_cohl","n_cohl_lo","n_cohl_hi"), 
                              detect_date < as.Date("2020-07-31")) %>%
    group_by(detect_date, r, name,cutoff,scen_so, disch_time) %>%
    summarise(mean_sims = mean(value, na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(r,name,cutoff,scen_so) %>% 
    mutate(mean_7=rollapply(mean_sims,7,mean,fill=NA)) %>%
    dplyr::select(-mean_sims) %>%
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = mean_7) %>%
    mutate(n_coca_orig = n_all - n_hoha) %>%
    dplyr::select(detect_date, cutoff, r, disch_time, scen_so,n_all, n_cf, n_hoha, n_coca, n_coca_orig,n_coha,n_cohl,n_cohl_lo,n_cohl_hi) %>% 
    pivot_longer(cols = n_all:n_cohl_hi)
  
  
  cf_new2 <- cf_new
  cf_new2$name <- factor(cf_new2$name, levels = c("n_all","n_coca_orig","n_coca","n_cf"))
  ggplot(cf_new2 %>% filter(cutoff == 5), aes(x=detect_date, y = value, group = name)) + geom_line(aes(col = name)) + 
    facet_grid(scen_so~disch_time + r) + 
    scale_color_brewer(palette = "RdYlBu", breaks = c("n_all","n_coca_orig","n_coca","n_cf"), 
                       labels = c("Total","COCA orig", "COCA (model)", "No HA")) + 
    scale_x_continuous("Detection date") + 
    scale_y_continuous("Number of cases")
  
  
  cf_new_withs <- cf_all %>% filter(cutoff == 5, r %in% c(0.8,1,1.2,"rt"), name %in% c("n_all", "n_hoha","n_coca","n_cf"), 
                                    detect_date < as.Date("2020-07-31")) %>%
    ungroup() %>% 
    group_by(r,name,sim_id,scen_so) %>% 
    mutate(mean_7=rollapply(value,7,mean,fill=NA)) %>%
    dplyr::select(-value) %>%
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = mean_7) %>%
    mutate(n_coca_orig = n_all - n_hoha) %>%
    dplyr::select(sim_id, detect_date, disch_time, scen_so, r, n_all, n_cf, n_coca, n_coca_orig) %>% 
    pivot_longer(cols = n_all:n_coca_orig)
  
  ### up to here corrected for symptom onset differences
  
  cf_new_withs$detect_date <- as.Date(cf_new_withs$detect_date)
  cf_new_withs$name <- factor(cf_new_withs$name, levels = c("n_all","n_coca_orig","n_coca","n_cf"))
  ggplot(cf_new_withs, aes(x=detect_date, y = value, group = interaction(sim_id,name))) + geom_line(aes(col = name)) + 
    facet_wrap(scen_so~disch_time + r) + 
    scale_color_brewer(palette = "RdYlBu", "",labels = c("Total","COCA orig", "COCA (model)", "No HA")) + 
    scale_x_date("Detection date") + 
    scale_y_continuous("Number of cases")
  ggsave(paste0(output_location,"/",name_o,"S_timeseries_total_coca_cf_allsims.jpeg"), width = 15)
  
  
  #### Community onset cases
  cf_new2 <- cf_new %>% filter(name %in% c("n_coca","n_coha","n_cohl","n_cohl_lo","n_cohl_hi"))
  
  cf_new2$name <- factor(cf_new2$name, levels = c("n_coca","n_coha","n_cohl","n_cohl_lo","n_cohl_hi"))
  ggplot(cf_new2 %>% filter(r %in% c("0.8", "rt")), aes(x=detect_date, y = value, group = name)) + 
    geom_line(aes(col = name)) + 
    facet_grid(r+scen_so ~ cutoff + disch_time) + 
    scale_color_manual("",values = c("#d7191c","#fdae61","#2171b5","#bdd7e7","#6baed6"), 
                       breaks = c("n_coca","n_coha","n_cohl","n_cohl_lo","n_cohl_hi"),
                       labels = c("COCA","COHA", "COHL", "COHL (low)","COHL (high)")) + 
    scale_x_date("Detection date") + 
    scale_y_continuous("Number of cases") + 
    ggtitle("Community onset cases")
  ggsave(paste0(output_location,"/",name_o,"S_timeseries_community.jpeg"), width = 10)
  
  ggplot(cf_new2 %>% filter(r %in% c("0.8", "rt")), aes(x=detect_date, y = value, group = name)) + geom_line(aes(col = name)) + 
    facet_grid(r + scen_so ~ cutoff + disch_time) + 
    scale_color_manual("",values = c("#d7191c","#fdae61","#2171b5","#bdd7e7","#6baed6"), 
                       breaks = c("n_coca","n_coha","n_cohl","n_cohl_lo","n_cohl_hi"),
                       labels = c("COCA","COHA", "COHL", "COHL (low)","COHL (high)")) + 
    scale_x_date("Detection date") + 
    scale_y_continuous("Number of cases", lim = c(0,200)) + 
    ggtitle("Community onset cases (zoom)")
  ggsave(paste0(output_location,"/",name_o,"S_timeseries_community_zoom.jpeg"), width = 10)
  
  ### proportions plot
  cf_new2 <- cf_new %>% filter(name %in% c("n_hoha","n_coha","n_cohl","n_all","n_comm_orig","n_cf","n_hosp_orig"))
  cf_new2$name <- factor(cf_new2$name, levels = c("n_hoha","n_coha","n_cohl","n_all","n_comm_orig","n_cf","n_hosp_orig"))
  cf_new %>% filter(r =="rt") %>% 
    filter(name %in% c("n_coca", "n_hoha","n_cohl","n_coha")) %>%
    ggplot(aes(x=detect_date, y = value, group = interaction(name,cutoff, r))) + 
    geom_bar(stat = "identity",position = "fill",aes(fill = name, colour = name)) + 
    facet_grid(scen_so~cutoff+disch_time, scales = "free")+ 
    scale_x_date("Detection date",limits=c(as.Date("2020-02-01"),as.Date("2020-07-31")), 
                 date_breaks = "1 month", date_labels = "%B") + 
    scale_fill_brewer(palette = "Set3","",breaks = c("n_hoha","n_coha","n_coca","n_cohl"),labels = c("HOHA","COHA","COCA","COHL")) + 
    scale_color_brewer(palette = "Set3","",breaks = c("n_hoha","n_coha","n_coca","n_cohl"),labels = c("HOHA","COHA","COCA","COHL")) + 
    scale_y_continuous("Proportion of hospital admissions at each time point")
  ggsave(paste0(output_location,"/",name_o,"S_timeseries_proportion_plot.jpeg"), width = 10)
  
  
  #### Impact of 1-5 day discharge
  cf_new15 <- store %>% filter(r == "rt", cutoff == 5,
                               detect_date < as.Date("2020-07-31")) %>%
    dplyr::select(c("detect_date", "sim_id","disch_time", "n_hoha","n_coca","n_coha")) %>%
    ungroup() %>% 
    pivot_longer(cols = n_hoha:n_coha) %>%
    group_by(detect_date,disch_time,name) %>% 
    summarise(mean_sims = mean(value, na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(disch_time, name) %>% 
    mutate(mean_7=rollapply(mean_sims,7,mean,fill=NA)) %>%
    dplyr::select(-mean_sims) %>%
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = mean_7) %>%
    dplyr::select(detect_date, disch_time, n_coca, n_coha,n_hoha) %>% 
    pivot_longer(cols = n_coca:n_hoha)
  
  cf_new15$name <- factor(cf_new15$name, levels = c("n_coca","n_hoha","n_coha"))
  ggplot(cf_new15, aes(x=detect_date, y = value, group = interaction(disch_time,name))) + 
    geom_line(aes(col = name, lty = factor(disch_time))) + 
    scale_color_discrete(breaks = c("n_coca","n_hoha","n_coha"), 
                         labels = c("COCA","HOHA","COHA")) + 
    scale_x_date("Detection date") + 
    scale_y_continuous("Number of cases") + 
    scale_linetype_discrete("Discharge time\nof missed cases") 
  ggsave(paste0(output_location,"/",name_o,"S_disch_time.jpeg"))

}

  
  