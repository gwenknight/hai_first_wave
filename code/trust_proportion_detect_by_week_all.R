#### Calculation of proportion detected by Trust by week 

### use nosocomialdetection_functions_v2.R to 
### combine los distribution with a constant harzard
### to calculation the proportion detected in each Trust by week 

setwd(here::here())

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
cutoffs <- c(5, 8, 10,14)
# Probability distribution for incubation period
# p1<-1.621
# p2<-0.418
p1 <- time_inf_to_symp_mean # 1.63 # Use McAloon distribution log(5.5^2 / sqrt(2.4^2 + 5.5^2)) 
p2 <- time_inf_to_symp_sd  # 0.5 # Use McAloon distribution sqrt(log(1 + (2.4^2)/(5.5^2)))

p1_sd <- time_inf_to_symp_mean_sd #= 0.0640, # Lauer
p2_sd <- time_inf_to_symp_sd_sd # = 0.0691 # Lauer

## How many samples? 
nsamps = 200


####*** RUN 
# Run thru each Trust and week
nn <- c()
for(i in trusts){
  nn_trust <- c()
  for(wk in 1:53){
    los_trust <- los %>% filter(place == i, week == wk) 
    los_trust_values <- rep(los_trust$los.days, los_trust$total_admin)
    
    for(smple in 1:nsamps){
      print(c(i,wk, smple))
      
      # SAMPLE incubation period distribution
      p1_s <- rnorm(1, mean = p1, sd = p1_sd)
      p2_s <- rnorm(1, mean = p2, sd = p2_sd)
      # Max day
      maxday<-max(los_trust$los.days)
      
      cum_prob_inc <- plnorm(1:maxday,p1_s,p2_s)
      prob_inc <- cum_prob_inc-c(0,cum_prob_inc[1:(maxday-1)])
      #sum(prob_inc * seq(1,maxday,1)) 
      
      # SAMPLE LOS 
      los_sample <- sample(los_trust_values, length(los_trust_values), replace = TRUE) # equally as quick to replicate this at start and sample in
      
      for(c in cutoffs){
        pdd <- nosocomial.detection(los_trust[,c("los.days","prop")], prob_inc, c)
        if(length(pdd)>0){nn_trust <- rbind(nn_trust, c(wk,c,pdd$res, pdd$res_bc, pdd$res_ad, smple))
        }else{nn_trust <- rbind(nn_trust, c(wk,c,0))}
      }
      
    }
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
  nn <- rbind(nn, nn_trust)
}

nn <- as.data.frame(nn)
colnames(nn) <- c("week","cutoff","prop_detect","prop_bc","prop_ad","sample","place")

### Save for use in percentage contribution calculation
write.csv(nn,"output_all_trusts/trusts_prop_detect.csv")

### Plot outputs
nnp <- nn %>% filter(place == "ENG") %>% pivot_longer(cols = c("prop_detect","prop_bc","prop_ad"))
nnp$value <- as.numeric(nnp$value)
ggplot(nnp, aes(x=week, y = value, group = interaction(sample, cutoff,name))) + 
  geom_line(aes(col = name)) + 
  facet_wrap(~cutoff) + 
  scale_y_continuous("Proportion of infections") + 
  scale_color_discrete("Symptom onset", breaks = c("prop_ad","prop_bc","prop_detect"), labels = c("After discharge","Before cutoff","Detected\n(after cutoff)"))
ggsave("output_all_trusts/ENG_prop_detected_by_type.pdf")

# mean over samples
nn_mean <- nn %>% group_by(place, week, cutoff) %>%
  summarise(mean_prop_detect = mean(prop_detect))
  
ggplot((nn_mean %>% filter(!place %in% c("ENG","ENG1"))), aes(x=cutoff, y = mean_prop_detect, group =interaction(cutoff)))+ geom_point() + 
  geom_boxplot() + #facet_wrap(~week) + 
  scale_x_continuous("Cutoff threshold") +  
  scale_y_continuous("Proportion detected") #, lim = c(0,0.45))
ggsave("output_all_trusts/prop_detected_by_threshold_boxplot.pdf")

ggplot(nn_mean %>% filter(!place %in% c("ENG","ENG1")), aes(x=week, y = mean_prop_detect, group =interaction(week,cutoff)))+ geom_point() + 
  geom_boxplot() + facet_wrap(~cutoff) + 
  scale_x_continuous("Week",lim = c(0,35)) +  
  scale_y_continuous("Proportion detected") 
ggsave("output_all_trusts/prop_detected_by_week_boxplot.pdf")


g1 <- ggplot(nn_mean %>% filter(!place %in% c("ENG","ENG1"))%>%filter(cutoff==8), aes(x=cutoff, y = mean_prop_detect, group =interaction(cutoff)))+ geom_point() + 
  geom_boxplot() + #facet_wrap(~week) + 
  scale_x_continuous("Cutoff threshold", breaks = c(8)) +  
  scale_y_continuous("Proportion detected")

g2 <- ggplot(nn_mean %>% filter(!place %in% c("ENG","ENG1"))%>%filter(cutoff==8), aes(x=week, y = mean_prop_detect, group =interaction(week,cutoff)))+ geom_point() + 
  geom_boxplot() + facet_wrap(~cutoff) + 
  scale_x_continuous("Week",lim = c(0,35)) +  
  scale_y_continuous("Proportion detected") 

g2 + g1 + plot_layout(ncol=2,widths=c(3,1)) + plot_annotation(tag_levels = 'A')
ggsave("output_all_trusts/prop_detected_by_week_boxplot_joint.jpeg")

nn%>% filter(!place %in% c("ENG","ENG1")) %>% summarise(min(prop_detect))
nn%>% filter(!place %in% c("ENG","ENG1")) %>% summarise(max(prop_detect))

nn %>% filter(cutoff == 5, place == "ENG", week < 31) %>% summarise(mean(prop_detect), min(prop_detect),max(prop_detect))
nn_mean%>% filter(place == "ENG", week < 31) %>% summarise(mean(prop_detect), min(prop_detect),max(prop_detect))
ggplot(nn %>% filter(place == "ENG"), aes(x=week, y = prop_detect, group = cutoff)) + 
  geom_line(aes(col = factor(cutoff)))
