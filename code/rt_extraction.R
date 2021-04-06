#### R wrangling

### Using truncated as slightly shorter (8 days)
# FROM: https://github.com/epiforecasts/rt-comparison-uk-public/tree/master/rt-estimate/estimate-all-time

r <- readRDS("data/summary_truncated.rds")
ggplot(r %>% filter(region == "England",source == "cases_hosp"), aes(x=date, y = median, group = source)) + 
  geom_line() + 
  scale_x_date("Date") + scale_y_continuous("Median") 
ggsave("rt.jpeg")

# Use hospitalised cases as
#Rt from test-positive cases is typically more variable, and vulnerable to random testing biases (e.g. targeted testing in a cluster)
#Rt from admissions is generally more reliable (and more up to date than deaths)
#But if you think the average transmission is shifting towards younger age groups then the admissions Rt might not be as useful for prediction as that from test-positive cases.

rt <- r %>% filter(region == "England", source == "cases_hosp") %>% dplyr::select(c("date", "median","lower_50","upper_50"))


mm <- min(rt$date)
mmv <- rt[1,c("median","lower_50", "upper_50")]
mx <- max(rt$date)
mxv <- rt[dim(rt)[1],c("median","lower_50", "upper_50")]

rt <- rt[rt[,.(date = seq.Date(as.Date("2019-09-01"), as.Date("2020-12-31"), by = "day"))], on = .(date)]

# front fill
w <- which(rt$date == mm) - 1
rt[1:w,c("median","lower_50", "upper_50")] <- mmv
# back fill
w <- which(rt$date == mx) + 1
rt[w:dim(rt)[1],c("median","lower_50", "upper_50")] <- mxv


write.csv(rt,"data/rt.csv")

### RT exploration 
mean(rt$median) # rt = 1.8
ggplot(rt %>% pivot_longer(cols = c("median","lower_50", "upper_50")), aes(x=date, y = value, group = name)) + 
  geom_line(aes(col = name)) + 
  scale_x_date("Detection date",limits=c(as.Date("2020-02-01"),as.Date("2020-09-14")), 
               date_breaks = "1 month", date_labels = "%B") + 
  scale_y_continuous("R estimates (median, Epiforecasts)",lim = c(0,2)) + 
  geom_hline(yintercept = 1,lty =  "dashed") + 
  geom_vline(xintercept = c(mm, mx), lty = "dotted") + 
  scale_color_discrete("")
ggsave("output_figs/rt_epiforecasts.jpeg")
