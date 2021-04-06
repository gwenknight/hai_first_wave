#### NATURAL HISTORY PARAMETERS

### KEY PARAM ************************************************************************************************************
# Proportions of infections that end up in hospital 
#prop_miss_hosp_mean <- 0.15 # infection to hospitalization
prop_miss_hosp_min <- 0.1 
prop_miss_hosp_max <- 0.15 

# From Imperial report 41: IHR 3.52 95CrI = 3.29 - 3.72 
prop_comm_hosp_mean <- 0.035 # infection to hosp in non-recently admitted
prop_comm_hosp_sd <- 0.0005

# r <- rnorm(10000, 100*prop_comm_hosp_mean, 100*prop_comm_hosp_sd)
# c(mean(r), range(r)) # 3.52 3.29 - 3.72

# Time to hospitaliation = incubation (5 days) + symptom -> hospitlisation (7)
## EPISOON: LogNormally distributed with mean 5.5 days and standard deviation 2.4 days
time_inf_to_symp_mean <- 1.62 # Use Lauer distribution log(5.5^2 / sqrt(2.4^2 + 5.5^2)) 
time_inf_to_symp_sd <- 0.418 # Use Lauer distribution sqrt(log(1 + (2.4^2)/(5.5^2)))

time_inf_to_symp_mean_sd = 0.0640 # Lauer
time_inf_to_symp_sd_sd = 0.0691 # Lauer



# mean(rlnorm(1000, time_inf_to_symp_mean,time_inf_to_symp_sd))
# sd(rlnorm(1000, time_inf_to_symp_mean,time_inf_to_symp_sd))

## DAVIES MODEL: We assumed gamma-distributed delays, with the shape parameter set equal to the mean, for: 
# delay from symptom onset to hospitalisation of mean 7 days (standard deviation 2.65) 
# Now use fit to CO-CIN (filter for first wave i.e. symptom onset pre 1st August, and 
# fit gamma distribution to difference between onset and admission date)

#### Scen 1: fit to raw data (smoothing makes little difference)
time_symp_to_hosp_meanlog <- 1.664959
time_symp_to_hosp_sdlog   <- 0.893762

#### Scen 2: previous gamma (davies et al)
time_symp_to_hosp_shape <- 7 
time_symp_to_hosp_scale <- 1 

#### Scen 3: # FF100 data : lnorm
# FF100 data 
time_symp_to_hosp_meanlog3 <- 1.44
time_symp_to_hosp_sdlog3  <- 0.72


#mean(rgamma(1000, shape = time_symp_to_hosp_shape, scale = time_symp_to_hosp_scale))
#sd(rgamma(1000, shape = time_symp_to_hosp_shape, scale = time_symp_to_hosp_scale))

# # DAVIES model: infectious period: gamma(mu=3.5, k = 4)
# #With a shape parameter k and a mean parameter μ = kθ = α/β.
infectious_shape <- 4
infectious_scale <- 3.5/4
# #sd(rgamma(1000, shape = infectious_shape, scale = infectious_scale))

## What proportion are symptomatic / infectious and by what amount? 
#we_inf <- 0.5*0.5 + 0.5*1 # weighted average: assume 50% preclinical, 50% as infectious
we_inf <- 1 # not looking at any asymptomatics

## Readmissions for non-COVID: 14%. 90% of these within 14 days
prop_readm_nc <- 0.9*0.14
dist_readm_nc <- read.csv("data/nuff_readm.csv")


# FIXED natural history parameters
params = list(
  prop_miss_hosp_min <- 0.1, 
  prop_miss_hosp_max <- 0.15, 
  prop_comm_hosp_mean = 0.035, # infection to hosp in non-recently admitted
  prop_comm_hosp_sd = 0.1, # infection to hosp in non-recently admitted
  time_inf_to_symp_mean <- log(5.5^2 / sqrt(2.4^2 + 5.5^2)),
  time_inf_to_symp_sd <- sqrt(log(1 + (2.4^2)/(5.5^2))),
  time_inf_to_symp_mean_sd = 0.0640, # Lauer
  time_inf_to_symp_sd_sd = 0.0691, # Lauer
  time_symp_to_hosp_shape = 1.453, 
  time_symp_to_hosp_scale = 5.348,
  infectious_shape = 4,
  infectious_scale = 3.5/4,
  we_inf = 1, 
  prop_readm_nc = 0.9*0.14,
  dist_readm_nc <- dist_readm_nc
)
