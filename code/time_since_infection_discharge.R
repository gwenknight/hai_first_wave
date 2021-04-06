# =========================================================================== #
# Probability distribution for time since infection until hospital discharge
# =========================================================================== #
# INPUT
# los_distr = probability distribution for LOS
# OUTPUT
# res = Probability distribution for time since infection until discharge
# Note: 
# We assume that given a LOS, the time of infection is equally likely on 
# each day of LOS
time.infection.discharge <- function(los_distr){
  mean_los <- sum((1:length(los_distr))*los_distr)
  res <- rep(0,length(los_distr))
  for(t in 1:length(los_distr)){
    for(l in t:length(los_distr)){
      res[t] <- res[t] + los_distr[l]/mean_los
    }
  }
  return(res)
}

time.infection.discharge.05 <- function(los_distr){
  mean_los <- sum(c(0.5,seq(1:(length(los_distr)-1)))*los_distr)
  res <- rep(0,length(los_distr))
  for(t in 1:length(los_distr)){
    for(l in t:length(los_distr)){
      res[t] <- res[t] + los_distr[l]/mean_los
    }
  }
  return(res)
}

# # EXAMPLE
# # Exponential probability distribution for LOS
# meanlos <- 1
# maxday <- 30
# cum_prob_los <- pexp(1:maxday,1/meanlos)
# #prob_los <- cum_prob_los-c(0,cum_prob_los[1:(maxday-1)])
# discrete.meanlos<-sum(prob_los*(1:maxday))
# 
# # Probability distribution for time since infection until hospital discharge
# time.infection.discharge(prob_los)
# 
# s <- sample(x=seq(1,maxday,1),size = 1000, prob = time.infection.discharge(prob_los),  replace = TRUE)
# 
# # Normal probability distribution for LOS
# meanlos <- 1
# maxday <- 30
# cum_prob_los <- pnorm(1:maxday,mean = meanlos, sd = 0.1)
# #prob_los <- cum_prob_los-c(0,cum_prob_los[1:(maxday-1)])
# discrete.meanlos<-sum(prob_los*(1:maxday))
# 
# # Probability distribution for time since infection until hospital discharge
# r <- time.infection.discharge(prob_los)
# sum(r - prob_los)
# r
# prob_los
