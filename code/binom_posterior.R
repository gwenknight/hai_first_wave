##### 

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


# ### Example use
# ### x = number of nosocomial cases 
# ### p = probability of detection 
# ### prior = flat ok 
# sum(binom.ntrial.pos(100, 0.6, seq(0.1,1, length = 2000))*seq(1:2000))
# sum(binom.ntrial.pos(100, 0.6, seq(0.1,10, length = 2000))*seq(1:2000))
# sum(binom.ntrial.pos(100, 0.3, rep(0.2,100)))
# sum(binom.ntrial.pos(100, 0.3, rep(0.1,100)))
