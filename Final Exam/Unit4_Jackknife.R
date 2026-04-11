data("sleepstudy", package = "lme4") 
jack_mean <- numeric(n)
jack_var <- numeric(n)
for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i] #new sample is just the original but kill the i-th one
  jack_mean[i] <- mean(new_samp) #compute mean for each Jackknife sample
  jack_var[i] <- var(new_samp)
}

mean(jack_mean) #compute the jackknife estimate mean

mean(jack_var)
