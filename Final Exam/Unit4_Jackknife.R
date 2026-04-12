#Lec example compute Jackknife estimate mean and variance 
data("sleepstudy", package = "lme4") 
n <- nrow(sleepstudy)
jack_mean <- numeric(n)
jack_var <- numeric(n)
for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i] #new sample is just the original but kill the i-th one
  jack_mean[i] <- mean(new_samp) #compute mean for each Jackknife sample
  jack_var[i] <- var(new_samp)
}

mean(jack_mean) #compute the jackknife estimate mean

mean(jack_var) # compute the jackknife estimate variance


#Lec example compute Jackknife estimate bias
data("sleepstudy", package = "lme4") 
n <- nrow(sleepstudy)
jack_est_bias <- numeric(n)

var_est <- (1/n) * sum((sleepstudy$Reaction - mean(sleepstudy$Reaction))^2)
for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i] # jackknife sampling
  jack_est_bias[i] <- (1/(n-1)) * sum((new_samp - mean(new_samp))^2) # compute sigma_hat for all jackknife sampling
}

sigma_hat_jack <- mean(jack_est_bias) 
(n-1)*(sigma_hat_jack - var_est) #compute the estimated bias



#Lec example compute jackknife estimated of the standard error of the median from a random 
#sample of 10 integers from 1, 2, 100
n <- 10
x <- sample(1:100, size = n)

jack_median <- numeric(n)
for (i in 1 : n ){
  new_samp <- x[-i]
  jack_median[i] <- median(new_samp)
}

theta_hat_jack <- mean(jack_median)

sqrt((n-1)/ n * sum((jack_median - theta_hat_jack)^2)) # this is the estimated standard error
# for median, how ever you get different answer each time which it failed

