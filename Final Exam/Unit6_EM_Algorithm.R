# Lec Example use EM-algo to compute poisson(tau)

n <- 10^4
incomp_samp <- rpois(n-1, lambda = 3) #generate incomplete sample with x_-1 missing

em <- function(sample, tol = 0.01){
  tau <- sum(sample) / length(sample) # starting point
  no_sol <- TRUE
  while(no_sol){
    new_tau <- (tau + sum(sample)) / (length(sample)+1)
    if (sqrt(sum((new_tau - tau)^2)) <= tol){
      no_sol <- FALSE
    }
    tau <- new_tau
  }
  return(tau)
}

em(sample = incomp_samp)



#Lec Example use em-algo to compute mu for a normal with x_-1 missing and sigma is known
n <- 10^4
sigma <- 4
incomp_samp <- rnorm(n-1, mean = 2, sd = sigma)

em <- function(sample, tol = 0.01){
  mu <- sum(sample) / length(sample)
  no_sol <- TRUE
  while(no_sol){
    new_mu <- (mu + sum(sample)) / (length(sample)+1)
    if (sqrt(sum((new_mu - mu)^2)) <= tol){
      no_sol <- FALSE
    }
    mu <- new_mu
  }
  return(mu)
}

em(sample = incomp_samp)
















