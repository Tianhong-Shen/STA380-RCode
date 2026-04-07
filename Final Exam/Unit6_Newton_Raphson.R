#Lec Example use Newton-Raphson method to solve the mle for gamma
samp <- rgamma(10^4, shape = 5, rate = 2)

log_likelihood <- function(theta, sample){
  alpha <- theta[1]
  lambda <- theta[2]
  if (alpha < 0 || lambda < 0){
    return(-Inf) # in this time we don't have to -log_likelihood
  }
  
  n <- length(sample) 
  
  part1 <- n*alpha*log(lambda) - n*lgamma(alpha) 
  part2 <- (alpha - 1)*sum(log(sample)) - lambda*sum(sample)
  return(part1+part2) # same reason as above
}

newton_gamma <- function(theta0, sample, tol = 0.01){
  theta <- theta0 # starting point
  no_root <- TRUE # when it is false we stop the while loop and return theta^(n)
  while(no_root){
    L <- numDeriv::grad(log_likelihood, theta, sample = sample)
    #this is the gradient of ur log_likelihood of gamma
    H <- numDeriv::hessian(log_likelihood, theta, sample = sample)
    #this is the hessian of ur log_likelihood of gamma
    theta_n <- theta - solve(H, L) 
    #this is our step 2 theta^(n) = theta^(n-1)-H(theta^(n-1))L(theta^(n-1))
    if (sqrt(sum((theta_n - theta)^(2))) <= tol){
      no_root = FALSE
      # this is from step 3 if || theta^(n) - theta^(n-1) || < epsilon then we stop
    }
    theta <- theta_n #update our theta each time
  }
  return(theta)
}

newton_gamma(theta0 = c(2, 1), sample = samp) # now assume the starting point is (2, 1)



# Lec Example use newton raphson method to find mle for mu and sigma

samp <- rnorm(10^4, mean = 2, sd = 4)

log_likelihood <- function(theta, sample){
  mu <- theta[1]
  sigma <- theta[2]
  
  if (sigma < 0 ){
    return(-Inf)
  }
  
  n <- length(sample)
  
  part1 <- -(n/2)*log(2*pi*(sigma^2))
  part2 <- -(1/(2*(sigma^2)))*sum((sample - mu)^2)
  
  return(part1+part2)
}


newton_normal<- function(theta0, sample, tol = 0.01){
  theta <- theta0
  no_root <- TRUE
  while(no_root){
    L <- numDeriv::grad(log_likelihood, theta, sample = sample)
    H <- numDeriv::hessian(log_likelihood, theta, sample = sample)
    
    theta_n <- theta - solve(H, L)
    if (sqrt(sum((theta_n - theta)^2)) <= tol){
      no_root <- FALSE
    }
    theta <- theta_n
  }
  return(theta)
}

newton_normal(theta0 = c(2,1), sample = samp)





