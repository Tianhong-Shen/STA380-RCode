# Lec Example use optimize() to max f(x)

f <- function(x){
  log(1+log(x)) / log(1+x)
}

op <- optimize(f, lower = 2.4, upper = 8, maximum = TRUE)

op

op$maximum # this will give you the x value when the function reach max
op$objective # this will give you the maximum of the function
f(op$maximum) == op$objective #we should expected this return true



# Lec Example find MLE for alpha

n <- 10^4
alpha <- 3 # you can take any positive value you want
beta <- 2

samp <- extraDistr::rpareto(n, a = alpha, b = beta)

alpha_mle <- n / (sum(log(samp)) - n*log(beta)) # this is the mle we derived
alpha_mle
  
log_likelihood <- function(x, n, alpha){
  n*log(alpha) + alpha*n*log(2) - (alpha - 1)*sum(log(x))
}

op <- optimize(log_likelihood, lower = 2, upper = 10, n = n, x = samp, maximum = TRUE)
op$maximum #this will also give you the alpha_mle it should be a value close to
#alpha_mle and ture alpha = 3




#Lec Example find maximum of the log likelihood function of gamma

n <- 10^4
alpha <- 5
lambda <- 2

samp <- rgamma(n, shape = alpha, rate = lambda)

log_likelihood <- function(theta, sample){
  alpha <- theta[1]
  lambda <- theta[2]
  if (alpha < 0 || lambda < 0){
    return(Inf)
  } # we need this because alpha and lambda should all be positive value
  
  n <- length(sample) # n is the sample size
  
  part1 <- n*alpha*log(lambda) - n*lgamma(alpha) #lgamma in here is just log(gamma(alpha))
  part2 <- (alpha - 1)*sum(log(sample)) - lambda*sum(sample)
  return(-(part1+part2)) #we should return the negative of log_likelihood function
  #because optim() can only compute minimum 
}

# c(1,1) is the starting point
op <- optim(c(1, 1), sample = samp, log_likelihood, method = "Nelder-Mead")#there
# are 5 possible method this is the default one
op$par # this will give you the value for alpha_mle and lambda_mle



# Lec Example use optim() to find mle for mu and sigma

samp <- rnorm(10^4, mean = 2, sd = 4)

log_likelihood <- function(theta, sample){
   mu <- theta[1]
   sigma <- theta[2]
   
  if (sigma < 0 ){
    return(Inf)
  }
  
   n <- length(sample)
   
   part1 <- -(n/2)*log(2*pi*(sigma^2))
   part2 <- -(1/(2*(sigma^2)))*sum((sample - mu)^2)
   
   return(-(part1+part2))
}

op <- optim(c(1, 1), sample = samp, log_likelihood)
op$par










