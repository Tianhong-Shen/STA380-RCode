#Lec Example type I error rate  X1, X2 ... X20 ~N(mu, 10), h0: mu = 500 vs Ha: mu != 500
# at alpha = 0.05, estimator is X_bar, compute type I error

mu0 <- 500
sigma <- 100
alpha <- 0.05
n <- 20 
m <- 10^5

x <- matrix(rnorm(n*m, mean = mu0, sd = sigma), nrow = m)

test_stat <- (rowMeans(x) - mu0) / (sigma / sqrt(n))

p_val <- 2 * (pnorm(abs(test_stat), lower.tail = FALSE)) # compute 
# p_value = 2 * P(z >|test_stat|)

type1 <- mean(p_val < 0.05)
type1 # should be some number around aplha = 0.05



# Lec Example same set up as above this time we estimate type II error with mu1 = 600
mu0 <- 500
mu1 <- 600
sigma <- 100
alpha <- 0.05
n <- 20 
m <- 10^5

x <- matrix(rnorm(n*m, mean = mu1, sd = sigma), nrow = m) # now X ~ N(600, 10)

test_stat <- (rowMeans(x) - mu0) / (sigma / sqrt(n))

p_val <- 2 * (pnorm(abs(test_stat), lower.tail = FALSE))

type2 <- mean(p_val > alpha) #type II is fail to reject | Ha is true
power <- 1- type2 # power = 1 - type II error 

type2
power # as increase abs(mu1 - mu0) power increase, decrease sigma power increase
# increase sample size power increase













