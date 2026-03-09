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

type2 <- mean(p_val >= alpha) #type II is fail to reject | Ha is true
power <- 1- type2 # power = 1 - type II error 

type2
power # as increase abs(mu1 - mu0) power increase, decrease sigma power increase
# increase sample size power increase



# Quiz 4 practice problem Q2

# compute rejection region
k_star <- qgamma(0.1, shape = 10, scale = 3, lower.tail = FALSE)

k_star

# compute type I error rate
n <- 10
m <- 10^5
alpha <- 0.1
theta <- 3

x <- matrix(rexp(n*m, rate = 1 / theta), nrow = m)

# compute test statistic
test_stat <- rowSums(x)

type1 <- mean(test_stat > k_star)

type1

# compute type II error rate
set.seed(1)
n <- 10
m <- 10^5
theta_alter <- 4
alpha <- 0.1
x <- matrix(rexp(n*m, rate = 1 / theta_alter), nrow = m)
#compute critical value under allternative hypothesis
k <- qgamma(alpha, shape = 10, scale = 3, lower.tail = FALSE)
test_stat <- rowSums(x)
type2 <- mean(test_stat < k) ; power = 1 - type2

type2 ; power

# another way to compute type II error rate (personally prefer this way!)
n <- 10
m <- 10^5
theta_alter <- 4
theta_null <- 3
alpha <- 0.1
x <- matrix(rexp(n*m, rate = 1 / theta_alter), nrow = m)
test_stat <- rowSums(x)
k <- qgamma(alpha, shape = n, scale = theta_null, lower.tail = FALSE)

type2 <- mean(test_stat <= k); power = 1 - type2

type2; power





# Term Test 2 practice version A Q5
n <- 20
m <- 10^4
theta <- 5

# find critical value
k <- qgamma(0.05, shape = 20, scale = 5, lower.tail = FALSE)

x <- matrix(rexp(n*m, rate = 1/theta), nrow = m)

# find test statistic
test_stat <- rowSums(x)

# type I error rate
type1 <- mean(test_stat > k)
type1















