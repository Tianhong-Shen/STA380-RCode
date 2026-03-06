#Lec example find E(abs(X1 - X2)), where X1 and X2 are iid standard normal
m <- 10^5
x1 <- rnorm(m)
x2 <- rnorm(m)
y <- abs(x1 - x2)

theta_hat <- mean(y)

# Compare with actrual value

library(testthat)

test_that("compare monte carlo estimation of mean", {
  expect_equal(theta_hat, 4/sqrt(4*pi), tol = 0.001)
})



# Lec Example Estimate standard error of abs(X1 - X2)
n <- 10^5
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- abs(x1 - x2)

# compute true value
se_true <- sqrt((2 - 4/pi) / n)

# using Monte Carlo estimation(bias version)
se_mc_bias <- sqrt(sum((y - mean(y))^2)) / n

# unbias version
se_mc_unbias <- sqrt(sum((y - mean(y))^2) / (n * (n-1)))

test_that("compare monte carlo estimation  of variance", {
  expect_equal(se_true, se_mc_bias, tol = 0.001)
})

test_that("compare monte carlo estimation  of variance", {
  expect_equal(se_true, se_mc_unbias, tol = 0.001)
})



#Lec Example x1,x2, ... , xn ~ iid N(theta, theta^2) estimate theta by 
#theta_1_hat = 1/n * sum(xi)
#theta_2_hat = sqrt(1/(n-1) * sum((xi - mean(x))^2)
#compute MSE for both estimator

m <- 10^5 # number of monte carlo simulation
n <- 30 # sample size
theta <- 3

x <- matrix(rnorm(n * m , mean = theta, sd = theta), byrow = FALSE, nrow = m)

theta_1_hat <- rowMeans(x)
theta_2_hat <- sqrt(1/(n-1) * rowSums((x - rowMeans(x))^2))

mse1 <- mean((theta_1_hat - theta)^2)
mse2 <- mean((theta_2_hat - theta)^2)

mse2 < mse1 # it will return true, which theta_2_hat is a better estimator



# Lec Example, same setting as above question but using Pitman closer to compare
# theta_1_hat and theta_2_hat
m <- 10^5 # number of monte carlo simulation
n <- 30 # sample size
theta <- 3

x <- matrix(rnorm(n * m , mean = theta, sd = theta), byrow = FALSE, nrow = m)

theta_1_hat <- rowMeans(x)
theta_2_hat <- sqrt(1/(n-1) * rowSums((x - rowMeans(x))^2))

pc <- abs(theta_1_hat - theta) <= abs(theta_2_hat - theta)
sum(pc) / m # it will return something around 0.39871, which is smaller than 0.5
# then theta_2_hat is said to be pitman closer to theta, which theta_2_hat is 
# better estimator



# Lec Example estimate Confidence Interval generate a normal sample of size 30 with
# mean 5 and variance 2, using 10^4 replicates, compute the empirical confidence level
# of mu_hat = sum(xi) given alpha = 0.05
theta <- 5 # mu
m <- 10^4
n <- 30 
alpha <- 0.05

x <- matrix(rnorm(n*m, mean = theta, sd = sqrt(2)), nrow = m)

mu_x <- rowMeans(x)

se_x <- sqrt((1/(n-1)) * rowSums( (x - rowMeans(x))^(2)) ) / sqrt(n)# because
# standard error is sqrt(var(x) / n) 

lower <- mu_x - qt(alpha / 2, df = n-1, lower.tail = FALSE) * se_x
upper <- mu_x + qt(alpha / 2, df = n-1, lower.tail = FALSE) * se_x

y <- ifelse((theta < upper) & (theta > lower), 1, 0)
mean(y) # Expect to get a value around 0.95










