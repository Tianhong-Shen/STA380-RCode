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
mean(pc) # it will return something around 0.39871, which is smaller than 0.5
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
# standard error is sqrt(var(x) / n) we are using the unbias version in here

lower <- mu_x - qt(alpha / 2, df = n-1, lower.tail = FALSE) * se_x
upper <- mu_x + qt(alpha / 2, df = n-1, lower.tail = FALSE) * se_x

y <- ifelse((theta < upper) & (theta > lower), 1, 0)
mean(y) # Expect to get a value around 0.95



# unit 2 practice problems Q1 part b
n <- 10^5
alpha <- 2
beta <- 2
x <- rgamma(n, shape = alpha, scale = beta)

cond <- (x < 2)
p_hat <- mean(cond)

se <- sqrt(sum((cond - p_hat)^2)) / n

lower <- p_hat - qnorm(0.05, lower.tail = FALSE) * se
upper <- p_hat + qnorm(0.05, lower.tail = FALSE) * se

CI <- c(lower, upper)
CI



# unit 2 practice problems Q2 
n <- 10^5
u <- runif(n, 0, 10)
theta_hat <- mean(10 * u * log(u)) # estimate theta using simple monte carlo

#compute exact expression for var(theta_hat)
fun1 <- function(x){
  ((x * log(x))^2) / 10
}
fun2 <- function(x){
  (x*log(x)) / 10
}

part1 <- integrate(fun1, lower = 0, upper = 10)
part2 <- integrate(fun2, lower = 0, upper = 10)

var <- (100 / n) * (part1$value - ((part2$value)^2))
var

# use monte carlo to estimate var_hat
var_hat <- sum((10*u*log(u) - mean(10*u*log(u)))^2) / (n^2)
var_hat



# unit 2 practice problems Q3
n <- 10^4
u <- runif(n, 0, 10)
theta_hat <- mean(10 * exp(u) * log(u))

fun1 <- function(x){
  ((exp(x) * log(x))^2) / 10
}

fun2 <- function(x){
  (exp(x) * log(x)) / 10
}

part1 <- integrate(fun1, lower = 0, upper = 10)
part2 <- integrate(fun2, lower = 0, upper = 10)

var <- (100 / n) * (part1$value - (part2$value)^2)

var_hat <- sum((10*exp(u)*log(u) - mean(10*exp(u)*log(u)))^2) / (n ^ 2)

var ; var_hat



# Quiz 4 practice problem Q1
#install.packages("extraDistr")
#library("extraDistr")
alpha <- 3
beta <- 2
n <- 100
m <- 10^4

# generate y^(1), ..., y^(m) with n sample size in each y^(j)
y <- matrix(extraDistr::rpareto(n*m, a = alpha, b = beta), nrow = m)

# compute Monte Carlo estimate for alpha_hat_mome
alpha_mome <- mean(rowMeans(y) / (rowMeans(y) - 2))

# compute Monte Carlo estimate for alpha_hat_MLE
alpha_MLE <- mean(n / (rowSums(log(y)) - n * log(beta)))

alpha_MLE ; alpha_mome ; alpha # since alpha_MLE is closer to alpha, it is better



# Term Test 2 practice version A Q1 part b 
n <- 10^5
theta <- 2
x <- rexp(n, rate = 1 / theta)

cond <- (x < 3)
p_hat <- mean(cond)

se <- sqrt(sum((cond - p_hat)^2)) / n

lower <- p_hat - qnorm(0.005, lower.tail = FALSE) * se
upper <- p_hat + qnorm(0.005, lower.tail = FALSE) * se

CI <- c(lower, upper)



# Term Test 2 practice version A Q2 
n <- 10^4
x <- rnorm(n, mean = 0, sd = 3)
theta_hat <- mean(2 * sin(x ^ 2))

theta_hat


# Term Test 2 practice version A Q4
n <- 10
m <- 10^4
theta <- 1

x <- matrix(rnorm(n*m, mean = theta, sd = theta), nrow = m)

theta1_hat <- rowMeans(x)
theta2_hat <- sqrt((1 / (n-1)) * rowSums((x - rowMeans(x))^2))

pc <- mean(abs(theta1_hat - theta) <= abs(theta2_hat - theta))

pc # return a value around 0.4108, since it is less than 0.5, then we say 
# theta2_hat is pitman closer to theta










