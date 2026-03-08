# Lec Example integrate(1/(1+x)dx, lower = 0, upper = 1) using Antithetic variables
n <- 10^5
u <- runif(n/2) # remember it should be n/2 instead of n for antithetic variables

theta_hat <- mean(((1/(1+u))+(1/(1+(1-u))))/2) # using antithetic variable to estimate

# compare to simple Monte carol estimation
u1 <- runif(n)
theta_monte <- mean(1/(1+u1))

theta_hat
theta_monte

# compare to the actual answer you can see that antithetic is better
abs(theta_hat - log(2))
abs(theta_monte - log(2))



#Lec example integrate(standard normal, lower = -inf, upper = x) x is some unknown constant
n <- 10^5
u <- runif(n/2)
x <- 2.9

g1 <- x/sqrt(2*pi) * exp(-(x^2) * (u^2) / 2)
g2 <- x/sqrt(2*pi) * exp(-(x^2) * ((1- u)^2) / 2)


theta_hat <- 0.5 + mean((g1+g2)/2) # this is for x is bigger than 0

# if x is smaller that 0, we know that phi(x) = 1 - phi(-x)

x <- -3.2

g1 <- (-x)/sqrt(2*pi) * exp(-(x^2) * (u^2) / 2)
g2 <- (-x)/sqrt(2*pi) * exp(-(x^2) * ((1- u)^2) / 2)

theta_hat <- 1 - (0.5 + mean((g1+g2)/2))


#compare with the actual value

phi <- pnorm(x)

library(testthat)
test_that("compare pnorm", {
  expect_equal(phi, theta_hat, tol = 0.01)
})



# unit 2 practice problems Q6
n <- 10^5
theta <- 2
u <- runif(n/2)

part1 <- (u / theta) * exp(-u / theta)
part2 <- ((1-u) / theta) * exp(-(1-u) / theta)

theta_hat <- mean((part1 + part2) /2)
true_val <- theta - (theta + 1) * exp(-1 / theta)
theta_hat ; true_val



# Term Test 2 practice version A Q3
n <- 10^4
u <- runif(n)
theta <- 1
  
g1 <- (1 / theta) * exp(-u / theta)
g2 <- (1 / theta) * exp(-(1 - u) / theta)

theta_hat <- mean((g1+g2) / 2)

true_val <- 1- exp(- 1 / theta)

theta_hat ; true_val






