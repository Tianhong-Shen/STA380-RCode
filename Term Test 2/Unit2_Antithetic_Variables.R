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













