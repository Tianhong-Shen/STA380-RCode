#Lecture Example X ~ N(0, 1), use hit or miss to estimate P(x > 2) and p(0 < x < 1)
n <- 10^5
x <- rnorm(n)
f1 <- mean(x > 2) #compute F(x)_hat for P(x > 2)
f2 <- mean((0 < x) & (x < 1)) # compute F(x)_hat for P(0 < x < 1)

library(testthat)
test_that("Compare to pnorm()", {
  expect_equal(f1, pnorm(2, lower.tail = FALSE), tol = 0.01)
  expect_equal(f2, pnorm(1, lower.tail = TRUE) - pnorm(0, lower.tail = TRUE), tol = 0.01)
})



#Estimate the hit or miss estimators for P(z < 2) and P(z < 2.5) and construct 95% CI
n <- 10^5
z <- rnorm(n)

g1 <- (z < 2) # return True = 1 and FALSE = 0 for all z < 2
g2 <- (z < 2.5)

f1 <- mean(g1) # Find F(x)_hat for Z < 2
f2 <- mean(g2)

#find the standard error sqrt(sum(g_i - g_bar)^2) / n
SE1 <- sqrt(sum((g1 - f1)^2)) / n
SE2 <- sqrt(sum((g2 - f2)^2)) / n

#compute Z_alpha/2
z_alpha <- qnorm(0.025, lower.tail = FALSE)

#find the confidence interval for both E(theta_hat) +- Z_alpha/2 * SE
CI_1 <- c(f1 - z_alpha * SE1, f1 + z_alpha * SE1)
CI_2 <- c(f2 - z_alpha * SE2, f2 + z_alpha * SE2)



# Quiz 3 practice problem Q2
n <- 10^4
alpha <- 2
lambda <- 3
x <- rweibull(n, shape = alpha, scale = lambda)

# compute the actual value
val <- pweibull(3, shape = alpha, scale = lambda) - pweibull(0, shape = alpha, scale = lambda)

# use hit or miss to estimate
p_hat <- mean((x < 3) & (x > 0))

val
p_hat


# unit 2 practice problems Q1 part a 
n <- 10^5
alpha <- 2
beta <- 3
x <- rgamma(n, shape = alpha, scale = beta)

p_hat <- mean(x < 2)











