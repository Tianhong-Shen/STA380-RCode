# Lec Example X ~ N(0, 1) estimate P(x > 2.5) using importance sampling with 
# importance function exp(1)
n <- 10^4
x <- rexp(n, rate = 1)

theta_hat <- mean(1/sqrt(2*pi) * exp(x - x^2/2) * (x > 2.5)) # don't forget (x > 2.5)
true_prob <- pnorm(2.5, lower.tail = FALSE)

library(testthat)
test_that("Checking importance sampler", {
  expect_equal(theta_hat, true_prob, tol = 0.0001)
})



# Lec exercise theta = integrate(tan(x)exp(-x^2/2), lower = 0, upper = 1) with
# importance function N(0, 1)
n <- 10^5
x <- rnorm(n)

theta_hat <- mean(sqrt(2*pi) * tan(x) * (x < 1 & x > 0))
fun <- function(x){
  tan(x) * exp(-x^2/2)
}

true_val <- integrate(fun, lower = 0, upper = 1)

test_that("checking importance sampling", {
  expect_equal(theta_hat, true_val$value, tol = 0.0001) 
})



# unit 2 practice problems Q4

# compute exact value
fun <- function(x){
  exp(-((log(x))^2) / 2)
}

val <- integrate(fun, lower = 1, upper = 4)$value

# use simple monte carlo estimation
n <- 10^5
u <- runif(n, 1, 4)
theta_hat <- mean(3 * exp(-log(u)^2 / 2))

val ; theta_hat
  
#estimate theta using importance function log-normal
x <- rlnorm(n, meanlog = 0, sdlog = 1)
new_est <- mean(sqrt(2 * pi) * x * ((x < 4) & (x > 1)))

val ; new_est ; theta_hat





  
