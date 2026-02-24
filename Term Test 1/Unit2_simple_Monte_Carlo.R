#Lec Example theta = integrate(exp(-x), 0, 1)
n <- 10^4
u <- runif(n, 0, 1)
theta <- 1 - exp(-1)
theta_hat <- mean(exp(-u))

test_that("estimate theta vs true theta", {
  expect_equal(theta_hat, theta, tol = 0.001)
})




#Lec Example theta = integrate(exp(-u), 2, 4)
n <- 10^4
u <- runif(n, 2, 4)
fun <- function(x){exp(-x)}
theta <- integrate(fun, 2, 4)
theta_hat <- 2 * mean(exp(-u))

test_that("estimate vs true", {
  expect_equal(theta_hat, theta$value, tol = 0.001)
})



#Quiz 2 practice problem 1
n <- 10^4
u <- runif(n, 0, 2)
v <- runif(n, 2, 4)
first_part = 2*mean(u/8)
second_part = 2*mean((v^2)/8)

Expected_value = first_part + second_part

fun1 <- function(x){x/8}
fun2 <- function(x){(x^2)/8}
true_value = integrate(fun1, 0, 2)$value + integrate(fun2, 2, 4)$value


test_that("estimate vs true", {
  expect_equal(Expected_value, true_value, tol = 0.001)
})





#Term Test 1 Practice A Q6
n <- 10^4
u <- runif(n, 2, 5)
prob <- 3 * mean((9/gamma(2))*((1/u)^3)*exp(-3/u))




















