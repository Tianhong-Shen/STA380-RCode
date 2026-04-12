# Lec Example generate n = 30 from exp(theta=1/3) and gamma(alpha = 2, beta = 1/5)
#then perform the permutation test
n <- 30
x <- rexp(n, rate = 3)
y <- rgamma(n, shape = 2, rate = 5)

z <- c(x, y) # define the pooled sample first
Fn <- ecdf(x)
Gn <- ecdf(y)

D0 <- max(abs(Fn(z) - Gn(z))) # compute D0

R <- 999
D <- numeric(n)
for(i in 1:R){
  k <- sample(1:(2*n), size = n, replace = FALSE) # random sample indices
  xi <- z[k] #choose x be random sample n from z
  yi <- z[-k]# choose y be the left n samples
  
  Fni <- ecdf(xi)
  Gni <- ecdf(yi)
  D[i] <- max(abs(Fni(z) - Gni(z)))
}

p <- mean(c(D0, D) >= D0)
p

ks.test(x, y) # there is a bulid in function can directly do the permutation test
