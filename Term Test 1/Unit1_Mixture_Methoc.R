# Lec Example F(x) = 0.4F(x1)+0.6F(x2)
n <- 10^4
x1 <- rnorm(n, 0, 1) #x1~normal(0,1)
x2 <- rnorm(n, 3, 1) #x2~normal(3,1)

k <- sample(1:2, size = n, replace = TRUE, prob = c(0.4, 0.6)) #make a vector with value
#1 and 2 of size n, probablity of get a 1 is 0.4
x <- ifelse(k == 1, x1, x2) #if ki = 1 then x = x1, otherwise x = x2

s <- 0.4*x1 + 0.6*x2

par(mfrow = c(1,2))
h1 <- hist(x, prob = TRUE, col = "blue")
h2 <- hist(s, prob = TRUE, col = "blue")



# Lec Example F(x) = 0.4F(x1)+0.3F(x2) + 0.4F(x3)
n <- 10^4
u <- runif(n)
s <- numeric(n)

for(i in 1:n){
  if(u[i] <= 0.4){
    s[i] <- rnorm(1, 0, 1)
  }
  else if(u[i] <= 0.7){
    s[i] <- rnorm(1, 3, 1)
  }
  else if(u[i] <= 1){
    s[i] <- rnorm(1, 5, 1)
  }
}


par(mfrow = c(1,1))
h1 <- hist(s, prob = TRUE, col = "blue")




#QUiz 1 practice problem 5
n <- 10^4
u1 <- runif(n)
u2 <- sample(0:2, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
x <- 0.5*u1 + 0.5*u2




#Term Test 1 Practice A Q5
n <- 10^4
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 3, 1)

k <- sample(1:2, size = n, replace = TRUE, prob = c(0.3, 0.9))
x <- ifelse(k == 1, x1, x2)

s <- 0.3*x1 + 0.9*x2

par(mfrow = c(1,2))
h1 <- hist(x, prob = TRUE, col = "blue")
h2 <- hist(s, prob = TRUE, col = "blue")



