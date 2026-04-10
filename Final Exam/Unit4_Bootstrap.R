data("sleepstudy", package = "lme4") # loading data

B <- 200
n <- nrow(sleepstudy) # this n tells you the sample size you want to sampled, x1, x2 ... xn
R <- numeric(B) # this give you a vector of 0 (initial value)

for(i in 1 : B){
  b <- sample(1:n, size = n, replace = TRUE) #you randomly select n indices replacement
  R[i] <- mean(sleepstudy$Reaction[b]) #sleepstudy$Reaction[b] is the value under each indices
}


mean(R) # this is the estimated mean using bootstrap

sd(R) # since R is the mean of each bootstrap sampling, then this is the estimated standard error using bootstrap

mean(R) - mean(sleepstudy$Reaction) #this is the estimated bias 

alpha = 0.05
quantile(R, c(alpha/2, 1-alpha/2)) #this is the confidence interval quantile(R, alpha/2), quantile(R, 1-alpha/2)

deltastar <- R - mean(sleepstudy$Reaction)
alpha <- 0.05
q_star <- quantile(deltastar, c(1 -alpha/2, alpha/2))
mean(sleepstudy$Reaction) - q_star #this is the Empirical Bootstrap CI