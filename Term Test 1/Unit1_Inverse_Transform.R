#Lec Example 1 Distribution with density: fx(x)=3x^2
n<-10^4 #sample size of 10^4
u<-runif(n) #generate unif(0,1) with n samples
x<-u^(1/3) #let x be u^(1/3)

hist(x,prob=TRUE, col="blue", border="white") #drawing the histogram with random variate x
y<-seq(0,1,0.01) #create y be a sequence of number start at 0 end at 1 with gap 0.01
lines(y,3*y^2, col="red") #draw the actual line 3y^2


#Lec Example 2 exponential distribution
n<-10^4 
theta<-2 #just some random theta, for instance, 3
u<-runif(n)
x<- -theta*log(1-u)

hist(x,prob=TRUE,col="blue", border="white")
curve(dexp(x,rate=1/theta),0,10,add=T,col="red") #dexp is just the density 
#function of exponential distribution


#Lec Example 3 Weibull distribution
n<-10^4
alpha<-3
u<-runif(n)
x<-(-log(1-u))^(1/alpha)

hist(x,prob=TRUE,col="blue", border="white")
curve(dweibull(x,shape=alpha, scale=1),0,10,add=T,col="red")

mean(x)


#Lec Example 4 discrete R.V. with x={1,2,3,4}
n<-10^4
u<-runif(n)
x<-ifelse(u<=0.2,1,
          ifelse(u<=0.7,2,
                 ifelse(u<=0.9,3,4)))
table(x)/n #table function is just count how many times each distinct value of x 



#Lec Example 5 geometric distribution
n<-10^4
u<-runif(n)
p<-0.4 #p is just a arbitrary number
q<-1-p
x<-ceiling((log(1-u))/(log(q)))

#To check whether the answer is correct, we can compare the mean and variance 
mean(x)
1/p #expected value of a geometric distribution
var(x)
(1-p)/(p^2) #variance of a geometric distribution



#Lec Example 6 binomial distribution
m <- 10^4 #sample size
u <- runif(m) #generate U ~ unif(0,1) with m samples
sim_vec <- numeric(m) #create a numeric vector of length m to store result
n <- 10 # arbitrary choice
p <- 0.4 # arbitrary choice
p_start <- choose(n, 0) * p^(0) * (1-p)^(n-0) #this is p_0(where the recursion starts)
#for the algorithm, we want to return the smallest j such that F(j)>=u and F(j) = sum of p_i
for(i in 1:m){
  j <- 0 #candidate value of x
  F_val <- p_start #current cdf F(j), since F(0) = p_0
  p_j <- p_start #current pmf p_j
  while((u[i] >= F_val) && (j<n)){ #if u[i] is still bigger than F(j),
  #then move to j+1, j<n is a safety stop because the binomial should stop at n
    p_j <- (n-j)/(j+1) * (p)/(1-p) * p_j #this is just the recursive formula derived
  #in question
    F_val <- F_val + p_j #F(j+1) = F(j)+p_j+1
    j <- j + 1
  }
  sim_vec[i] <- j
}

table(sim_vec)/m
# Comparing mean and variance
mean(sim_vec)
n*p
var(sim_vec)
n*p*(1-p)



#Lec Example 7 pmf p_i = (a*theta^i)/i
n<-10^4
u<-runif(n)
sim_vec<-numeric(n)
theta<-0.4
a<-(-log(1-theta))^(-1)
p_start<-(a*theta^1)/1

for(i in 1:n){
  j<-1
  F_val<-p_start
  p_j<-p_start
  while(u[i]>=F_val){
    j<-j+1
    p_j<-(theta*(j-1)/(j))*p_j
    F_val<-F_val+p_j
  }
  sim_vec[i]<-j
}

mean(sim_vec)
(-log(1-theta))^(-1) * (theta/(1-theta))



#Quiz 1 Practice problem Q1
n <- 10^4
u <- runif(n)

x <- ifelse(u <= 1/4, 8*u, 4*sqrt(u))

#histogram of x
hist(x, prob = TRUE, col = "blue")

# Actually density function
fy <- function(y){
  ifelse(y < 0, 0, 
         ifelse(y < 2, 1/8, 
                ifelse(y < 4, y/8, 0)))
}

curve(fy, from = 0, to = 4, add = TRUE, col = "red")




#Quiz 1 Practice problem Q2
n <- 10^4
u <- runif(n)
x <- u^(1/5)

hist(x, prob = TRUE, col = "blue")

den <- function(x){
  5*x^(4)
}
curve(den, from = 0, to = 1, add = TRUE, col = "red", lwd = 2)



#Term Test 1 Practice A Q1
n <- 10^4
u <- runif(n)
x <- ifelse(u <= 1/4, 8*u, 4*sqrt(u))



#Term Test 1 Practice B Q1
n <- 10^4
u <- runif(n)
x <- ifelse(u <= 0.2, 1, 
            ifelse(u <= 0.7, 2, 
                   ifelse( u <= 0.9, 3, 4)))











