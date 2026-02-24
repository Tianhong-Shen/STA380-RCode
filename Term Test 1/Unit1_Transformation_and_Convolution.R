#Lec Example Beta(2,3)
n<-10^4 #create sample size
b<-numeric(n) #pre-allocate 

for(i in 1:n){
  u<-sum(rexp(2,rate=1)) #u~Exp(1) with 2 observation, remember if scale = a,
#then rate=1/a
  v<-sum(rexp(3,rate=1)) #v~Exp(1) with 3 observation
  b[i]<-u/(u+v) #store value of u/(u+v) into b
}

hist(b,prob=TRUE,col="blue")
curve(dbeta(x,shape1=2, shape2=3),add=TRUE,col="black")




#Lec Example Gamma(alpha=10,beta=1/2)
n<-10^4
b<-numeric(n)

for(i in 1:n){
  u<-sum(rexp(10,rate=2)) #remember if scale = a,
  #then rate=1/a
  b[i]<-u
}

hist(b,prob=TRUE,col="blue")
curve(dgamma(x,shape = 10,rate=2),add=TRUE,col="red")




#Term Test 1 Practice A Q3
n <- 10^4
deno <- rchisq(n, 15)/15
num <- rnorm(n, 0, 1)
b <- num/deno

hist(b,prob=TRUE,col="blue")
curve(dt(x, df = 15), add = TRUE, col = "red", lwd = 2)




#Term Test 1 Practice B Q3
n <- 10^4
v1 <- 3
v2 <- 7
w1 <- rchisq(n, 3)
w2 <- rchisq(n, 7)

f <- (w1/v1)/(w2/v2)
hist(f, prob = TRUE, col = "blue")
curve(df(x, df1 = 3, df2 = 7), add = TRUE, col = "red", lwd = 2)


















