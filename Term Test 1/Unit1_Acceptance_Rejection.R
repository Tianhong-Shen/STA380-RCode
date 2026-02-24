#Lec Example with density f(x)=3x^2
n<-10^4 #number of acceptance candidate
accepted<-numeric(n) #storing value for candidate density
u_accepted<-numeric(n) #storing corresponding u values for accepted candidate
#it is not in the algorithm which it's optional, it's here just for future graphing
i<-0 #counter how many acceptance sample you have so far
iteration<-0 #count total loop iteration, should be close to 1/c
while(i<n){
  y<-runif(1,0,1) 
  u<-runif(1,0,1)
  ftgt<-y^2 #f(t)/cg(t)
  
  if(u<ftgt){
    i<-i+1
    accepted[i]<-y
    u_accepted[i]<-u
  }
  iteration<-iteration+1
}

#Graphing
hist(accepted,prob=TRUE,col="blue",border="white")
curve(3*x^2, -1, 1, add = TRUE, col = "red", lwd = 2)

plot(runif(iteration, 0, 1), runif(iteration, 0, 1), pch=1, col = "maroon")
points(accepted, u_accepted, pch = 1 , col = "green")

#comparing the iteration
c=3
1/c
n/iteration



#Lec Example beta distribution
n<-10^4
c<-135/64
accepted<-numeric(n)
i<-0
iteration<-0
while(i<n){
  y<-runif(1,0,1)
  u<-runif(1,0,1)
  ftgt<-(20*y*(1-y)^3)/c
  
  if(u<ftgt){
    i<-i+1
    accepted[i]<-y
  }
  iteration<-iteration+1
}


1/c
n/iteration



#Lec Example with density 2/pi*sqrt(1-x^2)
n<-10^4
accepted<-numeric(n)
c<-4/pi
i<-0
iteration<-0
while(i<n){
  y<-runif(1,-1,1)
  u<-runif(1,0,1)
  ftgt<-sqrt(1-y^2)
  if(u<ftgt){
    i<-i+1
    accepted[i]<-y
  }
  iteration<-iteration+1
}

1/c
n/iteration





#Quiz 1 Practice problem Q3
n <- 10^4
accepted <- numeric(n)
u_accepted <- numeric(n)
c <- 2
i <- 0 
iteration <- 0 

while(i < n){
  y <- rexp(1, rate = 1/3)
  u <- runif(1, 0, 1)
  ftgt <- 1/3*y*exp(-(y/3)^2+(y/3))
  
  if(u < ftgt){
    accepted[i] <- y
    u_accepted[i] <- u
    i <- i+1
  }
  iteration <- iteration + 1
}

1/c
n/iteration




#Term Test 1 Practice A Q2
n <- 10^4
accepted <- numeric(n)
i <- 0 
c <- ((3*sqrt(3))/sqrt(2*pi))*exp(-1/2)

while(i < n){
  y <- rexp(1, rate = 3/2)
  u <- runif(1, 0, 1)
  ftgt <- (3/sqrt(pi))*y^(1/2)*exp(-y/3)*(1/c)
  
  if(u <= ftgt){
    accepted[i] <- y
    i <- i+1
  }
}
c <- ((3*sqrt(3))/sqrt(2*pi))*exp(-1/2)
u <- 0.62
y <- 0.02
ftgt <- (3/sqrt(pi))*y^(1/2)*exp(-y/3)*(1/c)

u < ftgt





#Term Test 1 Practice B Q2
n <- 10^4
accepted <- numeric(n)
i <- 0

while(i < n){
  y <- runif(1, 0, 1)
  u <- runif(1, 0, 1)
  ftgt <- y^4
  if(u < ftgt){
    accepted <- y
    i <- i + 1
  }
}

u <- 0.23
y <- 0.74
ftgt <- y^4
u < ftgt














