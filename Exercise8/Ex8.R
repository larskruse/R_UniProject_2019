library(ggplot2)
library(splines)

#read in the data
stemcells <- read.delim("stemcell.txt")
#stemcells$id <- rownames(stemcells)
stemcells
#summary(stemcells)

#set value for autoregressive process of the errors
alphaVal <- 0.55


#computing splines
splineReg <- function(resp, covar, knots, degree){
  
  #set the knots to equidistant knots starting with 0 and ending with 144
  n <-length(covar)
  knotdist <- n/knots
  knots <- seq(1, n, by = knotdist)
  
  #setting X1 the b-spline basis matrix
  X1 <- NA
  X1 <- bs(covar, knots = knots, degree = degree )
  #setting Y the response vector
  Y <- resp
  
  #calculating the fixex part of the spline function
  sp <- solve(t(X1)%*%X1)%*%t(X1)%*%Y
  
  #calcualte values of the splines for all x from 1 to 144
  int <- rep(NA, n)
  x <- 1:n
  int <- bs(x, knots = knots, degree = degree)%*%sp

}

#setting knots
knots <- c(2,5,20,50)

#spKnots will store the function values of the splines for all four numbers of knots
spKnots <- matrix(NA, ncol = 4, nrow = length(stemcells$X0.3986215))
x <- seq(1:143)

#calculate splines for all four numbers of knots
for (i in 1:4){
  spKnots[,i] <- splineReg(stemcells$X0.3986215, x, knots[i], 2)
}

#create data frame for plotting
spKnotsData <- data.frame(x, spKnots, stemcells$X0.3986215)

#plotting the splines on the data
ggplot(spKnotsData, aes(x = x)) +
  geom_line(aes(y = X1))+
  geom_line(aes(y = X2), col = "red")+
  geom_line(aes(y = X3), col = "blue")+
  geom_line(aes(y = X4), col = "deeppink")+
  geom_point(aes(y = stemcells.X0.3986215), col = "grey60")+
  ylim(0.3,0.6)+
  labs(title = "Splines for different numbers of knots", y = "y")

#fixing number of knots to 10
knots <- 20

degree <- seq(1:4)
spDegree <- matrix(NA, ncol = 4, nrow = length(stemcells$X0.3986215))

#calculate splines for spline degrees from 1 to 4
for (i in 1:4){
  spDegree[,i] <- splineReg(stemcells$X0.3986215, x, knots, degree[i])
  
}

#creating data frame for plotting
spDegreeData <- data.frame(x, spDegree, stemcells$X0.3986215)

ggplot(spDegreeData, aes(x = x)) +
  geom_line(aes(y = X1))+
  geom_line(aes(y = X2), col = "red")+
  geom_line(aes(y = X3), col = "blue")+
  geom_line(aes(y = X4), col = "deeppink")+
  geom_point(aes(y = stemcells.X0.3986215), col = "grey60")+
  ylim(0.3,0.6)+
  labs(title = "Splines for different numbers of spline degrees", y = "y")


#b)
#function to optimize gcv
gcvopti <- function(resp, covar, degree){
  
  #gcv function that calcualtes GCV(h)
  gcv <- function(knots, resp, covar, degree) {
    sum(norm((splineReg(resp, covar, knots, degree) - resp),type ="2")/(1-knots/length(covar))^2)
  }

  #optim minimizes gcv to give the best value for the number of knots
  optim(5, gcv, resp = resp, covar = covar, degree = degree, method = "Brent", lower = 0.001, upper = 140)
}

degree <- seq(1:4)
#calculate optimal number of knots for spline degrees from 1 to 4
optiknots <- rep(NA, 4)
for (i in degree){
  optiknots[i] <- gcvopti(stemcells$X0.3986215, x, i)$par
}
optiknots

#compute the splines with opitmal number of knots for degree 1 to 4
spOptiKnots <- matrix(NA, ncol = 4, nrow = length(stemcells$X0.3986215))
x <- seq(1:143)
for (i in 1:4){
  spOptiKnots[,i] <- splineReg(stemcells$X0.3986215, x, optiknots[i], i)
  
}
#create data frame for plotting
spOptiKnotsData <- data.frame(x, spOptiKnots, stemcells$X0.3986215)

#plotting the data
ggplot(spOptiKnotsData, aes(x = x)) +
  geom_line(aes(y = X1))+
  geom_line(aes(y = X2), col = "red")+
  geom_line(aes(y = X3), col = "blue")+
  geom_line(aes(y = X4), col = "deeppink")+
  geom_point(aes(y = stemcells.X0.3986215), col = "grey60")+
  ylim(0.3,0.6)+
  labs(title = "Splines for different numbers of spline degrees using optimal number of knots", y = "y")



#c)

#error corrected spline function
splineRegErr <- function(resp, covar, knots, degree){
  #compute equidistant knots
  n <-length(covar)
  knotdist <- n/knots
  knots <- seq(1, 143, by = knotdist)

  #set error vector
  err <- rep(NA, n)
  #set Y 
  Y <- resp
  
  #error is corrected by subtracting the error of all previous points from the ith point
  #the error of the ith element is than normal distributed
  for (i in 1:n){
    #compute splines as before
    X1 <- NA
    X1 <- bs(covar, knots = knots, degree = degree )
    sp <- solve(t(X1)%*%X1)%*%t(X1)%*%Y
    int <- rep(NA, n)
    x <- 1:n
    int <- bs(x, knots = knots, degree = degree)%*%sp
    
    #calculate error term
    err[i] <-  (Y[i]-int[i])
    for(j in (i+1):143){
      #subtracting the error from all subsequent errors with weight alpha
      Y[j] <- Y[j]- alphaVal^(j-i) * err[i]
    }
  }
  
  return(int)
}

#set degrees
degree <- seq(1:4)

#calculate opitmal number of knots
optiknots <- rep(NA, 4)
for (i in degree){
  optiknots[i] <- gcvopti(stemcells$X0.3986215, x, i)$par
}
optiknots

#calculate splines for degrees 1 to 4 with opitmal number of knots and error corrected algo
spOptiKnots <- matrix(NA, ncol = 4, nrow = length(stemcells$X0.3986215))
x <- seq(1:143)
for (i in 1:4){
  spOptiKnots[,i] <- splineRegErr(stemcells$X0.3986215, x, optiknots[i], i)
  
}


#the the interpolation function for a polynomial of degree 4
interfunc <- function(x, b) b[1]+x*b[2]+x^2*b[3]+x^3*b[4]+x^4*b[5]

#interpolate a polynimial of degree 4 to the data
polyInter <- lm(X0.3986215 ~ x+I(x^2)+I(x^3)+I(x^4), data = stemcells)
polyInterVals <- interfunc(x, polyInter$coefficients)


#binging all the data togehter
spOptiKnotsData <- data.frame(x, spOptiKnots, stemcells$X0.3986215, polyInterVals)
spOptiKnotsData

#plotting the error corrected splines 
ggplot(spOptiKnotsData, aes(x = x)) +
  geom_line(aes(y = X1))+
  geom_line(aes(y = X2), col = "red")+
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4), col = "deeppink")+
  geom_line(aes(y=polyInterVals), col = "navy")+
  geom_point(aes(y = stemcells.X0.3986215), col = "grey60")+
  ylim(0.3,0.6)+
  labs(title = "Splines for corrected data", y = "y")

