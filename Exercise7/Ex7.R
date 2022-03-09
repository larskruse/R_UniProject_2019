library(ggplot2)
library(NonpModelCheck)

#read in the data
children_data <- read_delim("/home/lars/Dokumente/Studium/Master/6.Semester/RProject/Exercise7/childrean_data.txt", "\t")
children_data <- read_delim("childrean_data.txt", "\t")

#use matrix notation to compute regression polyomial
polyfitMat <- function(x, resp, covar, bandw, polydeg, kern, numderiv){
  
  #create the matrix X defined in the theoretical introduction
  X <- function(covar, x, polydeg){
    n <- length(covar)
    mat <- matrix(NA, nrow = n, ncol = polydeg+1 )
    for (i in 0:polydeg){
      mat[,i+1] <- t((covar-x)^i)
    }
    mat
  }
  
  #diagonal matrix defined in the theoretical introduction
  V <- function(h, kern, covar, x) {
    diag(sapply((covar-x)/h, kern))
  }
  
  Xmat <- X(covar, x, polydeg)
  Vmat <- V(bandw,kern, covar,x )
  fitmat <- solve(t(Xmat)%*%Vmat%*%Xmat)%*%t(Xmat)%*%Vmat%*%resp

}

#chosing 4 bandwidths
bandws <- c(2,10,30, 60)


x <- seq(0, 59, by = 2)
yb <- matrix(NA, nrow = length(x), ncol = 4)

#fit polynomial regression models for the four given bandwidths.
for (j in 1:length(bandws)){
  for (i in 1:length(x)){
    print(i)
    yb[i,j] <- polyfitMat(x[i], children_data$zwast, children_data$hypage, bandws[j], 1, kern = epaKern, 1)[1,1]
}
}


#plotting the function
bandwPlotData <- data.frame(x,yb)
#with given data
ggplot(data = bandwPlotData, aes(x = x))+
  geom_point(data = children_data, aes(x = hypage, y = zwast), col = "grey70")+
  geom_line(aes(y = X1), col = "blue")+
  geom_line(aes(y = X2), col = "red") + 
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4))+
  labs(title = "Local Polynomials for different Bandwidths", x = "age", y = "zwast")
#withouth the given data
ggplot(bandwPlotData, aes(x = x))+
  geom_line(aes(y = X1), col = "blue")+
  geom_line(aes(y = X2), col = "red") + 
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4))+
  labs(title = "Local Polynomials for different Bandwidths", x = "age", y = "zwast")



#chose bandwidth = 10
yk <- matrix(NA, nrow = length(x), ncol = 4)

#compute regression polynimials for different kernels
for (i in 1:length(x)){
  yk[i,1] <- polyfitMat(x[i], children_data$zwast, children_data$hypage, 10, 1, kern = unifKern, 1)[1]
}

for (i in 1:length(x)){
  yk[i,2] <- polyfitMat(x[i], children_data$zwast, children_data$hypage, 10, 1, kern = triKern, 1)[1]
}

for (i in 1:length(x)){
  yk[i,3] <- polyfitMat(x[i], children_data$zwast, children_data$hypage, 10, 1, kern = epaKern, 1)[1]
}

for (i in 1:length(x)){
  yk[i,4] <- polyfitMat(x[i], children_data$zwast, children_data$hypage, 10, 1, kern = gauKern, 1)[1]
}

#plotting the polynimias for different kernels
kernelPlotData <- data.frame(x,yk)


ggplot(kernelPlotData, aes(x = x))+
  geom_point(data = children_data, aes(x = hypage, y = zwast), col = "grey70")+
  geom_line(aes(y = X1), col = "blue")+
  geom_line(aes(y = X2), col = "red") + 
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4))+
  labs(title = "Local Polynomials for different Kernels", x = "age", y = "zwast")

ggplot(kernelPlotData, aes(x = x))+
  geom_line(aes(y = X1), col = "blue")+
  geom_line(aes(y = X2), col = "red") + 
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4))+
  labs(title = "Local Polynomials for different Kernels", x = "age", y = "zwast")




# b)

#function to optimise the gcv
gcvopti <- function( resp, covar, polydeg, kern){
  
  #as given in the theoretical introductions
  X <- function(covar, x, polydeg){
    n <- length(covar)
    mat <- matrix(NA, nrow = n, ncol = polydeg+1 )
    for (i in 0:polydeg){
      mat[,i+1] <- t((covar-x)^i)
    }
    mat
  }
  
  V <- function(h, kern, covar, x) {
    diag(sapply((covar-x)/h, kern))
  }
  
  #weights according to the theoretical introdcution
  weights <- function(h , resp, covar, polydeg, kern){
    n <- length(resp)
    val <- 0  
    for(i in 1:n){
      Xmat <- X(covar, covar[i] , polydeg)
      Vmat <- V(h, kern, covar, covar[i] )
      
      val <- val + solve(t(Xmat)%*%Vmat%*%Xmat)[1,1]*kern(0) 
    }
    (1- (val/n))^2
  }
  
  #calculation the numerator of the gcv function
  num <- function(h , resp, covar, polydeg, kern){
    n <- length(resp)
    numer <- 0
    for (i in 1:n){
      numer <- numer + (resp[i]-polyfitMat(covar[i], resp, covar, h, polydeg, kern, 1)[1,1])^2
    }
    numer
  }
  
  #
  gcv <- function(h, resp, covar, polydeg, kern){
    num(h , resp, covar, polydeg, kern) / weights(h , resp, covar, polydeg, kern)
    
  }
  
  #minimising the gcv
  optimize(gcv, interval = c(1,120), resp ,covar,polydeg, kern)$minimum
  
}



x <- seq(0, 59, by = 2)
yb <- matrix(NA, nrow = length(x)-1, ncol = 4)

#epaBw will store the optimal bandwidths
epaBw <- rep(NA, 4)
stime <- rep(NA, 4)
etime <- rep(NA, 4)
ttime <- rep(NA,4)

#calculating optimal bandwidths for different polynomial degrees
for (i in 1:4){
  stime[i] <- Sys.time()
  epaBw[i] <- gcvopti(children_data$zwast, children_data$hypage, i, kern = epaKern)
  etime[i] <- Sys.time()
  print(i)
}


#epaBw  1 4.999968
# 2 10.999998
# 3 10.999996
# 4 59.999955

epaBw <- c(4.999968, 10.999998, 10.999996, 59.999955)

#due to the complexity, the calculation needs a lot of time. 
ttime <- etime-stime
ttime
#time 1 38801.57 
#     2 34226.64 
#     3 39162.32 
#     4 41918.13


#calulating the regression functions
for (i in 1:4){
  for (j in 1:length(x)){
    yb[j,i] <- polyfitMat(x[j], children_data$zwast, children_data$hypage, epaBw[i], i, kern = epaKern, 1)[1,1]
  }
}


#plotting the data
optiBwDataFrame <- data.frame(x,yb[-31,])

ggplot(optiBwDataFrame, aes(x = x))+
  geom_point(data = children_data, aes(x = hypage, y = zwast), col = "grey70")+
  geom_line(aes(y = X1), col = "blue")+
  geom_line(aes(y = X2), col = "red") + 
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4))+
  labs(title = "Local Polynomials for optimal Bandwidths", x = "age", y = "zwast")

ggplot(optiBwDataFrame, aes(x = x))+
  geom_line(aes(y = X1), col = "blue")+
  geom_line(aes(y = X2), col = "red") + 
  geom_line(aes(y = X3), col = "green")+
  geom_line(aes(y = X4))+
  labs(title = "Local Polynomials for optimal Bandwidths", x = "age", y = "zwast")



#third part 

x <- 0:59

#calculation first degrees of regression functions
locpol1 <-localpoly.reg(children_data$hypage, children_data$zwast, points = x, bandwidth = "GCV", degree.pol = 1, kernel.type = "epanech", deriv = 1)
locpol2 <-localpoly.reg(children_data$hypage, children_data$zwast, points = x, bandwidth = "GCV", degree.pol = 2, kernel.type = "epanech", deriv = 1)
locpol3 <-localpoly.reg(children_data$hypage, children_data$zwast, points = x, bandwidth = "GCV", degree.pol = 3, kernel.type = "epanech", deriv = 1)
locpol4 <-localpoly.reg(children_data$hypage, children_data$zwast, points = x, bandwidth = "GCV", degree.pol = 4, kernel.type = "epanech", deriv = 1)


#extract the datapoints
loc1 <- locpol1$predicted
loc2 <- locpol2$predicted
loc3 <- locpol3$predicted
loc4 <- locpol4$predicted

#create data frame for plotting
locData <- data.frame(x, loc1, loc2, loc3, loc4)
colnames(locData) <- c("age" ,"poldeg1", "poldeg2", "poldeg3", "poldeg4")

#plotting the first derivatives
ggplot(data = locData, aes(x= age) ) +
  geom_line(aes(y = poldeg1), col = "blue")+
  geom_line(aes(y = poldeg2), col = "red")+
  geom_line(aes(y = poldeg3), col = "green" )+
  geom_line(aes(y = poldeg4))+
  labs(x = "Age", y = "Zwast", title = "First derivative")

  
  

#Kernels
#Nadaraya-Watson
nadaKern <- function(x){
  if(abs(x) <=1)
    1
  else
    0
}

#uniform kernel
unifKern <- function(x){
  if (abs(x)<=1)
    0.5
  else
    0
}

#triangular kernel
triKern <- function(x){
  if (abs(x)<=1){
    1-abs(x)
  }
  else
    0
}

#epanechikov kernel
epaKern <- function(x){
  if (abs(x)<=1){
    0.75*(1-x^2)
  }
  else
    0
}

#gaussian kernal
gauKern <- function(x){
  (2*pi)^(-0.5)*exp(-x^2/2)
}