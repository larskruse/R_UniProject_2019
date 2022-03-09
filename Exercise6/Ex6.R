library(ggplot2)

stud_performance_raw <- read.csv("StudentsPerformance.csv")

used_columns <- c("test.preparation.course" , "math.score", "reading.score", "writing.score")
stud_performance <- stud_performance_raw[used_columns]

#none = 2, completed = 1
stud_performance$test.preparation.course <- as.integer(stud_performance$test.preparation.course)


#kernal estimator
kernelEstim <- function (x, data, bandw, kernfunc){
  n <- length(data)
  
  estim <- rep(NA, length(x))
  for (i in 1:length(x)){
    elem <- (x[i]-data)/bandw
    estim[i] <- 1/(n*bandw)*sum(sapply(elem, kernfunc))
  }
  
  return(estim)
}

#Kernels
#uniform kernel
unifKern <- function(x){
  if (abs(x)<=1)
    0.5
  else
    0
}
plot(seq(-3,3,by=0.01), sapply(seq(-3,3,by=0.01), unifKern), type = "l")

#triangular kernel
triKern <- function(x){
  if (abs(x)<=1){
    1-abs(x)
  }
  else
    0
}
plot(seq(-3,3,by=0.1), sapply(seq(-3,3,by=0.1), triKern), type = "l")


#epanechikov kernel
epaKern <- function(x){
  if (abs(x)<=1){
    0.75*(1-x^2)
  }
  else
    0
}
plot(seq(-3,3,by=0.1), sapply(seq(-3,3,by=0.1), epaKern), type = "l")


#gaussian kernal
gauKern <- function(x){
  (2*pi)^(-0.5)*exp(-x^2/2)
}

plot(seq(-3,3,by=0.1), sapply(seq(-3,3,by=0.1), gauKern), type = "l")

stud_range <- 0:120
epa1 <- kernelEstim(stud_range, stud_performance$math.score, 1, epaKern)
epa5 <- kernelEstim(stud_range,stud_performance$math.score, 5, epaKern)
epa15 <- kernelEstim(stud_range,stud_performance$math.score, 15, epaKern)
epa40 <- kernelEstim(stud_range,stud_performance$math.score, 40, epaKern)
epa_data <- data.frame(stud_range, epa1, epa5, epa15, epa40)

ggplot(data = epa_data, aes(stud_range)) +
  geom_line(aes(y=epa1, colour = "1")) +
  geom_line(aes(y=epa5, colour = "5")) +
  geom_line(aes(y=epa15, colour = "15")) +
  geom_line(aes(y=epa40, colour = "40")) +
  labs(x = "Grades", y = "Density", title = "Epanechikov Kernel Estimators", colour = "Bandwidth")


epa <- epa15
unif <- kernelEstim(stud_range,stud_performance$math.score, 15, unifKern)
gaus <- kernelEstim(stud_range,stud_performance$math.score, 15, gauKern)
tri <- kernelEstim(stud_range,stud_performance$math.score, 15, triKern)
kern_data <- data.frame(stud_range, unif, tri, epa, gaus)

ggplot(data = kern_data, aes(stud_range)) +
  geom_line(aes(y=unif, colour = "Uniform")) +
  geom_line(aes(y=tri, colour = "Tridiagonal")) +
  geom_line(aes(y=gaus, colour = "Gauss")) +
  geom_line(aes(y=epa, colour = "Epanechnikov")) +
  labs(x = "Grades", y = "Density", title = "Kernel Estimators", colour = "Kernel")

#b)

cvopti <- function(data, kernel){
  
  cv <- function(h, data, kernel) {
    n <- length(data)
    
    den <- integrate(kernelEstim2, lower = 0, upper = 100, data = data, bandw = h, kernfunc = kernel)$value
    den-2*1/(n*(n-1)*h)*K(h,data,kernel)
  }
  
  kernelEstim2 <- function (x, data, bandw, kernfunc){
    n <- length(data)
    
    estim <- rep(NA, length(x))
    for (i in 1:length(x)){
      elem <- (x[i]-data)/bandw
      estim[i] <- 1/(n*bandw)*sum(sapply(elem, kernfunc))
    }
    
    return(estim^2)
  }
  
  K <- function(h, data, kernel){
    kval <- 0
    for (i in 1:length(data)){
      for (j in (1:length(data))[-i])
        kval <- kval + kernel((data[i]-data[j])/h)
    }
    return(kval)
  }
  
  optimize(cv, interval = c(1,20), data = data, kernel =  kernel)$minimum
}

#calculating optimal bandwidth for gauss kernel 
mathBw <- cvopti(stud_performance$math.score, gauKern)
readBw <- cvopti(stud_performance$reading.score, gauKern)
writeBw <- cvopti(stud_performance$writing.score, gauKern)

mathUcv <- bw.ucv(stud_performance$math.score, lower = 1, upper = 50)
mathBcv <- bw.bcv(stud_performance$math.score)

readUcv <- bw.ucv(stud_performance$reading.score)
readBcv <- bw.bcv(stud_performance$reading.score, lower = 1, upper = 50)

writeUcv <- bw.ucv(stud_performance$writing.score)
writeBcv <- bw.bcv(stud_performance$writing.score)

mathBwSet <- c(mathBw, mathUcv, mathBcv)
readingBwSet <- c(readBw, readUcv, readBcv) 
writingBwSet <- c(writeBw, writeUcv, writeBcv)

mathBwSet
readingBwSet
writingBwSet


#part c ) 

prepcourse <- subset(stud_performance, test.preparation.course == 1 )
no_prepcourse <- subset(stud_performance, test.preparation.course == 2)

#math density 
prepMathBw <- cvopti(prepcourse$math.score, gauKern)
no_prepMathBw <- cvopti(no_prepcourse$math.score, gauKern)

prepMath <- kernalEstim(stud_range, prepcourse$math.score, prepMathBw, gauKern)
no_prepMath <- kernalEstim(stud_range, no_prepcourse$math.score, no_prepMathBw, gauKern)

MathData <- data.frame(stud_range, prepMath, no_prepMath)

ggplot(data = MathData, aes(stud_range)) +
  geom_line(aes(y=prepMath, colour = "Course taken")) +
  geom_line(aes(y=no_prepMath, colour = "Course not taken")) +
  
  labs(x = "Grades", y = "Density", title = "Mathe Score Kernel Estimators", colour = "Preparation Course")



prepReadBw <- cvopti(prepcourse$reading.score, gauKern)
no_prepReadBw <- cvopti(no_prepcourse$reading.score, gauKern)

prepRead <- kernalEstim(stud_range, prepcourse$reading.score, prepReadBw, gauKern)
no_prepRead <- kernalEstim(stud_range, no_prepcourse$reading.score, no_prepReadBw, gauKern)


ReadData <- data.frame(stud_range, prepRead, no_prepRead)

ggplot(data = ReadData, aes(stud_range)) +
  geom_line(aes(y=prepRead, colour = "Course taken")) +
  geom_line(aes(y=no_prepRead, colour = "Course not taken")) +
  
  labs(x = "Grades", y = "Density", title = "Reading Score Kernel Estimators", colour = "Preparation Course")



prepWriteBw <- cvopti(prepcourse$writing.score, gauKern)
no_prepWriteBw <- cvopti(no_prepcourse$writing.score , gauKern)

prepWrite <- kernalEstim(stud_range, prepcourse$writing.score, prepWriteBw, gauKern)
no_prepWrite <- kernalEstim(stud_range, no_prepcourse$writing.score, no_prepWriteBw, gauKern)


WriteData <- data.frame(stud_range, prepWrite, no_prepWrite)

ggplot(data = WriteData, aes(stud_range)) +
  geom_line(aes(y=prepWrite, colour = "Course taken")) +
  geom_line(aes(y=no_prepWrite, colour = "Course not taken")) +
  
  labs(x = "Grades", y = "Density", title = "Writing Score Kernel Estimators", colour = "Preparation Course")
