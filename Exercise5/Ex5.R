library(survival)
library(tidyverse)

#read in the data and remove not needed columns
throracic <- read.table("Thoracic.txt")
throracic <- throracic[, c(14, 16, 17)]
summary(throracic)

#set the variables names 
names(throracic) <- c("PRE30", "AGE", "Risk1Y")

#create a Surv object
survob <- Surv( throracic$AGE+1, throracic$Risk1Y)

#fit a Kaplan-Meier and Fleming-Harrington model
fitKM <- survfit(survob ~ 1, data=throracic,type="kaplan-meier", conf.type="plain")
fitFH <- survfit(survob ~ 1, data=throracic,type="fleming-harrington",conf.type="plain")

plot(fitKM, xlab  = "Age",  main = "Kaplan-Meier")
plot(fitFH, xlab = "Age", main = "Fleming-Harrington")

##plots 
plot(fitKM, col = "red", xlab = "Age", main = "Kaplan-Meier and Fleming-Harrington")
lines(fitFH, col = "blue")

#fit an exponential model and extract the parameter 
expo <- survreg(survob ~ 1, throracic, dist = "exponential")
ex_coef <- exp(-expo$coefficients )
ex_coef

#fit a weibull model and extract the parameters
weib <- survreg(survob ~ 1, throracic, dist = "weibull")
weib_scale <- exp(-weib$coefficients)
weib_shape <- 1/weib$scale

x <- 0:87
#plot KM and parametric estimators in one plot
plot(fitKM, xlab = "Age", main ="Exponential and Weibull Models")
lines(x, exp(-ex_coef*x), col = "red")
lines(x, weibullval(x, weib_scale, weib_shape), col = "blue")

#weibull function
weibullval<-function(x,scale,shape){ 
  erg=exp(-(scale*x)^shape)
  return(erg)
}

#determine if weibull fits the data
plot(log(fitKM$time), log(-log(fitKM$surv)), xlim = c(3,5), ylim = c(-5, 0), xlab = "", ylab ="", main ="Weibull Test")
abline(a = log(weib_scale)*weib_shape, b = weib_shape , col = "blue")


#part two 
#seperate the smokers and nonsmokers
smoker <- subset(throracic, PRE30 == TRUE)
nonSmoker <- subset(throracic, PRE30 == FALSE) 

dim(smoker)[1]/dim(throracic)[1]*100 #percentage of smokers in the data set 


#create Surv objects for both classes
survSmoker <- Surv(smoker$AGE+1, smoker$Risk1Y)
survNonSmoker <- Surv(nonSmoker$AGE+1, nonSmoker$Risk1Y)

#and fit the models
fitKMSmoker <- survfit(survSmoker ~ 1, data=smoker,type="kaplan-meier", conf.type="plain")
fitKMNonSmoker <- survfit(survNonSmoker ~ 1, data=nonSmoker,type="kaplan-meier", conf.type="plain")


plot(fitKMSmoker, xlab = "Age", main = "Smoker vs. Non-Smoker")
lines(fitKMNonSmoker, col = "blue")

#test if different distributions for smokers and non smokers
survdiff(survob ~ PRE30 , throracic, rho = 0) # p value: cannot be rejected with 0.05 confidence -> collect more data on non smokers 

#create weibul estimator and extract parameters for both groups
weibSmoker <- survreg(survSmoker ~ 1, smoker, dist = "weibull")
weibSmoker_scale <- exp(-weibSmoker$coefficients)
weibSmoker_shape <- 1/weibSmoker$scale


weibNonSmoker <- survreg(survNonSmoker ~ 1, nonSmoker, dist = "weibull")
weibNonSmoker_scale <- exp(-weibNonSmoker$coefficients)
weibNonSmoker_shape <- 1/weibNonSmoker$scale

#plot KM and weibull estimators
plot(fitKMSmoker, xlab = "Age", main = "Weibull for Smokers")
lines(x, weibullval(x, weibSmoker_scale, weibSmoker_shape), col = "blue")

plot(fitKMNonSmoker, xlab = "Age", main = "Weibull for Non-Smokers")
lines(x, weibullval(x, weibNonSmoker_scale, weibNonSmoker_shape), col = "blue")


#determine if weibull fits the data
plot(log(fitKMNonSmoker$time), log(-log(fitKMNonSmoker$surv)), xlim = c(3,5), ylim = c(-5,0), xlab ="", ylab = "", main = "Weibull Test Non-Smokers")
abline(a = log(weibNonSmoker_scale)*weibNonSmoker_shape, b = weibNonSmoker_shape , col ="blue")


plot(log(fitKMSmoker$time),log(-log(fitKMSmoker$surv)), xlim = c(3,6), ylim = c(-5, 0), xlab ="", ylab = "", main = "Weibull Test Smokers")
abline(a = log(weibSmoker_scale)*weibSmoker_shape, b = weibSmoker_shape , col = "blue")
