library(ggplot2)
library(bootstrap)

#read in the data
sleep_heart_data <- read.delim("shhs1.txt")
summary(sleep_heart_data)
#store the needed variable
rdi_data <- data.frame(sleep_heart_data$rdi4p)
rm(sleep_heart_data)

#a)
#different values used
n <- 100 #100   #1000  #100
R <- 5000 #1000  #1000  #5000
M <- 1000

#create n weibul distributed variables
weibull_vars <- rweibull(n, 1, scale = 13)
weibull_vars

#sample from the weibull variables
samples <- matrix(sample(weibull_vars, size = n*R, replace = TRUE), R, n)

#compute the median for all rows of the matrix
samples_median <- apply(samples, 1, median)

#plot histogram and denstiy of the medians
ggplot(data.frame(meanTime = samples_median),aes(x=meanTime)) +
  geom_histogram(binwidth = 1, aes(y=..density..)) +
  geom_density(color="red")+
  labs(title = "Histogram and Densitiy of Weibull Medians", x = "median", y = "density")

#calculate confidece intervals
mean_lower <- sort(samples_median)[floor(R*0.05/2)]
mean_upper <- sort(samples_median)[floor(R*(1-0.05/2))]
#output intervals
mean_lower
mean_upper


  #calculate probability of Monte Carlo varialbes lieing inside the interval
#genereate Weibull distributed samples
monteCarlo_matrix <- matrix(rweibull(M*n, 1, scale = 13),M,n)
#compute median
monteCarlo_median <- apply(monteCarlo_matrix, 1, median)
#check number of variables inside the interval / number of samples
length(monteCarlo_median[monteCarlo_median <= mean_upper & monteCarlo_median >= mean_lower])/M

#compute variance estimator using bootstrap samples
samples_var <- apply(samples, 1, var)
#computing standard deviation from variance
samples_sd <- sqrt(samples_var)

#plotting histogram and denstiy of standard deviation
ggplot(data.frame(sd = samples_sd),aes(x=sd)) +
  geom_histogram(binwidth = 0.5, aes(y=..density..)) +
  geom_density(color="red")+
  labs(title = "Histogram and Densitiy of Weibull Standard Deviations", x = "Standard Deviation", y = "density")

#computing confidence intervals for variance
var_lower <- sort(samples_var)[floor(R*0.05/2)]
var_upper <- sort(samples_var)[floor(R*(1-0.05/2))]
#and using them to compute the confidence intervals for standad deviation
sd_lower <- sqrt(var_lower)
sd_upper <- sqrt(var_upper)
sd_lower
sd_upper


#Computing propability of Monte Carlo sample sd to lie in that interval
monteCarlo_matrix <- matrix(rweibull(M*n, 1, scale = 13),M,n)
monteCarlo_var <- apply(monteCarlo_matrix, 1, var)
monteCarlo_sd <- sqrt(monteCarlo_var)
length(monteCarlo_sd[monteCarlo_sd <= sd_upper & monteCarlo_sd >= sd_lower])/M



#set matrix for the interval lengths
intlen <- matrix(NA, nrow = 100, ncol = 2)

#repeat computation of confidence intervals to estimate average interval length
for (i in 1:100){
  #create weibull varialbes
  weibull_vars <- rweibull(n, 1, scale = 13)
  
  #create bootstrap samples
  samples <- matrix(sample(weibull_vars, size = n*R, replace = TRUE), R, n)
  
  #compute median
  samples_median <- apply(samples, 1, median)
  #and confidence intervals
  mean_lower <- sort(samples_median)[floor(R*0.05/2)]
  mean_upper <- sort(samples_median)[floor(R*(1-0.05/2))]
  #store the length of the interval
  intlen[i,1] <- mean_upper-mean_lower
  
  #compute sd
  samples_var <- apply(samples, 1, var)
  samples_sd <- sqrt(samples_var)
 
  #compute confidence intervals
  var_lower <- sort(samples_var)[floor(R*0.05/2)]
  var_upper <- sort(samples_var)[floor(R*(1-0.05/2))]
  sd_lower <- sqrt(var_lower)
  sd_upper <- sqrt(var_upper)
  
  #store the interval length
  intlen[i,2] <- sd_upper-sd_lower
  
}

#average interval length
#median
sum(intlen[,1])/100
#sd
sum(intlen[,2])/100



#second part:

#create weibull varialbes
weibull_vars <- rweibull(n, 1, scale = 13)

#use build in function to compute bootstrap accelerated
#bias correctec confidence intervals for the median
boot_acc_median <- bcanon(weibull_vars, R, median, alpha = c(0.025,0.975))
#print the resulting parameters
boot_acc_median$z0
boot_acc_median$acc
#extract confidence intervals
confpoints_median <- boot_acc_median$confpoints
boot_acc_med_lower <- confpoints_median[1,2]
boot_acc_med_upper <- confpoints_median[2,2]
boot_acc_med_lower
boot_acc_med_upper
#compute coverage probability
length(monteCarlo_median[monteCarlo_median <= boot_acc_med_upper & monteCarlo_median >= boot_acc_med_lower])/M

#reapeat for sd
##use build in function to compute bootstrap accelerated
#bias correctec confidence intervals for the standard deviation
boot_acc_sd <- bcanon(weibull_vars, R, sd, alpha = c(0.025, 0.975))
#print the parameters
boot_acc_sd$z0
boot_acc_sd$acc
#extract the confidence interval
confpoints_sd <-  boot_acc_sd$confpoints
boot_acc_sd_lower <- confpoints_sd[1,2]
boot_acc_sd_upper <- confpoints_sd[2,2]
boot_acc_sd_lower
boot_acc_sd_upper
#compute coverage probability for sd
length(monteCarlo_sd[monteCarlo_sd <= boot_acc_sd_upper & monteCarlo_sd >= boot_acc_sd_lower])/M


#compute average interval length by running the analysis 100 times
intlenacc <- matrix(NA, 100, 2)

for (i in 1:100){
  #creating weibull vars
  weibull_vars <- rweibull(n, 1, scale = 13)
  
  #computing median confidence interval
  boot_acc_median <- bcanon(weibull_vars, R, median, alpha = c(0.025,0.975))
  confpoints_median <- boot_acc_median$confpoints
  boot_acc_med_lower <- confpoints_median[1,2]
  boot_acc_med_upper <- confpoints_median[2,2]
  #store the length in the matrix
  intlenacc[i,1] <- boot_acc_med_upper - boot_acc_med_lower
  
  #compute sd confidence interval
  boot_acc_sd <- bcanon(weibull_vars, R, sd, alpha = c(0.025, 0.975))
  confpoints_sd <-  boot_acc_sd$confpoints
  boot_acc_sd_lower <- confpoints_sd[1,2]
  boot_acc_sd_upper <- confpoints_sd[2,2]
  #store the length in the matrix
  intlenacc[i,2] <- boot_acc_sd_upper - boot_acc_sd_lower
}

#average interval length
#median
sum(intlenacc[,1])/100
#sd
sum(intlenacc[,2])/100


#part b)
#store the needed data in a vector
rdi <- rdi_data$sleep_heart_data.rdi4p
rdi

#histogram of the data
ggplot(data = rdi_data, aes(x =sleep_heart_data.rdi4p)) +
  geom_histogram(binwidth = 4, aes(y=..density..)) +
  labs(title = "Histogram of Respiratory Disturbance Index", x = "RD Index", y = "density")

#ECDF of the data
ggplot(rdi_data, aes(x =sleep_heart_data.rdi4p)) +
  stat_ecdf(geom = "step")+
  labs(title = "ECDF of Respiratory Disturbance Index", x = "RD Index", y = "y")

#create matrices to store median and variance of bootstrap samples  
rdi_samples_median <- rep(NA, 1000)
rdi_samples_var <- rep(NA,1000)

#sampling 1000 times from the data and computing median and variance
for (i in 1:1500){
  rdi_sample <- sample(rdi, 1500, replace = TRUE)
  rdi_samples_median[i] <- median(rdi_sample)
  rdi_samples_var[i] <- var(rdi_sample)
}

#compute bootstrap percentile intervals for the median
rdi_med_lower <- sort(rdi_samples_median)[floor(1000*0.05/2)]
rdi_med_upper <- sort(rdi_samples_median)[floor(1000*(1-0.05/2))]
rdi_med_lower
rdi_med_upper


#compute bootstrap percentile intervals for the sd
rdi_var_lower <- sort(rdi_samples_var)[floor(1000*0.05/2)]
rdi_var_upper <- sort(rdi_samples_var)[floor(1000*1-0.05/2)]
rdi_sd_lower <- sqrt(rdi_var_lower)
rdi_sd_upper <- sqrt(rdi_var_upper)
rdi_sd_lower
rdi_sd_upper


#compute bootstrap accelerated bias-corrected confidence intervals for median
boot_acc_rdi_median <- bcanon(rdi, 1500, median, alpha= c(0.025,0.975 ))
boot_acc_rdi_median
confpoints_rdi_median <- boot_acc_rdi_median$confpoints
boot_acc_rdi_med_lower <- confpoints_rdi_median[1,2]
boot_acc_rdi_med_upper <- confpoints_rdi_median[2,2]
boot_acc_rdi_med_upper
boot_acc_rdi_med_lower

#compute bootstrap accelerated bias-corrected confidence intervals for sd
boot_acc_var <- bcanon(rdi, 15000, var, alpha = c(0.25, 0.975)) 
confpoints_var <-  boot_acc_var$confpoints
boot_acc_var_lower <- confpoints_var[1,2]
boot_acc_var_upper <- confpoints_var[2,2]
boot_acc_sd_lower <- sqrt(boot_acc_var_lower)
boot_acc_sd_upper <- sqrt(boot_acc_var_upper)
boot_acc_sd_lower
boot_acc_sd_upper
