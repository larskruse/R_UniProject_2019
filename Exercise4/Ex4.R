library(tidyverse)

#reading the data
grades_data <- read.csv("student-mat.csv")
summary(grades_data)

#calculating means and sd for the normal densities in the histograms
mean_g1 <- mean(grades_data$G1)
var_g1 <- sqrt(var(grades_data$G1))

mean_g2 <- mean(grades_data$G2)
var_g2 <- sqrt(var(grades_data$G2))

mean_g3 <- mean(grades_data$G3)
var_g3 <- sqrt(var(grades_data$G3))


#Are G1, G2, G3 normal distributed?
#histogram for G1
hist(grades_data$G1, freq = FALSE, ylim = c(0,0.13), xlab = "First period grade", main = "Histogram First Periode Grades with Normal Density")
xfit_g1 <- seq(0, 20, length = 100) 
yfit_g1 <- dnorm(xfit_g1, mean = mean_g1, sd = var_g1) 
lines(xfit_g1, yfit_g1, col = "blue", lwd = 2)

#histogram for G2
hist(grades_data$G2, freq = FALSE, ylim = c(0,0.13),  xlab = "Second period grade", main = "Histogram Second Periode Grades with Normal Density")
xfit_g2 <- seq(0, 20, length = 100) 
yfit_g2 <- dnorm(xfit_g2, mean = mean_g2, sd = var_g2) 
lines(xfit_g2, yfit_g2, col = "blue", lwd = 2)

#histogram for G3
hist(grades_data$G3, freq = FALSE, ylim = c(0,0.13), xlab = "Third period grade", main = "Histogram Third Periode Grades with Normal Density")
xfit_g3 <- seq(0, 20, length = 100) 
yfit_g3 <- dnorm(xfit_g3, mean = mean_g3, sd = var_g3) 
lines(xfit_g3, yfit_g3, col = "blue", lwd = 2)

#QQplots 
#G1
qq_g1_norm <- ggplot(grades_data, aes(sample = G1))
qq_g1_norm +
  geom_qq(distribution = stats::qnorm, dparams = list(mean = mean_g1, sd = var_g1)) +
  geom_qq_line(distribution = stats::qnorm, dparams = list(mean = mean_g1, sd = var_g1))

#G2
qq_g2_norm <- ggplot(grades_data, aes(sample = G2))
qq_g2_norm +
  geom_qq(distribution = stats::qnorm, dparams = list(mean = mean_g2, sd = var_g2)) +
  geom_qq_line(distribution = stats::qnorm, dparams = list(mean = mean_g2, sd = var_g2))

#G3
qq_g3_norm <- ggplot(grades_data, aes(sample = G3))
qq_g3_norm +
  geom_qq(distribution = stats::qnorm, dparams = list(mean = mean_g3, sd = var_g3)) +
  geom_qq_line(distribution = stats::qnorm, dparams = list(mean = mean_g3, sd = var_g3))


#Are G1, G2, G3 Poisson distributed?
#Histograms
hist(grades_data$G1, freq = FALSE, ylim = c(0,0.13), xlab = "First period grade", main = "Histogram First Periode Grades with Poisson Density")
xfit_g1 <- seq(0, 20) 
yfit_g1 <- dpois(xfit_g1, lambda = mean_g1) 
lines(xfit_g1, yfit_g1, col = "blue", lwd = 2)

hist(grades_data$G2, freq = FALSE, ylim = c(0,0.13), xlab = "Second period grade", main = "Histogram Second Periode Grades with Poisson Density")
xfit_g2 <- seq(0, 20) 
yfit_g2 <- dpois(xfit_g2, lambda = mean_g2) 
lines(xfit_g2, yfit_g2, col = "blue", lwd = 2)

hist(grades_data$G3, freq = FALSE, ylim = c(0,0.13), xlab = "Third period grade", main = "Histogram Third Periode Grades with Poisson Density")
xfit_g3 <- seq(0, 20) 
yfit_g3 <- dpois(xfit_g3, lambda = mean_g3) 
lines(xfit_g3, yfit_g3, col = "blue", lwd = 2)

#QQplots
qq_g1_pois <- ggplot(grades_data, aes(sample = G1))
qq_g1_pois +
  geom_qq(distribution = stats::qpois, dparams = list(lambda = mean_g1)) +
  geom_qq_line(distribution = stats::qpois, dparams = list(lambda = mean_g1))

qq_g2_pois <- ggplot(grades_data, aes(sample = G2))
qq_g2_pois +
  geom_qq(distribution = stats::qpois, dparams = list(lambda = mean_g2)) +
  geom_qq_line(distribution = stats::qpois, dparams = list(lambda = mean_g2))

qq_g3_pois <- ggplot(grades_data, aes(sample = G3))
qq_g3_pois +
  geom_qq(distribution = stats::qpois, dparams = list(lambda = mean_g3)) +
  geom_qq_line(distribution = stats::qpois, dparams = list(lambda =mean_g3))


#b)
#generalized linear model
#remove G2, G3 from the data frame 
drops <- c("G2","G3")
glm_data1 <- grades_data[ , !(names(grades_data) %in% drops)]

glm_data1

#generate a linear model, G1 depends on all other variables (hence, G2, G3 were removed)
glm_m1 <- glm(G1 ~ ., data = glm_data1 ,family = poisson())
summary(glm_g1)
#extract fitted values and get the real values
fitted_m1 <- glm_m1$fitted.values
realvalues_m1 <- glm_data1$G1

#compute pearson residuals and generate histogram to check if normal distributed
pearsonres_m1 <- (realvalues_m1-fitted_m1)/sqrt(fitted_m1)
pearsonres_m1
mean(pearsonres_m1)
var(pearsonres_m1)
hist(pearsonres_m1, freq = FALSE,  xlab = "Pearson Residuals", main = "Person Residuals for Model 1")
xfit <- seq(-3,3, by = 0.1) 
yfit <- dnorm(xfit) 
lines(xfit, yfit, col = "blue", lwd = 2)

#calculate anscoberes residuals and check if normal distributed
anscoberes_m1 <- 3*(realvalues_m1^(2/3)-fitted_m1^(2/3))/(2*fitted_m1^(1/6))
hist(anscoberes_m1, freq = FALSE, xlab = "Anscoberes Residuals", main = "Anscoberes Residuals for Model 1")
lines(xfit, yfit, col = "blue", lwd = 2)

mean(anscoberes_m1)
var(anscoberes_m1)

dim(glm_data1)

#c)
#first part
#remove all not significant variables
keep <- c("sex", "Fedu", "studytime", "failures", "schoolsup", "famsup", "goout", "G1")
glm_data2 <- grades_data[, names(grades_data) %in% keep]
glm_data2

#generate a new linear model depending only on the significant variables
glm_m2 <- glm(G1 ~ ., data = glm_data2, family = poisson)
summary(glm_m2)

#calculate pearson andanscoberes residuals and check if normal distributed
fitted_m2 <- glm_m2$fitted.values
realvalues_m2 <- glm_data2$G1

pearsonsres_m2 <- (realvalues_m2-fitted_m2)/sqrt(fitted_m2)
hist(pearsonsres_m2, freq = FALSE, xlab = "Pearson Residuals", main = "Pearson Residuals for Model 2")
lines(xfit, yfit, col ="blue", lwd = 2)

mean(pearsonsres_m2)
var(pearsonsres_m2)

anscoberes_m2 <- 3*(realvalues_m2^(2/3)-fitted_m2^(2/3))/(2*fitted_m2^(1/6))
hist(anscoberes_m2, freq = FALSE, xlab = "Anscoberes Residuals", main = "Anscoberes Residuals for Model 2")
lines(xfit, yfit, col ="blue", lwd = 2)

mean(anscoberes_m2)
var(anscoberes_m2)

#deviance give a chi square rv
dev_m2 <- glm_m2$deviance
dev_m3 <- glm_m1$deviance
dev_m2-dev_m3

#rejecte b_0, ...,b_27 = 0 if 
qchisq(0.95, 27) # < dev_m2-dev_m3
#do not reject the hypothesis


#2. part
#replace goout by Walc
glm_data3 <- glm_data2[, !names(glm_data2) %in% c("goout")]
glm_data3$Walc <- grades_data$Walc
glm_data3

#generate a new linear model
glm_m3 <- glm(G1 ~ . , data = glm_data3, family = poisson)
summary(glm_m3)

plot(glm_m3)

#calculate pearson and anscoberes residuals
fitted_m3 <- glm_m3$fitted.values
realvalues_m3 <- glm_data3$G1

pearsonsres_m3 <- (realvalues_m3-fitted_m3)/sqrt(fitted_m3)
hist(pearsonsres_m3, freq = FALSE, xlab = "Pearson Residuals", main = "Pearson Residuals for Model 3")
lines(xfit, yfit, col ="blue", lwd = 2)

mean(pearsonsres_m3)
var(pearsonsres_m3)

anscoberes_m3 <- 3*(realvalues_m3^(2/3)-fitted_m3^(2/3))/(2*fitted_m3^(1/6))
hist(anscoberes_m3, freq = FALSE, xlab = "Anscoberes Residuals", main = "Anscoberes Residuals for Model 3")
lines(xfit, yfit, col ="blue", lwd = 2)
plot(glm_m3)

