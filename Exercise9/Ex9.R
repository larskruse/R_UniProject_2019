library(nlme)
library(JoSAE)
library(tidyverse)
library(maps)


data("landsat")
landsat <- data.frame(landsat)
summary(landsat)

#remove the outlier
landsat <- landsat[-33,]

drops <- c("outlier")
landsat <- landsat[ , !(names(landsat) %in% drops)]
landsat


grouped_Corn <- groupedData(HACorn ~ PixelsCorn + PixelsSoybeans   | CountyName, data = landsat)
plot(grouped_Corn)
grouped_Corn_model <- lmList(grouped_Corn, data = landsat)
grouped_Corn_model

coef(lm(HACorn ~ PixelsCorn + PixelsSoybeans, data = landsat))

grouped_Soy <- groupedData(HASoybeans ~ PixelsCorn + PixelsSoybeans | CountyName, data = landsat)
plot(grouped_Soy)
grouped_Soy_model <-lmList(grouped_Corn, data = landsat)
grouped_Soy_model

# b ) 

lmeModel_Corn <- lme(HACorn ~ PixelsCorn + PixelsSoybeans, data = landsat, random = ~ 1 | CountyName)
summary(lmeModel_Corn)
coef(lmeModel_Corn)

lmeModel_Soy <- lme(HASoybeans ~ PixelsCorn + PixelsSoybeans, data = landsat, random = ~ 1 | CountyName)
summary(lmeModel_Soy)
plot(lmeModel_Soy)
coef(lmeModel_Soy)

#c)
landsat.domains <- unique(landsat[-33, c(1, 7:8, 10)])
landsat.domains

mu_soy <- rep(NA, 12)
mu_corn <- rep(NA,12)

beta_soy <- t(coef(lmeModel_Soy))
beta_corn <- t(coef(lmeModel_Corn))

#Regression predictor:
for (i in 1:12){
  x_corn <- as.matrix(landsat.domains$PixelsCorn[i])
  x_soy <-  as.matrix(landsat.domains$PixelsSoybeans[i])
  x <- cbind(1, matrix(c(x_corn, x_soy), ncol = 2))

  mu_soy[i] <- x %*%  as.matrix(beta_soy[,i])
  mu_corn[i] <- x %*%  as.matrix(beta_corn[,i])
  
}

mu_soy
mu_corn


#Survey predictor:
y_corn <- aggregate(landsat["HACorn"], list(landsat$CountyName), mean)
y_soy <- aggregate(landsat["HASoybeans"], list(landsat$CountyName), mean)
y_corn <- y_corn$HACorn
y_soy <- y_soy$HASoybeans


#Adjusted survey predictor:
mu_1_corn <- rep(NA,12)
mu_1_soy <- rep(NA,12)
x_1_soy <- rep(NA, 12)
x_1_corn <- rep(NA, 12)
for (i in 1:12){
  x_i_corn <-as.vector(aggregate(landsat$PixelsCorn, list(landsat$CountyName), mean))$x[i]
  x_i_soy <- as.vector(aggregate(landsat$PixelsSoybeans, list(landsat$CountyName), mean))$x[i]
  
  x_i_col <- cbind(1, matrix(c(x_i_corn, x_i_soy), ncol = 2))

  x_1_soy[i] <- x_i_col %*%  as.matrix(beta_soy[,i])
  x_1_corn[i] <- x_i_col %*%  as.matrix(beta_corn[,i])
}

mu_1_corn <- mu_corn + (y_corn - mu_1_corn)
mu_1_soy <- mu_soy + (y_soy - mu_1_soy)

#(Empirical) BLUP:



blup_corn <- mu_corn + (y_corn - mu_1_corn)
blup_soy <- mu_soy + (y_soy - mu_1_soy)

mu_soy
mu_corn

y_soy
y_corn

mu_1_soy
mu_1_corn
#d)
library(raster)
USA<-getData("GADM", country="USA", level=1)

USA$NAME_1

Iowa <- subset(USA, NAME_1 = "Iowa")
Iowa
plot(Iowa)

#
counties <- (Kenya1$NAME_1)
zmean <- rep(NA,length(counties))

#calculate the mean of zstunt for all counties with given data
for (i in 1:(length(counties))){
  zmean[i] <- mean(filter(childrean_data_clean, adm2 == toupper(counties)[i])$zstunt)
}

#create a data frame of county names and z-score means
NAME_1<-Kenya1@data$NAME_1
zmean_df<-data.frame(NAME_1, zmean)

#join the map and z-score data frame
Kenya1@data$id <- rownames(Kenya1@data)
Kenya1@data <- inner_join(Kenya1@data, zmean_df, by="NAME_1")
Kenya1_df <- fortify(Kenya1)
Kenya1_df <- inner_join(Kenya1_df,Kenya1@data, by="id")

#data frame for printing county names on the map with long and lat values
cnames <- aggregate(cbind(long, lat) ~ NAME_1, data=Kenya1_df, 
                    FUN=function(x)mean(range(x)))

#plotting the maps
ggplot() + 
  #fill the counties according to values in zmean
  geom_polygon(data = Kenya1_df, aes(x = long, y = lat, group = group, fill =
                                       zmean), color = "black", size = 0.25)+
  #set the title
  labs(title="Mean Z-Score of Children in Kenya per County")+
  #print the names of the counties on the map
  geom_text(data=cnames, aes(x = long, y = lat, label = NAME_1), size=3) 
















landsat



library(rsae)
data(landsat)
landsat.domains <- unique(landsat[-33, c(1, 7:8, 10)])
landsat.domains$domain.ID <- 1:nrow(landsat.domains)
names(landsat.domains)[2:3] <- c("PixelsCorn", "PixelsSoybeans")
tmp <- landsat[-33, c(2:6, 10)]
landsat.sample <- merge(landsat.domains[4:5], tmp, by = "CountyName")

landsat.domains
landsat.sample

summary(landsat.lme <- lme(HACorn ~ PixelsCorn + PixelsSoybeans, data = landsat.sample, random = ~1 | domain.ID))


result <- eblup.mse.f.wrap(domain.data = landsat.domains, lme.obj = landsat.lme)
result

