library(tidyverse)
library(haven)

#read in the data from STATA format
childrean_data <- read_dta("childrenfinal.dta")
summary(childrean_data)

#remove alle variables m, v, s + number
childrean_data_clean <- childrean_data[!grepl("^m\\d|^v\\d|^s\\d", names(childrean_data))]


sapply(childrean_data_clean, class)
#removing all labels
childrean_data_clean <- zap_labels(childrean_data_clean)
#set ruralfactor and female to factor class
childrean_data_clean$ruralfacto <- as_factor(childrean_data_clean$ruralfacto)
childrean_data_clean$female <- as_factor(childrean_data_clean$female)

#select the variables to use
small_data_set <- dplyr::select(childrean_data_clean, hypage, ruralfacto, female, zstunt, zweight, zwast, adm2) 

#plot age vs z-score with smooth line
ggplot(data=small_data_set, aes(x=hypage, y=zstunt)) + 
  geom_point()+
  geom_smooth()+
  labs(title="Age vs Z-Score", x = "Age in Months", y = "Z-Score")

#plot age vs z-score divided by gender
ggplot(data=small_data_set, aes(x=hypage, y = zstunt, col = female))+
  geom_smooth()+
  labs(title="Age vs Z-Score by Gender", x = "Age in Months", y = "Z-Score", col = "Female")

#plot age vs z-score divided by ruralfactor
ggplot(data=small_data_set, aes(x=hypage, y = zstunt, col = ruralfacto))+
  geom_smooth()+
  labs(title="Age vs Z-Score by Rural Factor", x = "Age in Months", y = "Z-Score", col = "Rural Area")


#c) Map of Kenia
#downloading the map data
Kenya1<-getData("GADM", country="KE", level=1)

#the names in adm2 are not the same as the names in the map data. Therefore, change the wrong names
childrean_data_clean$adm2[childrean_data_clean$adm2 == "HOMA_BAY"] <- toupper("Homa Bay")
childrean_data_clean$adm2[childrean_data_clean$adm2 == "MURANGA"] <- toupper("Murang'a")
childrean_data_clean$adm2[childrean_data_clean$adm2 == "TRANS-NZOIA"] <- toupper("Trans Nzoia")
childrean_data_clean$adm2[childrean_data_clean$adm2 == "E. MARAKWET"] <- toupper("Elgeyo-Marakwet")
childrean_data_clean$adm2[childrean_data_clean$adm2 == "NITHI"] <- toupper("Tharaka-Nithi")

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



#c)
#write tibble to text file, separate by \t
write.table(small_data_set, file = "childrean_data.txt", sep = "\t",
            row.names = FALSE)
