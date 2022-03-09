library(tidyverse)
library(pls)
require(Matrix)

likes <- read.csv("Dokumente/Studium/Master/6.Semester/likes.csv")
users <- read.csv("Dokumente/Studium/Master/6.Semester/users.csv")
users_likes <- read.csv("Dokumente/Studium/Master/6.Semester/users-likes.csv")

users_likes$user_row <- match(users_likes$userid,users$userid)
users_likes$like_row <- match(users_likes$likeid,likes$likeid)


UL <- sparseMatrix(i=users_likes$user_row, j = users_likes$like_row, x = 1)
rownames(UL) <- users$userid
colnames(UL) <- likes$likeid


UL <- UL[rowSums(UL) >= 60, colSums(UL) >= 120]  
#80 , 150
#60, 120

users <- users[match(rownames(UL), users$userid),]

ULnorm <- as.matrix(UL)

#part 2)

set.seed(1122)
testULIndex <- sort(sample(1:dim(ULnorm)[1], dim(ULnorm)[1]/3))
trainingULIndex <- 1:dim(ULnorm)[1]
trainingULIndex <- trainingULIndex[-testULIndex]


testUL <- ULnorm[testULIndex,]
trainingUL <- ULnorm[trainingULIndex,]

testAge <- users$age[testULIndex]
trainingAge <- users$age[trainingULIndex]

trainingsdata <- data.frame(trainingUL)

trainingsdata$age <- trainingAge



a <- plsr(age ~ . , data = trainingsdata, ncomp = 50 )
gc()

pred <- predict(a, testUL)
gc()

corr <- rep(NA,50)

for (i in 1:50){
  corr[i] <-   sum((pred[,1,i]-mean(pred[,1,i]))*(testAge-mean(testAge)))/(sqrt(sum((pred[,1,i]-mean(pred[,1,i]))^2))*sqrt(sum((testAge-mean(testAge))^2)))
}


plot(1:50,corr,  col = "blue")

d_opt <- match(max(corr),corr)
d_opt
plot(testAge, pred[,1,d_opt], col = "grey40")
abline(0,1, col = "blue")


values <- a$coefficients[,1,8]
h <- head(sort(values), 6)
t <- tail(sort(values), 6)

hnames <- names(h)
tnames <- names(t)

matchbest <- match(hnames, make.names(likes$likeid))
matchworse <- match(tnames, make.names(likes$likeid))

likes[matchbest,]
likes[matchworse,]
