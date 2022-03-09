#Part a)

#setting the random number generator
RNGkind("Wichmann-Hill")

#set total number of random values
N <- 1000

#using built in rbinom
buildInBinom <- rbinom(N,10,0.4)



#using bernoulli variables
bernoul <- runif(10*N, 0,1)
bernoul
#setting the values to 0 if < 0.4 and 1 of > 1-0.4
#putting values in a 10x1000 matrix to make summing easier 
bernoul_matrix <- matrix(as.integer(bernoul > 0.6), nrow = 1000, ncol=10)
bernoul_matrix
#the sum of 10 bernoulli variables is a binomial variable with parameter 10
bernoulli <- rowSums(bernoul_matrix)


#inverse method for binomial distribution
uniformVars <- runif(1000, 0, 1)

#depending on the inverval in which the random number lies, it is set to a number between 0 and 10
#the inveral length are equal to the prob that the assinged value is obtained by the bernoulli experiment
binomFunc <- function(x){
  if(0 <= x && x < 0.006) return(0)
  if(0.006 <= x && x < 0.0463) return(1) 
  if(0.0463 <= x & x < 0.1672) return(2)
  if(0.1672 <= x & x < 0.3822) return(3)
  if(0.3822 <= x & x < 0.633) return(4)
  if(0.633 <= x && x < 0.8337) return(5)
  if(0.8337 <= x && x < 0.9452) return(6)
  if(0.9452 <= x && x < 0.9877) return(7)
  if(0.9877 <= x && x < 0.9983) return(8)
  if(0.9983 <= x && x < 0.9999) return(9)
  if(0.9999 <= x && x <=1) return(10)
  }

#apply that function to all random number generated
fullyInverse <- sapply(uniformVars, FUN=binomFunc)


#generate the distribution function
buildInBinom_ecdf <- ecdf(buildInBinom)
bernoulli_ecdf <- ecdf(bernoulli)
fullyInverse_ecdf <- ecdf(fullyInverse)


#plotting the distribution functinos
plot(buildInBinom_ecdf, verticals=TRUE, do.points=FALSE, main = "Binomial Distribution Functions")
plot(bernoulli_ecdf, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
plot(fullyInverse_ecdf, verticals=TRUE, do.points=FALSE, add=TRUE, col='red')


#setting the random number generator to default
RNGkind("default")


#Part b)
#number of variables to generate
N <- 10000

#the variable c has to be computed such that Ucg(X) <= f(X). See report
c <- 1.5203

#the inverse Cauchy distribution function according to the lecutre notes
invertCauchy <- function(x){
  tan(pi*(x-1/2))
}

prob <- rep(NA, 1000)

for (i in 1:1000){

#generating unif(0,1) random numbers and applying the inverse Cauchy function
unif <- runif(N,0,1)
invcauchy <- sapply(unif, invertCauchy)

#check if Ucg(X) <= f(X)
#generate random numbers U
unifor <- runif(N,0,1)
#calculate Ucg(X) with g(X) being the cauchy density function
check <- c*unifor*dcauchy(invcauchy)

#determing the valid random numbers
vars <- invcauchy[which((check <= dnorm(invcauchy)))]

#calculating the real and exprecte probabilties to find a valid random number
prob[i] <- sum(as.integer(check <= dnorm(invcauchy)))/10000
prob
expProb <- 1/c
expProb
}
summary(prob)
#plotting a histogram of the normal dist random numbers with std. norm. distribution density
hist(vars, freq = FALSE, ylim = c(0,0.4), xlim = c(-4,4), xlab = "",  main = "Distribution of Simulated Normal Variables")
xfit <- seq(-4, 4, length = 100) 
yfit <- dnorm(xfit) 
lines(xfit, yfit, col = "blue", lwd = 2)

#qq-plot
qqnorm(vars)
qqline(vars)

