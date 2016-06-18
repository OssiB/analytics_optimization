setwd("~/Development/analytics_optimiztion")
#Exercise 1 Part 1
who <- read.table(file="data/WHO.csv",header=TRUE,sep = ",",colClasses = c("factor","factor",rep("numeric",7)))
head(who)
str(who)
summary(who)
pairs(who([,2:9]))
library(car)
scatterplotMatrix(who[3:9])
#There is a strong negative correlation between life expectancy and child mortality
cor(who[3:9])
who_numeric <- who[3:9]
lapply(who_numeric,shapiro.test)
#Only Cellular Subscribers distribution is normal
#First try 
attach(who_numeric)
all_lm <- lm(LifeExpectancy ~ Under15 + Over60 + FertilityRate + ChildMortality + CellularSubscribers + Population )
summary(all_lm)
# Under15 and ChildMortality are the most important variables
final_model <- lm(LifeExpectancy ~ Under15 + ChildMortality)
summary(final_model)
library(leaps)
leaps<-regsubsets(LifeExpectancy ~ Under15 + Over60 + FertilityRate + ChildMortality + CellularSubscribers + Population ,data=who_numeric,nbest=4)
# view results 
summary(leaps)
plot(leaps,scale ="r2")
#Let's try two ChildMortality + Under15 +Under15:ChildMortality, if they have any interaction
final_model2 <- lm(LifeExpectancy ~ Under15 + ChildMortality+Under15:ChildMortality )
summary(final_model2)
#MASS eliminate variable one by one
library(MASS)
stepAIC(all_lm,direction = "backward")
#Exercise Part 2
timeseries_data <- scan("data/timeseries.csv")
#Helper function, which return N elements from vector before index i
lastN <- function(timeseries,N,i){
     start <- i - N
     end <- i- 1
     timeseries[start:end]
}
naiv  <- function(timeseries) {
	first <- timeseries[1];
	last_index <- length(timeseries) - 1
	data <- c(first,timeseries_data[1:last_index]);
	data;
}


mav <- function(timeseries,N = 2) {

	data <- vector();
	data[1:N] <- timeseries[1:N];
	last_index <- length(timeseries);
	for (index  in N:last_index-1) {
        data[index+1] <- mean(lastN(timeseries,N,index+1));
    }
    data;
}


#Weighted moving average,optimize the value of the weight w 
weighted_ma <- function(timeseries,weights){

	data <- vector();
	size <- length(weights);
	last_index <- length(timeseries);
	data[1:size] <- timeseries[1:size];
	for (index  in size:(last_index-1)) {
        data[index+1] <- sum(lastN(timeseries,size,index+1)*weights);
  }
  data;
}
#Exponential smoothing, first value as inital forecast 
exp_smoothing <- function(timeseries,alpha) {
	data <- vector()
	data[1] <- mean(timeseries[1:2])
	x <- data[1]
	for (index in 2:length(timeseries)){
		data[index] <- alpha*timeseries[index-1] +(1- alpha)*data[index-1];
	}
	data;
}

cum_error <- function(timeseries,estimate) {

	sum(timeseries - estimate)
}

avg_errors <- function(timeseries,estimate) {

	mean(timeseries -estimate)
}

mean_abs_error <- function(timeseries,estimate){

	mean(abs(timeseries - estimate))
}

mean_abs_squared <- function(timeseries,estimate){

	mean((timeseries - estimate)^2)
}

MAPE  <- function(timeseries,estimate) {
    mean(abs((timeseries - estimate)/timeseries))
}
# Method error calculation
method_errors <- rep(0,4)
methods <- c("naive","ma","wma","expsm")
# Naive error 
methods["naive"] <- MAPE(timeseries_data,naiv(timeseries_data))
# Mean value error
methods["ma"] <- MAPE(timeseries_data,mav(timeseries_data))
# Optimize w using values 0,0.01 ..,0.99,1.00
index <- 1 
weights <- seq(0,1,by = 0.01)
errors_wma <- rep(0,length(weights))
for (w in weights){
  errors_wma[index] <- MAPE(timeseries_data,weighted_ma(timeseries_data,c(w,1-w)));
  index <- index +1;
}

index <- 1 
alphas <- seq(0,1,by = 0.01)
errors_smooth <- rep(0,length(alphas))
for (alpha  in alphas) {
  errors_smooth[index] <- MAPE(timeseries_data,exp_smoothing(timeseries_data,alpha));
  index <- index +1;
}
errors["naive"] <- MAPE(timeseries_data,naiv(timeseries_data))
errors["ma"] <- MAPE(timeseries_data,ma(timeseries_data,2))
errors["wma"] <- errors_wma[which(errors_wma == min(errors_wma))]
errors["expsm"] <- errors_smooth[which(errors_smooth == min(errors_smooth))]
errors[which(errors == min(errors))]
# So the exponential smoothing shows the best performance




