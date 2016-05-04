#setwd("~/Development/analytics_optimiztion")
#who <- read.csv2(file ="data/WHO.csv,header =TRUE")

timeseries_data <- scan("data/timeseries.csv")

naiv  <- function(timeseries) {
	first <- timeseries[1];
	last_index <- length(timeseries) - 1
	data <- c(first,timeseries_data[1:last_index]);
	data;
}
naiv_error  <- mean_abs_percent_error(timeseries_data,naiv(timeseries_data)) 

mav <- function(timeseries,N = 2) {

	data <- vector();
	data[1:N] <- timeseries[1:N];
	last_index <- length(timeseries);
	for (index  in N:last_index) {
        data[index] <- mean(timeseries[(index + 1-N) :index]);
    }
    data;
}
mav_error <- mean_abs_percent_error(timeseries_data,mav(timeseries_data))

weighted_ma <- function(timeseries,weights){

	data <- vector();
	size <- length(weights);
	last_index <- length(timeseries);
	data[1:size] <- timeseries[1:size];
	for (index  in size:last_index) {
        data[index] <- sum(timeseries[(index + 1 - size) : index] * weights);
    }
    data;
}

exp_smoothing <- function(timeseries,alpha) {

	data <- vector()
	data[1] <- timeseries[1]
	x <- data[1]
	for (index in 2:length(timeseries)){
		data[index] <- alpha*x +(1- alpha)*timeseries[index-1]
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

mean_abs_percent_error  <- function(timeseries,estimate) {
    mean(abs((timeseries - estimate)/estimate)
}


