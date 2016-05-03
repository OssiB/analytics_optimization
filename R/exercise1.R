#setwd("~/Development/analytics_optimiztion")
#who <- read.csv2(file ="data/WHO.csv,header =TRUE")

timeseries_data <- scan("data/timeseries.csv")
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
	for (index  in N:last_index) {
        data[index] <- mean(timeseries[(index + 1-N) :index]);
    }
    data;
}
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
exp_smoothing(timeseries_data,0.25);
