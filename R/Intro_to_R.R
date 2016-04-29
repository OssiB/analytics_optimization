
library(forecast)

?mean

Age <- c(35, 60, 24, 22)

Age

Age[1] #the first element of Age

Age[1:3] # the first three values in Age

Age[c(1,4)] # the first and fourth elements of Age

Job <- c("Lecturer", "Lecturer", "Student", "Student")

Job <- factor(Job)

Login <- c("Daily", "Weekly", "Weekly", "Monthly")

Login <- factor(Login, levels=c("Monthly", "Weekly", "Daily"), order=TRUE)

Admin <- c(TRUE, TRUE, FALSE, FALSE)

Data <- data.frame(Job, Age, Admin, Login)

Data

View(Data)

dim(Data) # dimensions of the data frame (4 variables, 4 observations)

str(Data) # structure of the data frame (type of variables)

names(Data) # thenames of the variables in the data frame

Data$Job # the Job variable in the data frame

Data[1:2] # the first two variables of the data frame, in this case Job and Age

Data[c("Job", "Age")] # the same as the previous, Job and Age

Job_Age <- Data[c("Job", "Age")] # you create a new data frame of Age and Job

Data[which(Data$Job == "Student"),] #

Data[which(Data$Age < 50),]

#Visualization examples

Advertising <- read.csv("Advertising.csv")

attach(Advertising)
#here we create a scatterplot
plot(TV, Sales)
#we add a regression line
abline(lm(Sales~TV))

title("Regression of TV on Sales")

plot(Radio, Sales)
#we add a regression line
abline(lm(Sales~Radio))

title("Regression of Radio on Sales")

plot(Newspaper, Sales)
#we add a regression line
abline(lm(Sales~Newspaper))

title("Regression of Newspaper on Sales")

pairs(~mpg+disp+drat+wt, data=mtcars, main="Basic Scatter Plot Matrix")

library(car)

scatterplotMatrix(~ mpg + disp + drat + wt | cyl, data=mtcars,
                  spread=FALSE, diagonal="histogram",
                  main="Scatter Plot Matrix via car Package")

library(Rcmdr)
attach(mtcars)
scatter3d(wt, disp, mpg)







