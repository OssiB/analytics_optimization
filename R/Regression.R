
# In the first example, we try to determine whether 
#there is a linear relationship between the size of a house 
#and the market value. The data is contained in the file estate

estate <- read.csv2("estate.csv")

attach(estate)


# When we create a scatterplot, we can recognize some kind of linear relationship 
#between the two variables

plot(Market_Value~Size)

estate1 <- estate[which(Size < 10),]

plot(Market_Value~Size, data = estate1)

# Next we create a linear regression model using the function lm(). First always 
#write the dependent variable, then the predictor

estate_fit <- lm(Market_Value~Home_size)

# Then we can look at the details of the regresion model, estimated coefficients, 
# significance, R^2 value. Te last F-statistic concerns the hypothesis that all 
# the coefficients of the model are 0, in the sense that the sample mean 
#of Market_Value is a better predictor than Home_size
summary(estate_fit)

# We can perform the same model building with different variables, 
#in the following with Size, which represent the size of the area on which the 
#house is built
plot(Size~Market_Value)

estate_fit1 <- lm(Market_Value~Size)

summary(estate_fit1)

# We can use the created model to estimate the market value for a new observation 
# using the predict function. In the next row, we check what is the predicted 
#market value for a home with size 125.

predict(estate_fit, data.frame(Home_size=125))

# We can also calculate the confidence interval for the coefficients
confint(estate_fit, level=0.95)

# Finally, we need to check whether the regression assumptions are fulfilled
plot(Market_Value~Home_size)

# Linear relationship
abline(estate_fit)

# residuals seems random
plot(Home_size,estate_fit$residuals)

res <- scale(estate_fit$residuals)

# Normality of errors

hist(res)

detach(estate)

# In the next example, we will see a case with multiple independent variables
# The goal is to predict graduation rate using different attributes of the university 
# and students
attach(study1)

str(study1)

# We create a multiple linear regression model with four independent variables
grad_fit <- lm(Graduation~SAT+Acc_Rate+Exp+HS)

# We can see differences in the significance levels
summary(grad_fit)

# We have to check the same assumptions
plot(HS,grad_fit$residuals)

library("car")

influencePlot(grad_fit, id.n=3)

# Checking normality
qqPlot(grad_fit)

# Variance inflation factor: how much the variation in the coefficient is increased 
#because of multicollinearity. To run this function, you have to install and 
# load the package car first

install.packages("car")

library("car")

vif(grad_fit)

detach(study1)

# We will investigate in the following how to improve a regression model 
# by excluding some predictors based on different criteria. 
# We will use a dataset about balance information
attach(bank)

bank_fit <- lm(Balance~Age+Education+Income+Home.Value+Wealth)

# We check which predictors' contribution is not significant
summary(bank_fit)

vif(bank_fit)

# We remove Home.Value as it has the highest p-value
bank_fit1 <- lm(Balance~Age+Education+Income+Wealth)

# The explained variance increased, but not much
summary(bank_fit1)

# We go back to the university dataset to select variables based on correlation
study2 <- data.frame(study1[,3:6])
# no correlation above 0.7
cor(study2)

# For the bank data, we have seen that the VIF is very high for some, now we check 
# the correlation; it is very high for Income, Wealth
cor(bank)

# We remove wealth additionally to Home.value
bank_fit2 <- lm(Balance~Age+Education+Income)

summary(bank_fit2)

# New model: we remove income instead of wealth
bank_fit3 <- lm(Balance~Age+Education+Wealth)

summary(bank_fit3)

detach(bank)

# In the next example, we will look at how to use categorical variables 
#in the regression. In the example, we try to determine 
# whether age or having an MBA increases salary more

library(car)

attach(salary)

# We need to recode MBA into a binary variable to use it in a regression model
salary$MBA <- recode(salary$MBA, "'Yes'=1; 'No'=0", as.factor.result=FALSE)

salary_fit <- lm(salary$Salary~salary$Age+salary$MBA)

summary(salary_fit)

# In the next model, we suppose that the combination of age and MBA provides a good 
#predictor

interaction <- salary$Age * salary$MBA

interaction

# We attach it as a new column to the dataset

salary1 <- cbind(salary,interaction)

# It is of course a numerical variable

str(salary1)

salary_fit1 <- lm(salary1$Salary~salary1$Age+salary1$MBA+salary1$interaction)

# The explained variance increases and MBA alone is not significant
summary(salary_fit1)

# So we can exclude MBA from the model
salary_fit2 <- lm(salary1$Salary~salary1$Age+salary1$interaction)

summary(salary_fit2)

# Sometimes we can have a situation ,when it is reasonable to assume that 
# different other functions of the variables should be included in the model
# in the following example, the square of age

salary_fit3 <- lm(salary1$Salary~I(salary1$Age^2)+salary1$interaction)

summary(salary_fit3)

detach(salary)

# In the following, we will look at the advertising data that we used in 
# exploratory data analysis. We concluded that TV is the best candidate for being 
# a good linear predictor
attach(Advertising)

fit <- lm(Sales~TV, data= Advertising)

summary(fit)

fit1 <- lm(Sales~Radio, data= Advertising)

summary(fit1)

# We can see the large differences in the R^2 values
fit2 <- lm(Sales~Newspaper, data= Advertising)

summary(fit2)

plot(fit)

res <- residuals(fit)

plot(res)

fitted(fit)

#We can also compare models using ANOVA

fit1 <- lm(Sales ~ TV + Radio, data=Advertising)
fit2 <- lm(Sales ~ TV + Newspaper)
anova(fit1, fit2) 

# Finally, to see the R^2 for all the possible predictor combinations
install.packages("leaps")
library(leaps)

leaps<-regsubsets(Balance~Age+Education+Income+Home.Value+Wealth,data=bank,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")





