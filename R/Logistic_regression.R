# In the following, we will go through the steps of performing
# logistic regression and multinomial regression
# We will use the dataset in the file logistic_data
# Open it through the Import dataset button in the Environment tab on the right

View(logistic_data)

# Binary dependent variable: admit
# There are three predictor variables: gre (numeric), gpa (numeric) and rank (1-4). 
# Graduate Record Exam scores, grade point average and prestige of the 
# undergraduate institution
# First we look at the basic statistics of the variables

summary(logistic_data)

# As we discussed, in case of binary dependent variable, 
# we use logistic regression instead of simple linear regression
# First we need to convert rank into a factor variable

logistic_data$rank <- factor(logistic_data$rank)

# Then we perform logistic regression using the function glm

logit <- glm(admit ~ gre + gpa + rank, data = logistic_data, family = "binomial")

summary(logit)

str(logit)

# To interpret the results, we look at the estimate of the coefficients
# In logistic regression they are calculated with respect to the log odds of 
# the value of the binary variable (admit)
# For example, if the probability of admitted in p=.6, then the odds=p/(1-p)=1.5
# First we look at the sign: positive implies positive effect on the odds
# The closer the value to 0 is, the less influence the predictor has
# For example, every one unit change in gre, the log odds of admission
# (versus non-admission) increases by 0.002 (not a very high value)
# Every one unit change in gpa, the log odds of admission
# (versus non-admission) increases by 0.804
# For rank, rank of 2 versus a rank of 1 institution
# changes the log odds of admission by -0.675 (e.g. worse institution 
# decreases the chance of admittance)


# We can check the significance of the predictors.
# We need the package aod, so first you have to install and load it

install.packages("aod")
library("aod")

# Then we can check whether the prediction based on rank is significant
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)

# To make the interpretation of the estimations easier, we can transform them
# into the odds (from the log odds)

exp(coef(logit))

# In the following, we will build a model again,
# but based only on a subset of the whole dataset. To do this,
# first we need to divide the data into training set for model 
# building and test set for evaluation. 

install.packages("caTools")
library(caTools)

# We can randomly split the data into two subsets.
# First we set the seed. It ensures that if we run the analysis 
# several times, the splitting will always be the same.

set.seed(1985)

# In the following line, we split the data set in a way that 
# 75% will be in the training set. More importantly, the splitting 
# happens in a way that the proportion of admiited students 
# will be the same in both training and test sets

split <- sample.split(logistic_data$admit, SplitRatio = 0.75)
split

# Based on the split, we can specify the training and testing sets

training_set <- subset(logistic_data, split == TRUE)
test_set <- subset(logistic_data, split == FALSE)

logit <- glm(admit ~ gre + gpa + rank, data = training_set, family = "binomial")

summary(logit)

# Specify predictions on training set

predict_training <- predict(logit, type="response")

# Confusion matrix for threshold of 0.5
table(training_set$admit, predict_training > 0.5)

# Sensitivity and specificity
23/(23+72)
186/(186+19)

# Confusion matrix for threshold of 0.7
table(training_set$admit, predict_training > 0.7)

# Sensitivity and specificity
2/(93+2)
204/(204+1)

# Confusion matrix for threshold of 0.2
table(training_set$admit, predict_training > 0.2)

# Sensitivity and specificity
84/(11+84)
75/(75+130)








