#After importing the credit data set, you can take a look at it

Credit <- read.csv2("data/Credit.csv")

View(Credit)

#We start with univariate for categorical variables. We can create frequency table

cards.table <- table(Credit$Cards)
cards.table

#and proportions table

prop.table(cards.table)

#We continue with univariate for numerical variable. You can look at individually at mean

mean(Credit$Age)

#or standard deviation 

sd(Credit$Age)

#or others. In the following line, we simplify our typing: after this, we can use 
#simply the variable names and no need to write Credit$...

attach(Credit)

#Try this

Rating

# The basic command to obtain the most important descriptive statistics is summary
summary(Rating)
summary(Balance)

# There are several ways to see more detailed list of descriptors, try the following ones

install.packages("Hmisc")

library(Hmisc)

describe(Credit)

install.packages("pastecs")

library(pastecs)

stat.desc(Credit, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)

install.packages("psych")

library(psych)

describe(Credit)

# Multivariate non-graphical statistics for categorical variables include the two-ways table

table(Gender, Cards)

table(Gender, Married)

# We can combine categorical and numerical variables: compare the balance for different
# genders and people with different number of credit cards 

aggregate(Balance, by= list(Gender), FUN= summary)

aggregate(Balance, by= list(Cards), FUN= summary)

#table() can also be done with more than 2 variables

table(Gender, Student, Married)

#The basic statistics for analysing relationship between numerical variables is correlation.
#First we create a data frame that contains only the numerical variables.
Credit_num <- Credit[c(1:6,11)]
#The following command calculates the correlation for all the pairs of variables
cor(Credit_num)
#If we find some that seems high enough, we can test whether it is significant
cor.test(Income,Limit)
#We can make sure that low values actually correspond to non-significant differences
cor.test(Income,Education)

library(psych)
corr.test(Credit_num, use="complete")

#Next we go through basic graphical methods. The first type of plot is histogram.
hist(Age)
#You can specify the number of bins on the plot
hist(Age, breaks=20)

#We can also plot a type of density function
plot(density(Age))
plot(density(Balance))

#For categorical variables, barplot and stacked barplot can provide useful information 
barplot(table(Cards))
gen_card <- table(Gender, Cards)
barplot(gen_card)

#Boxplots

boxplot(Balance~Cards)
boxplot(Balance~Education)
boxplot(Balance~Gender)

#t-test

t.test(Balance~Gender)

t.test(Balance~Married)

t.test(Balance~Student)

#ANOVA

aov1 <- aov(Balance~Student)

summary(aov1)

aov2 <- aov(Balance~Education)

summary(aov2)

aov3 <- aov(Balance~Cards)

summary(aov3)

