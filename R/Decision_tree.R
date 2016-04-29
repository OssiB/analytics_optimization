# In this lesson, we will perform decision tree 
# analysis on several datasets. As in the case of logistic regression, 
# this method is used for classification problem. First, we need to 
# install two packages for performing decision tree analysis 
# and for visualizing the tree

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# First we will do the analysis on the classification dataset used for
# logistic regression. First we split the dataset into training 
# and testing data

library(caTools)

set.seed(1985)

split <- sample.split(logistic_data$admit, SplitRatio = 0.75)

training_set <- subset(logistic_data, split == TRUE)
test_set <- subset(logistic_data, split == FALSE)

# We can build a tree using the three predictors and the admission variable as the 
# output class

tree_adm_1 <- rpart(admit ~ gre + gpa + rank, data = training_set, method="class")

# We can visualize the tree. When we interpret it, 
# after evaluating the condition, we move on to the right branch if 
# the observation satisfies it, to the left branch if not

prp(tree_adm_1)

summary(tree_adm_1)

# We can check the performance on the test set

Predict_adm <- predict(tree_adm_1, newdata = test_set, type = "class")

table(test_set$admit, Predict_adm)

# We can specify various options when creating the tree, for
# example the minimum number of observations in a final leaf
# Try different values: 100, 50, 40, 25

tree_adm_2 <- rpart(admit ~ gre + gpa + rank, data = training_set, method="class", minbucket = 25)

prp(tree_adm_2)

Predict_adm_2 <- predict(tree_adm_2, newdata = test_set, type = "class")

table(test_set$admit, Predict_adm_2)


# We continue by looking at the Haberman dataset 

colnames(haberman) <- c("Age", "Year", "Node", "Result")

set.seed(1985)

split <- sample.split(haberman$Result, SplitRatio = 0.75)

training_set <- subset(haberman, split == TRUE)
test_set <- subset(haberman, split == FALSE)

hab_tree  <- rpart(Result ~ Age + Year + Node, data = training_set, method="class")

# We can look at the result

prp(hab_tree)

# Check the predictions

Predict_hab <- predict(hab_tree, newdata = test_set, type = "class")

table(test_set$Result, Predict_hab)

# We can determine the optimal pruning of the tree 
# by specifying the complexity parameter, not onlly by guessing the minimum leaf value. 
# First we look at it for our tree and plot it with the relative error.

printcp(hab_tree)
plotcp(hab_tree)

# Then we can look at the tree that is optimal based on the complexity

prune_adm <- prune(hab_tree, cp=   hab_tree$cptable[which.min(hab_tree$cptable[,"xerror"]),"CP"])

# In this case, we actually get that just predicting 1 to everybody we get the best,
# most parsimonious model
prp(prune_adm)



