install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")
library(ggplot2) # Data visualization
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)

data <- read.csv("../input/bank.csv",sep = ";")
idx <- sample(1:nrow(data),8)
data[idx,]
sapply(data, class)

#Data Cleaning
data$age <- as.numeric(data$age)
data$balance <- as.numeric(data$balance)
data$day <- as.numeric(data$day)
data$duration <- as.numeric(data$duration)
data$campaign <- as.numeric(data$campaign)
data$pdays <- as.numeric(data$pdays)
data$previous <- as.numeric(data$previous)

summary(data)

#Creation of testing and training dataset
size <- nrow(data) * 0.8
na.omit(data)
data_bank <- sample(1:nrow(data), size = size)
data_testing <- data[-data_bank,]	# Testing Data
data_training <- data[data_bank,]		# Training Data
education_testing <- education[-data_bank]
age_testing <- age[-data_bank]
summary(data_training)

#Plot decision tree
data_training.rpart <- rpart(y ~ ., data = data_training)
fancyRpartPlot(data_training.rpart)
predictions <- predict(data_training.rpart, data_bank, type = "class")
confusion.matrix <- prop.table(table(predictions, data_training$y))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 
accuracy	#  0.9156527
confusion.matrix

#Linear Regression Classification
attach(data_training)
lm.bank <- lm(age~., data=data_training)
summary(lm.bank)
plot(lm.bank)
	
plot(age, balance, main="Scatterplot", xlab="Age", ylab="Bank Data") ## type = numeric	
#points(job) 		## type = factor
#points(marital) 		## type = factor
#points(education) 	## type = factor
#points(default) 		## type = factor
#points(housing)		## type = factor
#points(loan)		## type = factor
#points(contact)		## type = factor
points(day, col=1)	## type = numeric
#points(month)		## type = factor
points(duration, col=2)	## type = numeric
points(campaign, col=3)	## type = numeric
points(pdays, col=4)	## type = numeric
points(previous, col=5)	## type = numeric
#points(poutcome)		## type = factor
#points(y)			## type = factor
abline(lm.bank, col="red", lwd=3)

# data_bank == training data
# Fit a tree based on training data
data_training_tree_reg <- tree(age~., data_training)
data_training_tree_reg
plot(data_training_tree_reg)
text(data_training_tree_reg, pretty=0)

# Check how the model stands with testing data
data_training_predict_reg <- predict(data_training_tree_reg, data_testing)
mean((data_training_predict_reg - age_testing)^2) # Mean Square Error = 162.286

# Cross validation to check level of pruning 
data_training_cvtree_reg <- cv.tree(data_training_tree_reg)
names(data_training_cvtree_reg) # Deviance = Error Rate
plot(data_training_cvtree_reg$size, data_training_cvtree_reg$dev, xlab="Tree Size", ylab="Mean Square Error", type="b") # Minimum error rate is when the size of the tree is 6
which.min(data_training_cvtree_reg$dev) # 1
data_training_cvtree_reg$size[1] # 6

# Pruning is not going to reduce the Mean Square Error because we can get 
# the minimum Mean Sqaure Error only when we consider the full tree which 
# is not pruned.
# Pruning is  irrelavent here.

#Decision Tree Classification
#Fit the tree model using the training data
data_training_tree <- tree(education~., data_training)
plot(data_training_tree)
text(data_training_tree, pretty = 0)

# Test this tree model using testing data
data_training_predict <- predict(data_training_tree, data_testing, type="class")

# Misclassification Error
mean(data_training_predict != education_testing) # 0.5502703 ~= 55%

# Cross validation to check level of pruning 
set.seed(3)
data_training_cvtree <- cv.tree(data_training_tree, FUN=prune.misclass)
names(data_training_cvtree) # Deviance = Error Rate
plot(data_training_cvtree$size, data_training_cvtree$dev, xlab="Tree Size", ylab="Error Rate", type="b") # Minimum error rate is when the size of the tree is 4

# Pruning the tree when size is 2
data_training_prune <- prune.misclass(data_training_tree, best=2)
plot(data_training_prune)
text(data_training_prune, pretty=0)

# Test this prune model using testing data
data_training_prune_predict <- predict(data_training_prune, data_testing, type="class")

# Misclassification Error
mean(data_training_prune_predict != education_testing) # 0.518232 ~= 51.8%