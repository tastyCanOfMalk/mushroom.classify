
# https://www.kaggle.com/supriya675/mushroom-is-edible-or-not
# https://www.kaggle.com/shrirangphadke/randomforest-variable-importance-mushroom-dataset 


if(!require(tibble)) install.packages("tibble")
if(!require(corrplot)) install.packages("corrplot")
if (!require(tree)) install.packages("tree")
if (!require(randomForest)) install.packages("randomForest")
library(tibble)
library(corrplot)
library(tree)
library(randomForest)

# Load and look

setwd("/home/e/R/mushroom.classify")
x <- read.csv("data/mushrooms.csv")
glimpse(x)
summary(x)

x.tree <- tree(class~.,data=x)
summary(x.tree)

plot(x.tree)
text(x.tree)
###
x.rf <- randomForest(class ~ ., 
                     data=x, ntree=100)
plot(x.rf)
varImpPlot(x.rf)

###
# https://datascienceplus.com/random-forests-in-r/
# train data
train <- sample(1:nrow(x),5000)
x.rf <- randomForest(class~.,data=x,subset=train,ntree=50)
x.rf
plot(x.rf)
#


x.cor <- cor(x)
corplot(x)