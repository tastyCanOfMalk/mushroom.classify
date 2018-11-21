
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

# check unique levels of variables
x.levels <- cbind(colnames(x),
                  (as.data.frame(sapply(x,function(x) length(unique(x)))))
                  )
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]
# should probably drop veil.type

# # convert class to poisonous=F/T
# x <- x %>% 
#   mutate(Poisonous = as.factor(ifelse(class=="e","No","Yes"))) %>% 
#   select(-class)
# glimpse(x)

# tree1
x.tree1 <- tree(class~.,data=x)
summary(x.tree1)
plot(x.tree1)
text(x.tree1)

# tree2
train = sample(1:nrow(x),nrow(x)/2)
x.tree2 <-tree(class~.,data=x,subset=train) 
summary(x.tree2)
plot(x.tree2)
text(x.tree2,cex=.7)

# see if pruning will improve performance
cv.x <- cv.tree(x.tree2)
plot(cv.x$size,cv.x$dev,type='b')

# actually pruning
prune.x <- prune.tree(x.tree2,best=4)
plot(prune.x)
text(prune.x,pretty=0)

# use un-pruned tree to test actual dataset
yhat <- predict(x.tree2,newdata=x[-train,])
x.test <- x[-train,"class"]
plot(yhat,x.test)
abline(0,1)
mean((yhat-x.test)^2)
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