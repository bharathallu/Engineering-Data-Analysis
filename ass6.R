rm(list=ls())
##
library(ISLR)
library(randomForest)
library(MASS)
attach(Boston)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test=Boston[-train,"medv"]

##calculate test MSE
set.seed(1)
mse <- rep(0,13)

for(i in 1:13){
 bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=i,ntree=100, importance=TRUE)
 yhat.bag=predict(bag.boston,newdata=Boston[-train ,])
 mse[i] <- mean((yhat.bag-boston.test)^2)
}

## plot of test error against tree depth
par(mfrow=c(1,1))
plot(mse,type="b",col="red",xlab='mtry',main='MSE vs tree depth')
identify(mse,label=1:13)

## changing number of trees

trees <- seq(5,200,by=10)
mse <- rep(0,length(trees))
set.seed(1)
for(i in 1:length(trees)){
  bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=6,ntree=trees[i], importance=TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train ,])
  mse[i] <- mean((yhat.bag-boston.test)^2)
}
plot(trees,mse,type="b",col="blue",xlab='ntrees',main='MSE vs no. of trees')
identify(trees,mse,label=trees)

## problem1
rm(list=ls())

library(tree)
library(ISLR)
attach(Carseats)

set.seed(1)

train=sample(1: nrow(Carseats), 200)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]
sales.train=Sales[train]
sales.test=Sales[-train]

tree.carseats=tree(Sales~.,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0,cex=.7)

tree.pred=predict(tree.carseats,Carseats.test)
mse.test <- mean((sales.test-tree.pred)^2)

## tree pruning
set.seed(1)
cv.prune <- cv.tree(tree.carseats)
cv.prune$size[which.min(cv.prune$dev)]
cv.prune$dev
cv.prune$method

prune.sales <- prune.tree(tree.carseats,best = cv.prune$size[which.min(cv.prune$dev)])
plot(prune.sales)
text(prune.sales,pretty=0,cex=.7)

pred.prune.sales <- predict(prune.sales,Carseats.test)
mse.prune <- mean((sales.test- pred.prune.sales)^2)


##bagging

set.seed(1)
bag.sales <- randomForest(Sales ~.,data=Carseats,subset = train,mtry=10,ntrees=100,importance=T)
pred.bag.sales <- predict(bag.sales,Carseats.test)
mse.bag <- mean((sales.test - pred.bag.sales)^2)
imp <- order(bag.sales$importance[,2],decreasing = T)
names(Carseats.train[,imp+1])


## random forests
set.seed(1)
rf.sales <- randomForest(Sales ~.,data=Carseats,subset = train,mtry=4,ntrees=100,importance=T)
pred.sales <- predict(rf.sales,Carseats.test)
mse.rf <- mean((sales.test - pred.sales)^2)
imp <- order(rf.sales$importance[,2],decreasing = T)
names(Carseats.train[,imp+1])
