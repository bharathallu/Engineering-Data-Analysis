rm(list=ls())
library(ISLR)
library(e1071)
attach(Auto)


###creating a binary variable
median(mpg)
mileage <- rep(0,length(mpg))
mileage[mpg >= median(mpg)]=1
mileage <- as.factor(mileage)

dat <- data.frame(cbind(Auto[,2:8],mileage))

###sv classifier to data with various costs
set.seed(123)
tune.out=tune(svm,mileage ~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

### radial kernel
set.seed(1)
tune.radial.out=tune(svm,mileage~.,data=dat,kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.radial.out)
bestmod.rad <- tune.radial.out$best.model
summary(bestmod.rad)

### polynomial kernel

set.seed(1)
tune.poly.out=tune(svm,mileage~.,data=dat,kernel="polynomial",ranges=list(cost=c(0.1,1,10,100,1000),degree=c(2,3,4,5)))
summary(tune.poly.out)
bestmod.poly <- tune.poly.out$best.model
summary(bestmod.poly)


####problem 2
rm(list=ls())
library(ISLR)
library(e1071)
attach(OJ)


set.seed(1)
train <- sample(1:length(Purchase),800)
data.train <- OJ[train,]
data.test <- OJ[-train,]

set.seed(1)
svmfit=svm(Purchase~.,data=data.train,kernel="linear",cost=0.01,scale=FALSE)
s <- summary(svmfit)
s
tab.train <- table(data.train$Purchase,svmfit$fitted)
err.train <- (tab.train[1,2]+tab.train[2,1])/sum(tab.train)
svm.pred <- predict(svmfit,data.test,type="class")
tab.test <- table(data.test$Purchase,svm.pred)
err.test <- (tab.test[1,2]+tab.test[2,1])/sum(tab.test)


set.seed(5)
tune.out <- tune(svm,Purchase~.,data=data.train,kernel='linear',ranges=list(cost=c(.01,.1,.5,.10,.50,1,5,10)))
summary(tune.out)
bestmod1 <- tune.out$best.model
summary(bestmod1)
tab.train <- table(data.train$Purchase,bestmod1$fitted)
err.train <- (tab.train[1,2]+tab.train[2,1])/sum(tab.train)


svmfit1 <- svm(Purchase~.,data=data.train,kernel="linear",cost=5,scale=FALSE)
svm.pred1 <- predict(svmfit1,data.test,type="class")
tab.test <- table(data.test$Purchase,svm.pred1)
err.test <- (tab.test[1,2]+tab.test[2,1])/sum(tab.test)


### fitting and tuning a radial kernel
set.seed(1)
svm.rad.fit <- svm(Purchase~.,data = data.train,kernel='radial',gamma=1,cost=1)
summary(svm.rad.fit)
tab.train <- table(data.train$Purchase,svm.rad.fit$fitted)
err.train <- (tab.train[1,2]+tab.train[2,1])/sum(tab.train)
err.train
svm.rad.pred <- predict(svm.rad.fit,data.test,type='class')
tab.test <- table(data.test$Purchase,svm.rad.pred )
err.test <- (tab.test[1,2]+tab.test[2,1])/sum(tab.test)
err.test 

set.seed(1)
svm.tun.rad <- tune(svm,Purchase~.,data=data.train,kernel='radial',ranges=list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3)))
bestmod.rad <- svm.tun.rad$best.model
summary(bestmod.rad)
tab.train <- table(data.train$Purchase,bestmod.rad$fitted)
err.train <- (tab.train[1,2]+tab.train[2,1])/sum(tab.train)
err.train
svm.pred.radb <- predict(svm.tun.rad$best.model,data.test,type='class')
tab.test <- table(data.test$Purchase,svm.pred.radb)
err.test <- (tab.test[1,2]+tab.test[2,1])/sum(tab.test)
err.test 

### fitting and tuning a polynomial kernel
set.seed(1)
svm.poly.fit <- svm(Purchase~.,data = data.train,kernel='polynomial',degree=2,cost=1)
summary(svm.poly.fit)
tab.train <- table(data.train$Purchase,svm.poly.fit$fitted)
err.train <- (tab.train[1,2]+tab.train[2,1])/sum(tab.train)
err.train
svm.poly.pred <- predict(svm.poly.fit,data.test,type='class')
tab.test <- table(data.test$Purchase,svm.poly.pred)
err.test <- (tab.test[1,2]+tab.test[2,1])/sum(tab.test)
err.test

set.seed(1)
svm.tun.poly <- tune(svm,Purchase~.,data=data.train,kernel='polynomial',ranges=list(cost=c(0.1,1,10,100),degree=c(2,3,4,5)))
summary(svm.tun.poly)
bestmod.tun.poly <- svm.tun.poly$best.model
tab.train <- table(data.train$Purchase,bestmod.tun.poly$fitted)
err.train <- (tab.train[1,2]+tab.train[2,1])/sum(tab.train)
err.train
svm.pred.polyb <- predict(svm.tun.poly$best.model,data.test,type='class')
tab.test <- table(data.test$Purchase,svm.pred.polyb)
err.test <- (tab.test[1,2]+tab.test[2,1])/sum(tab.test)
err.test
