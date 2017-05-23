rm(list=ls())
library(ISLR)
data <- College
str(data)

## best subset selection ##
sum(is.na(data))
library(leaps)
attach(data)
mod <- regsubsets(Apps ~ ., data,nvmax = 17)
mod.summary <- summary(mod)
mod.summary
par(mfrow=c(2,2))
plot(mod.summary$rss,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)

plot(mod.summary$adjr2,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.max(mod.summary$adjr2),max(mod.summary$adjr2),cex=2,pch=20)

plot(mod.summary$cp,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(mod.summary$cp),min(mod.summary$cp),cex=2,pch=20)

plot(mod.summary$bic,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(mod.summary$bic),min(mod.summary$bic),cex=2,pch=20)

coef(mod,13)
coef(mod,12)
coef(mod,10)

coef(mod,10)[1,]

mod.fwd <- regsubsets(Apps ~ ., data,nvmax =17,method="forward")
mod.fwd.summary <- summary(mod.fwd)
mod.fwd.summary

par(mfrow=c(2,2))
plot(mod.fwd.summary$rss,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)

plot(mod.fwd.summary$adjr2,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.max(mod.fwd.summary$adjr2),max(mod.fwd.summary$adjr2),cex=2,pch=20)

plot(mod.fwd.summary$cp,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(mod.fwd.summary$cp),min(mod.fwd.summary$cp),cex=2,pch=20)

plot(mod.fwd.summary$bic,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(mod.fwd.summary$bic),min(mod.fwd.summary$bic),cex=2,pch=20)

mtext("Forward Selection", side = 3, line = -1, outer = TRUE)

coef(mod,13)
coef(mod,12)
coef(mod,10)

mod.bwd=regsubsets(Apps~.,data,nvmax=17, method="backward")
mod.bwd.summary <- summary(mod.bwd)
mod.bwd.summary

par(mfrow=c(2,2))
plot(mod.bwd.summary$rss,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)

plot(mod.bwd.summary$adjr2,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.max(mod.bwd.summary$adjr2),max(mod.bwd.summary$adjr2),cex=2,pch=20)

plot(mod.bwd.summary$cp,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(mod.bwd.summary$cp),min(mod.bwd.summary$cp),cex=2,pch=20)

plot(mod.bwd.summary$bic,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(mod.bwd.summary$bic),min(mod.bwd.summary$bic),cex=2,pch=20)

mtext("Backward Selection", side = 3, line = -1, outer = TRUE)

coef(mod,13)
coef(mod,12)
coef(mod,10)

### Fitting Lasso###

rm(list=ls())
data <- College
library(glmnet)
x <- model.matrix(Apps~.,data)[,-1]
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
Apps.test <- Apps[test]



### c part ###

val.lambda <- 10^seq(10,-2,length=100)
mod.lasso <- cv.glmnet(x,Apps,alpha=1)
mod.lasso$lambda.min
par(mfrow=c(1,1))
plot(mod.lasso)
plot(mod.lasso$lambda,mod.lasso$cvm,xlab="Lambda",ylab="CV Error")
lasso.mod.coeff <- predict(mod.lasso,s=mod.lasso$lambda.min,type="coefficients")[1:18,]
lasso.mod.coeff

### d part ###
mod.ridge <- cv.glmnet(x,Apps,alpha=0)
mod.ridge$lambda.min
plot(mod.ridge)
plot(mod.ridge$lambda,mod.ridge$cvm,xlab="lambda",ylab="CV error")
ridge.mod.coeff <- predict(mod.ridge,s=mod.ridge$lambda.min,type="coefficients")[1:18,]
ridge.mod.coeff


### e part for lasso ###
rm(list=ls())
data <- College
library(glmnet)
x <- model.matrix(Apps~.,data)[,-1]
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
Apps.test <- Apps[test]
val.lambda <- 10^seq(10,-2,length=100)


set.seed(1)
lasso.mod <- glmnet(x[train,],Apps[train],alpha=1,lambda = val.lambda)
plot(lasso.mod)
par(mfrow=c(1,1))
cv.lasso=cv.glmnet(x[train,],Apps[train],alpha=1)
plot(cv.lasso)
bestlam=cv.lasso$lambda.min

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-Apps[test])^2)

out=glmnet(x,Apps,alpha=1,lambda= val.lambda)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:18,]

lasso.coef
lasso.coef[lasso.coef!=0]

## e part for ridge##
rm(list=ls())
data <- College
library(glmnet)
x <- model.matrix(Apps~.,data)[,-1]
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
Apps.test <- Apps[test]
val.lambda <- 10^seq(10,-2,length=100)



ridge.mod <- glmnet(x[train,],Apps[train],alpha=0,lambda = val.lambda)
plot(ridge.mod)

set.seed(1)
cv.ridge=cv.glmnet(x[train,],Apps[train],alpha=0)
plot(cv.ridge)
bestlam=cv.ridge$lambda.min
  
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-Apps[test])^2)

out=glmnet(x,Apps,alpha=0,lambda= val.lambda)
ridge.coef=predict(out,type="coefficients",s=bestlam)[1:18,]

ridge.coef
ridge.coef[ridge.coef!=0]

####

rm(list=ls())
library(ISLR)
data <- College
library(glmnet)
set.seed(1)
train <- sample(1:nrow(data), nrow(data)/2)
test <- (-train)
attach(data)
data.test <- data[-train,]
data.train <- data[train,]

regfit.best=regsubsets(Apps~.,data[train,],nvmax=17)
regfit.best.summary <- summary(regfit.best)

par(mfrow=c(2,2))
plot(regfit.best.summary$rss,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)

plot(regfit.best.summary$adjr2,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.max(regfit.best.summary$adjr2),max(regfit.best.summary$adjr2),cex=2,pch=20)

plot(regfit.best.summary$cp,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(regfit.best.summary$cp),min(regfit.best.summary$cp),cex=2,pch=20)

plot(regfit.best.summary$bic,type="l",xaxt="n")
axis(1,at=seq(1,18,1),las=1)
points(which.min(regfit.best.summary$bic),min(regfit.best.summary$bic),cex=2,pch=20)

### predicting test data using adjR2###
data.train = data[train,]
lm.r2 <- lm(Apps ~ Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+PhD+Expend+Grad.Rate,data.train)
pred.r2 <- predict(lm.r2,data.test,type="response")
error1 <- mean((Apps[test]-pred.r2)^2)

#### prediction using cp

lm.cp <- lm(Apps ~ Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+PhD+Expend+Grad.Rate,data.train)
pred.cp <- predict(lm.cp,data.test,type="response")
error2 <- mean((Apps[test]-pred.cp)^2)

### prediction using BIC

lm.bic <- lm(Apps ~ Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board,data.train)
pred.bic <- predict(lm.bic,data.test,type="response")
error3 <- mean((Apps[test]-pred.bic)^2)

