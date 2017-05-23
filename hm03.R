rm(list=ls())
library(ISLR)
data <- Weekly

##
names(data)
dim(data)
str(data)
summary(data)
attach(data)
par(mfrow=c(2,2))
plot(Lag1,type="l",col="red",main = "lag1 plot")
plot(Lag2,type="l",col="orange",main = "lag2 plot")
plot(Lag3,type="l",col="yellow",main = "lag3 plot")
plot(Lag4,type="l",col="blue",main = "lag4 plot")
par(mfrow=c(2,2))
plot(Lag5,type="l",col="green",main = "lag5 plot")
plot(Volume,type="l",col="yellow",main = " volume plot")
plot(Today,type="l",col="blue",main = "today's returns plot")

par(mfrow=c(1,1))
boxplot(data[,2:8],main="Boxplots of the variables")

par(mfrow=c(1,1))
pairs(data)
##logistic regression ##

lm1<- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,data = data)
summary(lm1)

probs <- predict(lm1,type="response")
pred <- rep("Down",length(Lag1))
pred[probs >= 0.5]="Up"

cm<- table(pred,Direction)
accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
errorrate <- (1-accuracy)*100
sens <- mean(pred[Direction=="Up"]=="Up")
spec <- 1-mean(pred[Direction=="Down"]=="Up")

train <- (Year<2009)
test <- data[!train,]
Direction.test <- Direction[!train]
lm2 <- glm(Direction ~ Lag2,family = binomial,subset = train)
summary(lm2)

prob1 <- predict(lm2,type = "response", newdata = data.frame(test))
pred1 <- rep("Down",nrow(test))
pred1[prob1 >= 0.5]="Up"

cm1<- table(pred1,Direction.test)
accuracy <- (cm1[1,1]+cm1[2,2])/sum(cm1)
errorrate <- (1-accuracy)*100
sens <- mean(pred1[Direction.test=="Up"]=="Up")
spec <- 1-mean(pred1[Direction.test=="Down"]=="Up")

##LDA##
library(MASS)
lda.fit <- lda(Direction ~ Lag2,data = data,subset = train)
lda.pred <- predict(lda.fit,test)

cm.lda<- table(lda.pred$class,Direction.test)
accuracy <- (cm.lda[1,1]+cm.lda[2,2])/sum(cm.lda)
errorrate <- (1-accuracy)*100
sens <- mean(lda.pred$class[Direction.test=="Up"]=="Up")
spec <- 1-mean(lda.pred$class[Direction.test=="Down"]=="Up")

##QDA##

qda.fit <- qda(Direction ~ Lag2,data = data,subset = train)
qda.pred <- predict(qda.fit,test)

cm.qda<- table(qda.pred$class,Direction.test)
accuracy <- (cm.qda[1,1]+cm.qda[2,2])/sum(cm.qda)
errorrate <- (1-accuracy)*100
sens <- mean(qda.pred$class[Direction.test=="Up"]=="Up")
spec <- 1-mean(qda.pred$class[Direction.test=="Down"]=="Up")

##KNN##
library(class)
train.X <-Lag2[train]
test.X <- Lag2[!train]
Direction.train <- Direction[train]
set.seed(1)
knn.pred <- knn(data.frame(train.X),data.frame(test.X),Direction.train,k=1)
cm.knn <- table(knn.pred,Direction.test)
accuracy <- (cm.knn[1,1]+cm.knn[2,2])/sum(cm.knn)
errorrate <- (1-accuracy)*100
sens <- mean(knn.pred[Direction.test=="Up"]=="Up")
spec <- 1-mean(knn.pred[Direction.test=="Down"]=="Up")

## Experiment wit different combinations of predictors ##
lm3 <- glm(Direction ~ Lag2+Lag2*Lag1+Lag2*Lag3+Lag2*Lag4+Lag2*Lag5+log(Volume),family = binomial,subset = train)
summary(lm3)

prob1 <- predict(lm3,type = "response", newdata = data.frame(test))
pred1 <- rep("Down",nrow(test))
pred1[prob1 >= 0.5]="Up"

cm1<- table(pred1,Direction.test)
accuracy <- (cm1[1,1]+cm1[2,2])/sum(cm1)
errorrate <- (1-accuracy)*100
sens <- mean(pred1[Direction.test=="Up"]=="Up")
spec <- 1-mean(pred1[Direction.test=="Down"]=="Up")


###LDA###
library(MASS)
lda1.fit <- lda(Direction ~ Lag2+Lag2*Lag1+Lag2*Lag3+Lag2*Lag4+Lag2*Lag5+log(Volume),data = data,subset = train)
lda.pred <- predict(lda1.fit,test)

cm.lda<- table(lda.pred$class,Direction.test)
accuracy <- (cm.lda[1,1]+cm.lda[2,2])/sum(cm.lda)
errorrate <- (1-accuracy)*100
sens <- mean(lda.pred$class[Direction.test=="Up"]=="Up")
spec <- 1-mean(lda.pred$class[Direction.test=="Down"]=="Up")

##QDA##
qda1.fit <- qda(Direction ~ Lag2+Lag2*Lag1+Lag2*Lag3+Lag2*Lag4+Lag2*Lag5+log(Volume),data = data,subset = train)
qda.pred <- predict(qda1.fit,test)

cm.qda<- table(qda.pred$class,Direction.test)
accuracy <- (cm.qda[1,1]+cm.qda[2,2])/sum(cm.qda)
errorrate <- (1-accuracy)*100
sens <- mean(qda.pred$class[Direction.test=="Up"]=="Up")
spec <- 1-mean(qda.pred$class[Direction.test=="Down"]=="Up")

##KNN##

library(class)
train.X <-Lag2[train]
test.X <- Lag2[!train]
Direction.train <- Direction[train]
accuracy<- rep(0,100)

for(i in 1:100){
  set.seed(1)
  knn.pred <- knn(data.frame(train.X),data.frame(test.X),Direction.train,k=i)
  cm.knn <- table(knn.pred,Direction.test)
  accuracy[i] <- (cm.knn[1,1]+cm.knn[2,2])/sum(cm.knn)
}
plot(1:100,accuracy,xlab="K value",ylab="Accuracy",type="l",ylim=c(.45,.65))
which.max(accuracy)

### problem-2 ####
###ROC analysis ###

###ROC logistic reg###

lm2 <- glm(Direction ~ Lag2,family = binomial,data = data)
lr.pred <- predict(lm2,type="response")
roc.curve=function(s,print=FALSE){
  Ps=(l.pred>s)*1
  FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
  TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
  if(print==TRUE){
    print(table(Observed=Direction,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.5
roc.curve(threshold,print=TRUE)
## Plot ROC curve
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="green",lwd=2,type="l",xlab="Falsepositive rate",ylab="True positive rate",main="ROC for logistic reg.")



### ROC LDA ###

lda.fit <- lda(Direction ~ Lag2,data = data)
lda.pred0 <- predict(lda.fit,type="response")
lda.pred <- lda.pred0$posterior[,2]
roc.curve1=function(s,print=FALSE){
  Ps=(lda.pred >s)*1
  FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
  TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
  if(print==TRUE){
    print(table(Observed=Direction,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.5
roc.curve1(threshold,print=TRUE)
## Plot ROC curve
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l",xlab="Falsepositive rate",ylab="True positive rate",main="ROC for LDA")


detach(data)



rm(list=ls())
##problem-3###
library(ISLR)
attach(Auto)
mpg01 <- rep(0,length(mpg))
mpg01[mpg > median(mpg)]= 1
data1 <- data.frame(cbind(Auto,(mpg01)))
colnames(data1)[10] <- "mpg01"

pairs(data1)
par(mfrow=c(2,2))
boxplot(data1$displacement~data1$mpg01,xlab="MPG",ylab="displacement")
boxplot(data1$horsepower~data1$mpg01,xlab="MPG",ylab="Horsepower")
boxplot(data1$weight~data1$mpg01,xlab="MPG",ylab="weight")
boxplot(data1$acceleration~data1$mpg01,xlab="MPG",ylab="acceleration")

train <- (Auto$year < 80)
train.data <- data1[train,]
test.data <- data1[!train,]

##LDA##
library(MASS)
lda.fit <- lda(mpg01 ~ displacement+horsepower+weight+acceleration,data = train.data)
lda.pred <- predict(lda.fit,test.data)

cm.lda<- table(lda.pred$class,test.data$mpg01)
accuracy <- (cm.lda[1,1]+cm.lda[2,2])/sum(cm.lda)
errorrate <- (1-accuracy)*100
sens <- mean(lda.pred$class[test.data$mpg01==1]==1)
spec <- 1-mean(lda.pred$class[test.data$mpg01==0]==1)

##QDA##
qda.fit <- qda(mpg01 ~ displacement+horsepower+weight+acceleration,data = train.data)
qda.pred <- predict(qda.fit,test.data)

cm.qda<- table(qda.pred$class,test.data$mpg01)
accuracy <- (cm.qda[1,1]+cm.qda[2,2])/sum(cm.qda)
errorrate <- (1-accuracy)*100
sens <- mean(qda.pred$class[test.data$mpg01==1]==1)
spec <- 1-mean(qda.pred$class[test.data$mpg01==0]==1)

##logistic regression##

lm.fit <- glm(mpg01 ~ displacement+horsepower+weight+acceleration,family = binomial,data = train.data)
summary(lm.fit)

probs <- predict(lm.fit,type="response",newdata = data.frame(test.data))
pred <- rep(0,length(test.data$mpg01))
pred[probs >= 0.5]= 1

cm<- table(pred,test.data$mpg01)
accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
errorrate <- (1-accuracy)*100
sens <- mean(pred[test.data$mpg01==1]==1)
spec <- 1-mean(pred[test.data$mpg01==0]==1)

##KNN##

library(class)
trainclass <- train.data$mpg01
train_data <- cbind(train.data$displacement,train.data$horsepower,train.data$weight,train.data$acceleration)
test_data  <- cbind(test.data$displacement,test.data$horsepower,test.data$weight,test.data$acceleration)

accuracy <- rep(0,100)

for(i in 1:100){
set.seed(1)
knn.pred <- knn(data.frame(train_data),data.frame(test_data),trainclass,k=i)
cm.knn <- table(knn.pred,test.data$mpg01)
accuracy[i] <- (cm.knn[1,1]+cm.knn[2,2])/sum(cm.knn)
}
par(mfrow=c(1,1))
plot(1:100,accuracy,xlab="K value",ylab="Accuracy",type="l",ylim=c(.72,.85))
which.max(accuracy)

