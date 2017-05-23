rm(list=ls())
library(ISLR)
head(Default)
str(Default)
### fitting a logistic model usinf full data ###
data <- Default[,-2]                  
attach(data)
mod1 <- glm( default ~ income + balance, family = binomial)
summary(mod1)


### splitting the set into 80:20 ##
set.seed(45)
data.train <- data[sample(nrow(data),8000),]
data.test <- data[-sample(nrow(data),8000),]

mod2 <- glm( default ~ income + balance, family = binomial,data = data.train)
summary(mod2)
prob <- predict.glm(mod2,data.test,type="response")
pred <- rep(0,length(data.test$default))
pred[prob >= 0.5] = 1
cm <- table(data.test$default,pred)
er <- (cm[1,2]+cm[2,1])/length(data.test$default)

### three different splits 25,50,75##
set.seed(45)
data.train1 <- data[sample(nrow(data),0.25*nrow(data)),]
data.test1 <- data[-sample(nrow(data),0.25*nrow(data)),]
mod3 <- glm( default ~ income + balance, family = binomial,data = data.train1)
summary(mod3)
prob <- predict.glm(mod3,data.test1,type="response")
pred<- rep(0,length(data.test1$default))
pred[prob >= 0.5] = 1
cm <- table(data.test1$default,pred)
er <- (cm[1,2]+cm[2,1])/length(data.test1$default)
er

set.seed(45)
data.train2 <- data[sample(nrow(data),0.50*nrow(data)),]
data.test2 <- data[-sample(nrow(data),0.50*nrow(data)),]
mod4 <- glm( default ~ income + balance, family = binomial,data = data.train2)
summary(mod4)
prob <- predict.glm(mod4,data.test2,type="response")
pred<- rep(0,length(data.test2$default))
pred[prob >= 0.5] = 1
cm <- table(data.test2$default,pred)
er <- (cm[1,2]+cm[2,1])/length(data.test2$default)
er

set.seed(45)
data.train3 <- data[sample(nrow(data),0.75*nrow(data)),]
data.test3 <- data[-sample(nrow(data),0.75*nrow(data)),]
mod5 <- glm(default ~ income + balance, family = binomial,data = data.train3)
summary(mod5)
prob <- predict.glm(mod5,data.test3,type="response")
pred<- rep(0,length(data.test3$default))
pred[prob >= 0.5] = 1
cm <- table(data.test3$default,pred)
er <- (cm[1,2]+cm[2,1])/length(data.test3$default)
er

set.seed(45)
data <- Default
data.train <- data[sample(nrow(data),8000),]
data.test <- data[-sample(nrow(data),8000),]
mod6 <- glm( default ~ income + balance + student, family = binomial,data = data.train)
summary(mod6)
prob <- predict.glm(mod6,data.test,type="response")
pred <- rep(0,length(data.test$default))
pred[prob >= 0.5] = 1
cm <- table(data.test$default,pred)
er <- (cm[1,2]+cm[2,1])/length(data.test$default)

### problem - 2 ###
rm(list=ls())
library(boot)
set.seed(1)
x=rnorm(200)
y=x-2*x^2+rnorm(200)

par(mfrow=c(1,1))
plot(x,y) ## Hyperbolic function ##
data <- data.frame(cbind(x,y))

cv.error = rep(0,4)
for (i in 1:4) {
  glm.fit = glm(y~poly(x,i),data=data)
  cv.error[i] = cv.glm(data,glm.fit)$delta[1]
}
cv.error



### repeat using a different seed ###
rm(list=ls())
set.seed(100)
x=rnorm(200)
y=x-2*x^2+rnorm(200)


par(mfrow=c(1,1))
plot(x,y) ## Hyperbolic function ##
data <- data.frame(cbind(x,y))

cv.error = rep(0,4)
for (i in 1:4) {
  glm.fit = glm(y~poly(x,i),data=data)
  cv.error[i] = cv.glm(data,glm.fit)$delta[1]
}
cv.error


### 5 fold CV ###

cv.error = rep(0,4)
for (i in 1:4) {
  glm.fit = glm(y~poly(x,i),data=data)
  cv.error[i] = cv.glm(data,glm.fit,K=5)$delta[1]
}
cv.error

### 10 fold CV ###

cv.error = rep(0,4)
for (i in 1:4) {
  glm.fit = glm(y~poly(x,i),data=data)
  cv.error[i] = cv.glm(data,glm.fit,K=10)$delta[1]
}
cv.error
