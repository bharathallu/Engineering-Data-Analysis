library(car)

rm(list=ls())
data <- read.csv("C:/ISEN 613/Midterm-I/Training.csv",header = T,sep = ",")
attach(data)

### preliminary check ###
dim(data)
str(data)
summary(data)
pairs(data)
par(mfrow=c(2,3))
plot(casual,type="l")
plot(temp,type="l")
plot(atemp,type ="l")
plot(hum,type="l")
plot(windspeed,type="l")
par(mfrow=c(2,4))
boxplot(casual~season,xlab="Season",main="Casual ~ season")
boxplot(casual~weekday,xlab="weekday",main="Casual ~ weekday")
boxplot(casual~year,xlab="year",main="casual~ year")
boxplot(casual~holiday,xlab="holiday",main="casual ~ holiday")
boxplot(casual~weathersit,xlab="weathersit",main="casual ~ weathersit")
plot(temp,casual,col="blue",xlab="temp",main="casual vs temp")
abline(lm(casual ~ temp))
plot(windspeed,casual,col="red",xlab="windspeed",main="casual vs windspeed")
abline(lm(casual ~ windspeed))
plot(hum,casual,col="green",xlab="humidity",main="casual vs hum")
abline(lm(casual ~ hum))

rho <- data.frame(cor(data))
### regression using all the variables ###
lm <- lm(casual ~ .,data = data)
summary(lm)
vif(lm)
par(mfrow=c(2,2))
plot(lm)

## new regression ##
lm1 <- lm(log(casual) ~ year+weekday+I(temp^.33)+weathersit+sqrt(windspeed)+weekday:temp,data=data)
summary(lm1)
vif(lm1)
par(mfrow=c(2,2))
plot(lm1)

lm2 <- lm(log(casual) ~ year+season+I(temp^.33)+weathersit+sqrt(windspeed),data=data)
summary(lm2)
vif(lm2)
par(mfrow=c(2,2))
plot(lm2)

lm3 <- lm(log(casual) ~ year+month+scale(temp,center = T,scale = T)+sqrt(windspeed)+weathersit+month:scale(temp,center = T,scale = T),data=data)
summary(lm3)
plot(lm3)
vif(lm3)
## predict for test data ###
data.test <- read.csv("C:/ISEN 613/Midterm-I/Test.csv",header = T,sep = ",")
lm1.predict <- predict(lm1,newdata = data.test,type = "response")
casual.pred1 <- exp(lm1.predict)

lm2.predict <- predict(lm2,newdata = data.test,type = "response")
casual.pred2 <- exp(lm2.predict)

lm3.predict <- predict(lm3,newdata = data.test,type = "response")

casual.pred4 <- exp(lm3.predict)


  ## root mean square error ##
rmse1 <- sqrt(sum((data.test$casual - casual.pred1)^2)/length(data.test$casual))

par(mfrow=c(1,1))
plot(data.test$casual,type="l",col="red")
lines(casual.pred,type="l",col="blue")

rmse2 <- sqrt(sum((data.test$casual- casual.pred2)^2)/length(data.test$casual))

par(mfrow=c(1,1))
plot(data.test$casual,type="l",col="red")
lines(casual.pred2,type="l",col="blue")

rmse4 <- sqrt(sum((data.test$casual- casual.pred4)^2)/length(data.test$casual))

##logarithimic error ##
lme1 <- sqrt(sum((log(data.test$casual+1)-log(casual.pred+1))^2)/20)
lme2 <- sqrt(sum((log(data.test$casual+1)-log(casual.pred2+1))^2)/20)

### models ####
lm2 <- lm(log(casual) ~ year+holiday+weathersit+windspeed+temp,data=data)
lm1 <- lm(log(casual) ~ year+season+temp+weathersit+windspeed+windspeed:season,data=data)
lm1 <- lm(log(casual) ~ year+season+temp+weathersit+I(windspeed^2),data=data)
lm1 <- lm(log(casual) ~ year+season+I(temp^2)+weathersit+I(windspeed^2),data=data)
lm2 <- lm(log(casual) ~ year+month+weathersit+log(temp)+log(windspeed)+year:month,data=data)
par(mfrow=c(1,1))
plot(casual[season==1],type = 'l')

boxplot(casual~month)
boxplot(weekday~holiday)
par(mfrow=c(1,1))
plot(temp,casual)
plot(windspeed,casual)
plot(hum,casual)


## predict with first model ##
predict.lm <- predict(lm,newdata = data.test,type="response")
casual.pred3 <- predict.lm
rmse <- sqrt(sum((data.test$casual - casual.pred3)^2)/length(data.test$casual))

summary(powerTransform(cbind(data$temp,data$windspeed)~1,))
par(mfrow=c(1,1))
plot(log(scale(casual,center = T,scale=T)),type="l")

