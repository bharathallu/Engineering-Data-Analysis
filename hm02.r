rm(list=ls())
library(ISLR)
library(car)
attach(Auto)
lm1 <- lm(mpg ~ horsepower)
lm11 <- lm(mpg ~ log(horsepower))
lm12 <- lm(mpg ~ sqrt(horsepower))
lm13<- lm(mpg ~ (horsepower)^2)
summary(lm1)
summary(lm11) 
summary(lm12) 
summary(lm13) 

cor(mpg,horsepower)

predict(lm1,data.frame(horsepower=98)) 
predict(lm1,data.frame(horsepower=98),interval = "confidence") 
predict(lm1,data.frame(horsepower=98),interval="prediction") 

par(mfrow=c(1,1))
plot(horsepower,mpg,col="blue",main="mpg vs horsepower") 
abline(lm(mpg ~ horsepower)) 

par(mfrow=c(2,2))
plot(lm1)
## 1-d ##

par(mfrow=c(2,2))
plot(horsepower,type="l")
plot(log(horsepower),type="l",main = "log tranformation")
plot(sqrt(horsepower),type="l",main = "sqrt transformation")
plot((horsepower)^2,type="l",main="square transformation")

## problem-2 ##

pairs(Auto[,1:8])
pairs(Auto[,1:9])
corr<- data.frame(cor(Auto[,1:8]))

lm2 <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+origin)
summary(lm2)

lm_test <- lm(mpg ~ weight+acceleration+year+origin)
summary(lm_test)
vif(lm_test)


par(mfrow=c(2,2))
plot(lm2)
vif(lm2)

lm_int<- lm(mpg~ displacement*acceleration*year*origin)
summary(lm_int)

lm_int1 <- lm(mpg ~ displacement*weight*year*origin)
summary(lm_int1)

center_data <- scale(data.frame(Auto[,1:8],center = T,scale = F))
lm_inter <- lm(mpg ~ displacement*weight*year*origin*acceleration*horsepower*cylinders)
summary(lm_inter)

lm_inter <- lm(center_data[,1] ~ center_data[,2]*center_data[,3]*center_data[,4]*center_data[,5]*center_data[,6]*center_data[,7]*center_data[,8])
summary(lm_inter)
lm_test1 <- lm(mpg ~ year*origin*horsepower*cylinders)
summary(lm_test1)

detach(Auto)
## do interactions

## problem -3 ##
attach(Carseats)
lm3 <- lm( Sales ~ Price + Urban + US)
summary(lm3)

lm4 <- lm( Sales ~ Price + US)
summary(lm4)

par(mfrow=c(2,2))
plot(lm4)

s<- summary(lm4)$sigma
hii <- hatvalues(lm4)
z.resids <- lm4$residuals/(s*sqrt(1-hii))
par(mfrow=c(1,1))
plot(Price,z.resids)
detach(Carseats)
