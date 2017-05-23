rm(list=ls())
set.seed(1000)
vec<- rnorm(25)
mat<- matrix(vec,nrow=5)
mat
mat1<- matrix(vec,ncol=5,byrow=T)
mat1
set.seed(1000)
vec2<- rnorm(100)
hist(vec2,main="Histogram of vector",col=3,xlab="Random Number")

# Question 2

library(ISLR)

names(Auto)
dim(Auto)
summary(Auto)
pairs(Auto)
attach(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration,data=Auto)
plot(horsepower,mpg,col="red")
abline(39.9539,-0.1578)
abline(lm(mpg~horsepower))
