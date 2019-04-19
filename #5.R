#hw1 lab

library(ISLR)
set.seed(1)
train <- sample(392,196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)

glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

library(boot)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i),data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

alpha.fn <- function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace=T))
boot(Portfolio, alpha.fn, R=1000)

boot.fn <- function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)
boot.fn(Auto, sample(392,392,replace=T))
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef

boot.fn <- function(data, index){
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
}
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef

#hw2-1
#g
plot(1:10000,1-(1-1/1:10000)^(1:10000),type="l",main="The probability in the bootstrap sample",xlab="n", ylab="prob")
#h
store <- rep(NA,10000)
for(i in 1:10000){
  store[i] <- sum(sample(1:100, rep=TRUE)==4)>0
}
mean(store)

#hw2-2
#a
require(ISLR)
data(Default)
set.seed(1)
fit1 <- glm(default~income+balance, data=Default, family=binomial)
summary(fit1)
#b
train <- sample(1:nrow(Default), nrow(Default)*0.7)
fit2 <- glm(default~income+balance, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)
#d
fit3 <- glm(default~income+balance+student, data=Default, family=binomial, subset=train)
prob3 <- predict(fit3, Default[-train,], type="response")
pred3 <- ifelse(prob3 > 0.5, "Yes", "No")
table(pred3, Default[-train,]$default)
mean(Default[-train,]$default != pred3)

#hw2-3
#a
data(Weekly)
set.seed(1)
fit1 <- glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
summary(fit1)
#b
fit2 <- glm(Direction~Lag1+Lag2, data=Weekly[-1,], family=binomial)
summary(fit2)
#c
ifelse(predict(fit2, Weekly[1,], type="response")>0.5,"Up","Down")
Weekly[1,]
#d
loocv.err <-rep(0, nrow(Weekly))
for(i in 1:nrow(Weekly)){
  fit <- glm(Direction~Lag1+Lag2, data=Weekly[-i,], family=binomial)
  pred <- ifelse(predict(fit, Weekly[i,], type="response")>0.5, "Up", "Down")
  loocv.err[i] <- ifelse(Weekly[i,]$Direction==pred,0,1)
}
head(loocv.err)
#e
mean(loocv.err)

#hw2-4
#a
require(MASS)
require(boot)
data(Boston)
mean(Boston$medv)
#b
sd(Boston$medv)/sqrt(nrow(Boston))
#c
B <- 1000
n <- nrow(Boston)
boot.samples <- matrix(sample(Boston$medv, size=B*n, replace=TRUE), B, n)
boot.mean <- apply(boot.samples, 1, mean)
mean(boot.mean)
sd(boot.mean)
#d
mean(boot.mean)-2*sd(boot.mean)
mean(boot.mean)+2*sd(boot.mean)
t.test(Boston$medv)
#e
median(Boston$medv)
#f
boot.median <- apply(boot.samples, 1, median)
sd(boot.median)
#g
quantile(Boston$medv, 0.1)
#h
q10 <- function(var){
  quantile(var, 0.1)
}
boot.q10 <- apply(boot.samples, 1, q10)
sd(boot.q10)
