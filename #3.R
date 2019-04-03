#3 데이터 마이닝과제
#2-1
#a
library(ISLR)
data(Auto)
lm1 <- lm(mpg ~ horsepower, data=Auto)
summary(lm1)
new <- data.frame(horsepower = 98)
predict(lm1, new, interval="prediction")
#b
plot(Auto$horsepower, Auto$mpg, xlab="horsepower", ylab="mpg", main="Scatter plot of horsepower & mpg")
abline(lm1, col="red")
#c
par(mfrow=c(2,2))
plot(lm1)

#2.2
#a
pairs(Auto)
#b
data1 <- data.frame(Auto[-9])
print(cor(data1),digit=3)
#c
lm2 <- lm(mpg~., data=data1)
summary(lm2)
#d
par(mfrow=c(2,2))
plot(lm2)
#e
lm2.1 <- lm(mpg~displacement+horsepower+weight+year, data=data1)
lm2.2 <- lm(mpg~displacement+horsepower+weight*year, data=data1)
lm2.3 <- lm(mpg~displacement+horsepower*weight+year, data=data1)
summary(lm2.1)
summary(lm2.2)
summary(lm2.3)
#f
lm2.4 <- lm(mpg~displacement+horsepower+sqrt(weight)+year, data=data1)
lm2.5 <- lm(mpg~displacement+horsepower+weight+log(year), data=data1)
lm2.6 <- lm(mpg~displacement+poly(horsepower,2)+sqrt(weight)+year, data=data1)

#2.3
#a
set.seed(1)
x <- rnorm(100)
#b
eps <- rnorm(100, sd=0.25^0.5)
#c
y <- -1+0.5*x+eps
length(y)
#d
par(mfrow=c(1,1))
plot(x,y, main="scatterplot x & y", xlab="x", ylab="y")
#e
lm3 <- lm(y~x)
summary(lm3)
#f
plot(x,y,main="scatterplot x & y")
abline(-1,0.5,lty=2)
abline(lm3, lty=1)
legend(x=c(1,2.5),y=c(-2.5,-1.7),
       legend=c("population","model fit"),
       lty=c(2,1))
#g
lm3.1 <- lm(y~x+I(x^2))
summary(lm3.1)
anova(lm3, lm3.1)
#h
eps2 <- rnorm(100, sd=0.1)
y2 <- -1 + 0.5*x + eps2
lm3.2 <- lm(y2 ~ x)
summary(lm3.2)
#i
eps3 <- rnorm(100, sd=1)
y3 <- -1 + 0.5*x + eps3
lm3.3 <- lm(y3 ~ x)
summary(lm3.3)
#j
print(confint(lm3), digit=4)
print(confint(lm3.2), digit=4)
print(confint(lm3.3), digit=4)
