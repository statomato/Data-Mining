#hw1
#1 Lab: Introduction to R
#2.3.1
x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length (x)
length (y)
x + y
ls()
rm(x,y)
ls()
rm(list=ls())
?matrix
x = matrix (data=c(1,2,3,4) , nrow=2, ncol =2)
x
x=matrix (c(1,2,3,4) ,2,2)
matrix (c(1,2,3,4) ,2,2,byrow =TRUE)
sqrt(x)
x^2
x = rnorm (50)
y = x + rnorm (50, mean=50, sd=.1)
cor(x,y)
set.seed (1303)
rnorm (50)
set.seed (3)
y = rnorm (100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

#2.3.2
x = rnorm (100)
y = rnorm (100)
plot(x,y)
plot(x, y, xlab=" this is the x-axis", ylab=" this is the y-axis", main=" Plot of X vs Y")
pdf ("Figure .pdf")
plot(x, y, col ="green")
dev.off ()
x = seq (1 ,10)
x
x = 1:10
x
x = seq(-pi ,pi ,length =50)
y = x
f = outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)

#2.3.3
A = matrix (1:16 ,4 ,4)
A
A[2,3]
A[c(1,3) ,c(2,4)]
A[1:3 ,2:4]
A[1:2 ,]
A[ ,1:2]
A[1,]
A[-c(1,3) ,]
A[-c(1,3) ,-c(1,3,4)]
dim(A)

#2.3.4
Auto = read.table ("Auto.data ")
fix(Auto)
Auto = read.table ("Auto.data", header =T,na.strings ="?")
fix(Auto)
Auto = read.csv (" Auto.csv", header =T,na.strings ="?")
fix(Auto)
dim(Auto)
Auto [1:4 ,]
Auto = na.omit(Auto)
dim(Auto)
names(Auto)

#2.3.5
plot(cylinders , mpg)
plot(Auto$cylinders , Auto$mpg )
attach (Auto)
plot(cylinders , mpg)
cylinders = as.factor (cylinders )
plot(cylinders , mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ", ylab ="MPG ")
hist(mpg)
hist(mpg ,col =2)
hist(mpg ,col =2, breaks =15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)
plot(horsepower ,mpg)
identify (horsepower ,mpg ,name)
summary (Auto)
summary (mpg)

#hw2.1
#a
require(ISLR)
data(Auto)
str(Auto)
#b
range(Auto$mpg);range(Auto$cylinders);range(Auto$displacement);range(Auto$horsepower);range(Auto$weight);range(Auto$acceleration);range(Auto$year)

#c
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

#d
tmp <- Auto[,-(8:9)]   # drop origin, name
tmp <- tmp[-(10:85),]  # drop rows
sapply(tmp, range)

sapply(tmp, mean)
sapply(tmp, sd)

#e
pairs(Auto[,1:7])

#hw2.2
#a
require(ISLR)
require(MASS)
data("Boston")
str(Boston)

#b
pairs(Boston)

#c
require(ggplot2)
require(reshape2)

# plot each feature against crim rate
bosmelt <- melt(Boston, id="crim")
ggplot(bosmelt, aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") + 
  geom_point()

(corrmatrix <- cor(Boston)[1,])

#d
par(mfrow=c(1,3))
plot(Boston$crim,main="crim"); plot(Boston$tax,main="tax"); plot(Boston$ptratio,main="ptratio")

#e
table(Boston$chas)

#f
median(Boston$ptratio)

#g
(seltown <- Boston[Boston$medv == min(Boston$medv),])
sapply(Boston, quantile)

#h
nrow(Boston[Boston$rm > 7,])
nrow(Boston[Boston$rm > 8,])
sapply(Boston[Boston$rm > 8,], mean)
sapply(Boston, mean)
