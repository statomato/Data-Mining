### SVM R tutorial
# set pseudorandom number generator
set.seed(10)

# Attach Packages
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots

# Construct sample data set - completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 3/2
dat <- data.frame(x=x, y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data=dat, kernel="linear", scale=FALSE)
plot(svmfit, dat)

# fit model and produce plot
kernfit <- ksvm(x, y, type="C-svc", kernel='vanilladot')
plot(kernfit, data=x)

# Construct sample data set - not completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
dat <- data.frame(x=x, y=as.factor(y))

# Plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data=dat, kernel="linear", cost=10)
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
kernfit <- ksvm(x,y, type="C-svc", kernel='vanilladot', C=100)
# Plot results
plot(kernfit, data=x)

# find optimal cost of misclassification
tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)

# Create a table of misclassified observations
ypred <- predict(bestmod, dat)
(misclass <- table(predict=ypred, truth=dat$y))

# construct larger random data set
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2.5
x[101:150,] <- x[101:150,] - 2.5
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

set.seed(123)
train <- base::sample(200,100, replace=F)
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat)

# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,], y[train], type="C-svc", kernel='rbfdot', C=1, scaled=c())
plot(kernfit, data=x[train,])

# tune model to find optimal cost, gamma values
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
tune.out$best.model

# validate model performance
(valid <- table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newx=dat[-train,])))


##SVMs for Multiple Classes
# construct data set
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2.5
dat <- data.frame(x=x, y=as.factor(y))
# plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000","#FF0000","#00BA00")) +
  theme(legend.position = "none")

# fit model
svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

ypred <- predict(svmfit, dat)
(misclass <- table(predict=ypred, truth=dat$y))

# fit and plot
kernfit <- ksvm(as.matrix(dat[,2:1]), dat$y, type="C-svc",C=100, scaled=c())
# Create a fine grid of the feature space
x.1 <- seq(from = min(dat$x.1), to = max(dat$x.1), length = 100)
x.2 <- seq(from = min(dat$x.2), to = max(dat$x.2), length = 100)
x.grid <- expand.grid(x.2, x.1)

# Get class predictions over grid
pred <- predict(kernfit, newdata = x.grid)

# Plot the results
cols <- brewer.pal(3, "Set1")
plot(x.grid, pch = 19, col = adjustcolor(cols[pred], alpha.f = 0.05))

classes <- matrix(pred, nrow = 100, ncol = 100)
contour(x = x.2, y = x.1, z = classes, levels = 1:3, labels = "", add = TRUE)

points(dat[, 2:1], pch = 19, col = cols[predict(kernfit)])

# fit model
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
(out <- svm(y~., data=dat, kernel="linear", cost=10))

table(out$fitted, dat$y)

# validate model performance
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

### lab 9
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,]+1
plot(x, col=(3-y))

dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=F)
plot(svmfit, dat)
svmfit$index

summary(svmfit)

svmfit <- svm(y~., data=dat, kernel="linear", cost=0.1, scale=F)
plot(svmfit, dat)

#svm
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100, ]+2
x[101:150,] <- x[101:150,]-2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y)

train <- sample(200,100)
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)

svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svmfit, dat[train,])

tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newx=dat[-train,]))

# ROC Curves
library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt <- svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted, dat[train,"y"],main="Training Data")

svmfit.flex <- svm(y~., data=dat[train,], kernel="radial", gamma=5, cost=1, dicision.values=T)
fitted <- attributes(predict(svmfit.flex, dat[train,],decision.values=T))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

fitted <- attributes(predict(svmfit.opt, dat[-train,],decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"],main="Test Data")
fitted <- attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"], add=T, col="red")

# SVM with Multiple Classes
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2]+2
dat <- data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

# 9.4
set.seed(1)
par(mfrow=c(1,2))
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100, ]+2
x[101:150,] <- x[101:150,]-2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y, xlab="x1", ylab="x2")

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data=dat, kernel="linear", cost=10)
# Plot Results
plot(svmfit, dat)

train <- sample(200,100)


svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat)

svmfit <- svm(y~., data=dat[train,], kernel="polynomial", gamma=1, cost=1, degree=4)
plot(svmfit, dat)

# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,], y[train], type="C-svc", kernel='rbfdot', C=1, scaled=c())
plot(kernfit, data=x[train,])

# Fit polynomial-based SVM in kernlab
kernfit <- ksvm(x[train,], y[train], type="C-svc", kernel='polydot', degree=4,C=1, scaled=c())
plot(kernfit, data=x[train,])

library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.ra <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=10, decision.values=T)
fitted <- attributes(predict(svmfit.ra, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted, dat[train,"y"],main="Training Data")

svmfit.poly <- svm(y~., data=dat[train,], kernel="polynomial", degree=4,gamma=1, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.poly, dat[train,],decision.values=T))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

fitted <- attributes(predict(svmfit.ra, dat[-train,],decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"],main="Test Data")
fitted <- attributes(predict(svmfit.poly,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"], add=T, col="red")


tune.out.poly <- tune(svm, y~., data=dat[train,], kernel="polynomial",degree=4, ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out.poly)
table(true=dat[-train,"y"], pred=predict(tune.out.poly$best.model$fitted, newx=dat[-train,]))
out.poly <- tune.out.poly$best.model
table(true=dat[-train,"y"], pred=as.factor(out.poly$fitted))

tune.out.ra <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out.ra)
table(true=dat[-train,"y"], pred=predict(tune.out.ra$best.model, newx=dat[-train,]))


par(mfrow=c(1,1))

# 9.5
#a
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <- 1*(x1^2-x2^2 > 0)
plot(x1, x2, col=y+2)

#c
glm.fit <- glm(y ~x1+x2, family=binomial)
summary(glm.fit)

#d
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.50] = 1
table(glm.pred, y)
mean(glm.pred != y)
plot(x1, x2, col = glm.pred + 2, xlab = expression(X[1]), ylab = expression(X[2]), pch = 20)
title("Fitting Logistic Regression")

#e
glm.fit <- glm(y ~poly(x1,2)+x2, family=binomial, data=data.frame(x1,x2,y))
summary(glm.fit)

#f
glm.pred <- predict(glm.fit, data.frame(x1,x2))
plot(x1,x2,col=ifelse(glm.pred>0,'red','blue'),pch=ifelse(as.integer(glm.pred>0)==y,1,4))

#g
svm.fit <- svm(y~., data=data.frame(x1,x2,y=as.factor(y)),kernel='linear')
svm.pred <- predict(svm.fit, data.frame(x1,x2),type='reponse')
plot(x1,x2,col=ifelse(svm.pred!=0,'red','blue'),pch=ifelse(svm.pred==y,1,4))

#h
svm.fit <- svm(y~., data=data.frame(x1,x2,y=as.factor(y)),kernel='polynomial',degree=2)
svm.pred <- predict(svm.fit, data.frame(x1,x2),type='reponse')
plot(x1,x2,col=ifelse(svm.pred!=0,'red','blue'),pch=ifelse(svm.pred==y,1,4))

#i
(8+16)/(226+16+8+250)

# 9.8
#a
set.seed(42)
train <- sample(1:1070,800)
test <- (1:1070)[-train]

tb <- c()
res <- c()

#b
require(ISLR)
require(e1071)

svm.fit <- svm(Purchase~., data=OJ, subset=train, cost=0.01, kernel='linear')
summary(svm.fit)

#c
#train
svm.pred <- predict(svm.fit, OJ[train,])
table(OJ[train, 'Purchase'], svm.pred)
mean(OJ$Purchas[train] != svm.pred)

res<-cbind(res,'train'=mean(OJ$Purchase[train] != svm.pred))
#test
svm.pred <- predict(svm.fit, OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)

res <- cbind(res, 'test'=mean(OJ$Purchase[test] != svm.pred))

#d
svm.tune <- tune(svm, Purchase~., data=OJ[train,], ranges=data.frame(cost=seq(0.01,10,25)),kernel='linear')
summary(svm.tune)
svm.tune$best.model
res <- cbind(res, 'CV'=svm.tune$best.performance)


#e
svm.pred <- predict(svm.tune$best.model,OJ[train,])
table(OJ[train, 'Purchase'], svm.pred)
mean(OJ$Purchase[train]!=svm.pred)
res <- cbind(res,'train.tuned'=mean(OJ$Purchase[train]!=svm.pred))
svm.pred <- predict(svm.tune$best.model, OJ[test,])
table(OJ[test,'Purchase'], svm.pred)
mean(OJ$Purchase[test]!= svm.pred)
res <- cbind(res, 'test.tuned'=mean(OJ$Purchase[test]!= svm.pred))
tb <- rbind(tb, res)
res <- c()
tb

#f
svm.fit <- svm(Purchase~., data=OJ, subset=train, cost=0.01, kernel='radial')
summary(svm.fit)

#train
svm.pred <- predict(svm.fit, OJ[train,])
table(OJ[train,'Purchase'], svm.pred)
mean(OJ$Purchase[train] != svm.pred)

res <- cbind(res, 'train'=mean(OJ$Purchase[train]!=svm.pred))

#test
svm.pred <- predict(svm.fit, OJ[test,])
table(OJ[test,'Purchase'], svm.pred)
mean(OJ$Purchase[test]!= svm.pred)

res <- cbind(res, 'train'=mean(OJ$Purchase[test]!= svm.pred))
svm.tune <- tune(svm, Purchase~., data=OJ[train,], ranges=data.frame(cost=seq(0.01,10,25)))
summary(svm.tune)
svm.tune$best.model
res=cbind(res,'CV'=svm.tune$best.performance)

#g
svm.fit <- svm(Purchase~., data=OJ, subset=train, cost=0.01, kernel='polynomial')
summary(svm.fit)

#train
svm.pred <- predict(svm.fit, OJ[train,])
table(OJ[train, 'Purchase'], svm.pred)
mean(OJ$Purchase[train] != svm.pred)

res <- cbind(res,'train'=mean(OJ$Purchase[train]!=svm.pred))

#test
svm.pred <- predict(svm.fit, OJ[test,])
table(OJ[test, 'Purchase'], svm.pred)
mean(OJ$Purchase[test]!= svm.pred)

res <- cbind(res, 'test'=mean(OJ$Purchase[test] != svm.pred))
svm.tune <- tune(svm, Purchase~., data=OJ[train,], range=data.frame(cost=seq(0.01, 10,25)), kernel='polynomial')
summary(svm.tune)

res <- cbind(res, 'CV'=svm.tune$best.performance)
res

