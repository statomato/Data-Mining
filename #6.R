library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters))
Hitters <- na.omit(Hitters)

library(leaps)
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
summary(regfit.full)$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss, type="l")
which.max(reg.summary$adjr2)
par(mfrow=c(1,1))

regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat <- model.matrix(Salary~., data=Hitters[test,])
val.errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[, names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)

#Ridge & Lasso
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

library(glmnet)
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
predict(ridge.mod, s=50, type="coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

#PCR and PLS Regression
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters, scale=T, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

#2.4
#a
require(ISLR)
data(College)
set.seed(1)
trainid <- sample(1:nrow(College), nrow(College)/2)
train <- College[trainid,]
test <- College[-trainid,]
str(College)
#b
fit.lm <- lm(Apps~., data=train)
pred.lm <- predict(fit.lm, test)
(err.lm <- mean((test$Apps - pred.lm)^2))
#c
require(glmnet)
xmat.train <- model.matrix(Apps~., data=train)[,-1]
xmat.test <- model.matrix(Apps~., data=test)[,-1]
fit.ridge <- cv.glmnet(xmat.train, train$Apps, alpha=0)
(lambda <- fit.ridge$lambda.min)
pred.ridge <- predict(fit.ridge, s=lambda, newx=xmat.test)
(err.ridge <- mean((test$Apps-pred.ridge)^2))
plot(fit.ridge)
#d
fit.lasso <- cv.glmnet(xmat.train, train$Apps, alpha=1)
(lambda <- fit.lasso$lambda.min)
pred.lasso <- predict(fit.lasso, s=lambda, newx=xmat.test)
(err.lasso <- mean((test$Apps-pred.lasso)^2))
plot(fit.lasso)
coef.lasso <- predict(fit.lasso, type="coefficients", s=lambda)[1:ncol(College),]
coef.lasso[coef.lasso != 0]
round(coef.lasso, 4)
round(coef.lasso[coef.lasso != 0],4)
#e
require(pls)
set.seed(1)
fit.pcr <- pcr(Apps~., data=train, scale=T, validation="CV")
summary(fit.pcr)
validationplot(fit.pcr, val.type="MSEP",main="PCA of Apps")
pred.pcr <- predict(fit.pcr, test, ncomp=16)
(err.pcr <- mean((test$Apps-pred.pcr)^2))
#f
require(pls)
set.seed(1)
fit.pls <- plsr(Apps~., data=train, scale=TRUE, validation="CV")
summary(fit.pls)
validationplot(fit.pls, val.type="MSEP", main="PLS of Apps")
pred.pls <- predict(fit.pls, test, ncomp=10)
(err.pls <- mean((test$Apps-pred.pls)^2))
#g
err.all <- c(err.lm, err.ridge, err.lasso, err.pcr, err.pls)
names(err.all) <- c("lm","ridge","lasso","pcr","pls")
err.all
plot(test$Apps, pred.lm, xlab="test set of Apps", ylab="predicttion of Apps", main="linear regression of Apps")

#2.5
#a
require(leaps)
require(glmnet)
require(MASS)
data(Boston)

set.seed(1)
trainid <- sample(1:nrow(Boston), nrow(Boston)/2)
train <- Boston[trainid,]
test <- Boston[-trainid,]
xmat.train <- model.matrix(crim~., data=train)[,-1]
xmat.test <- model.matrix(crim~., data=test)[,-1]
str(Boston)

# ridge regression model
fit.ridge <- cv.glmnet(xmat.train, train$crim, alpha=0)
(lambda <- fit.ridge$lambda.min)  # optimal lambda
pred.ridge <- predict(fit.ridge, s=lambda, newx=xmat.test)
(err.ridge <- mean((test$crim - pred.ridge)^2))  # test error
plot(fit.ridge)
round(predict(fit.ridge, s=lambda, type="coefficients"),6)

# lasso regression model
fit.lasso <- cv.glmnet(xmat.train, train$crim, alpha=1)
(lambda <- fit.lasso$lambda.min)  # optimal lambda
pred.lasso <- predict(fit.lasso, s=lambda, newx=xmat.test)
(err.lasso <- mean((test$crim - pred.lasso)^2))  # test error
round(predict(fit.lasso, s=lambda, type="coefficients"),6)

# pcr model
fit.pcr <- pcr(crim~., data=train, scale=T, validation="CV")
summary(fit.pcr)
validationplot(fit.pcr, val.type="MSEP",main="PCA of crim")
pred.pcr <- predict(fit.pcr, test, ncomp=13)
(err.pcr <- mean((test$crim-pred.pcr)^2))

#best subset model
lm.fwd <- step(lm(crim~.,data=train), direction = "forward")
summary(lm.fwd)
lm.bwd <- step(lm(crim~.,data=train), direction = "backward")
summary(lm.bwd)
ncol(Boston)
pred.fwd <- predict(lm.fwd, test, id=i)
(err.fwd <- mean((test$crim-pred.fwd)^2))
pred.bwd <- predict(lm.bwd, test, id=i)
(err.bwd <- mean((test$crim-pred.bwd)^2))
