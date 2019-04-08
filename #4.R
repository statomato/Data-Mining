#1:lab
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9])
pairs(Smarket[,-9])
attach(Smarket)
plot(Volume)

#1.1 logistic regression
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fits)
coef(glm.fits)
glm.probs = predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred = rep("Down", 1250)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred, Direction)
mean(glm.pred==Direction)

train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type="response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits = glm(Direction~Lag1+Lag2+, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

#1.2 LDA
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
par("mar")
par(mar=c(1,1,1,1))
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>0.9)

#1.3 QDA
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

#1.4 KNN
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction,k=1)
table(knn.pred, Direction.2005)
(83+43)/252

knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

#1.5  Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)

knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

glm.fits = glm(Purchase~.,data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fits, Caravan[test,], type="response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred, test.Y)
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.25]="Yes"
table(glm.pred, test.Y)

#2-2
#a
library(ISLR)
data(Weekly)
names(Weekly)
dim(Weekly)
summary(Weekly)
# cor(Weekly[,-9])
pairs(Weekly)

#b
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fits)

#c
glm.prob <- predict(glm.fits, Weekly, type="response")
glm.pred <- ifelse(glm.prob>0.5, "Up", "Down")
table(glm.pred, Weekly$Direction)

#d
attach(Weekly)
train.y = (Weekly$Year<=2008)
Weekly.2008 = Weekly[!train,]
dim(Weekly.2008)
train <- Weekly[train.y,]
test <- Weekly[!train.y,]
fit2 <- glm(Direction~Lag2, data=train, family=binomial)
fit2.prob <- predict(fit2, test, type="response")
fit2.pred <- ifelse(fit2.prob>0.5, "Up", "Down")
table(fit2.pred, test$Direction)
mean(fit2.pred == test$Direction)

#e
library(MASS)
lda.fit <- lda(Direction~Lag2, data=train)
lda.pred <- predict(lda.fit, test)$class
table(lda.pred, test$Direction)
mean(lda.pred == test$Direction)

#f
qda.fit <- qda(Direction~Lag2, data=train)
qda.pred <- predict(qda.fit, test)$class
table(qda.pred, test$Direction)
mean(qda.pred == test$Direction)

#g
require(class)
set.seed(1)
train.x <- as.matrix(train$Lag2)
test.x <- as.matrix(test$Lag2)
knn.pred <- knn(train.x, test.x, train$Direction, k=1)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)

#i
knn.pred1 <- knn(train.x, test.x, train$Direction, k=20)
mean(knn.pred1 == test$Direction)

lda.fit <- lda(Direction~Lag2+I(Lag2^2), data=train)
lda.pred <- predict(lda.fit, test)$class
table(lda.pred, test$Direction)
mean(lda.pred == test$Direction)

qda.fit <- qda(Direction~Lag2+I(Lag2^2), data=train)
qda.pred <- predict(qda.fit, test)$class
table(qda.pred, test$Direction)
mean(qda.pred == test$Direction)

#2-3
#a
require(ISLR)
data(Auto)
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)
data3 <- data.frame(Auto, mpg01)

#b
pairs(data3)

#c
set.seed(1)
id <- sample(1:nrow(data3), nrow(data3)*0.7, replace=F)
train <- data3[id,]
test <- data3[-id,]

#d
lda.fit <- lda(mpg01~displacement+horsepower+weight+acceleration, data=train)
lda.pred <- predict(lda.fit, test)$class
table(lda.pred, test$mpg01)
mean(lda.pred != test$mpg01)
mean(lda.pred == test$mpg01)

#e
qda.fit <- qda(mpg01~displacement+horsepower+weight+acceleration, data=train)
qda.pred <- predict(qda.fit, test)$class
table(qda.pred, test$mpg01)
mean(qda.pred != test$mpg01)
mean(qda.pred == test$mpg01)

#f
log.fit <- glm(mpg01~displacement+horsepower+weight+acceleration, data=train, family=binomial)
log.prob <- predict(log.fit, test, type="response")
log.pred <- ifelse(log.prob>0.5, 1, 0)
table(log.pred, test$mpg01)
mean(log.pred != test$mpg01)
mean(log.pred == test$mpg01)

#g
train.x <- cbind(train$displacement, train$horsepower, train$weight, train$acceleration)
test.x <- cbind(test$displacement, test$horsepower, test$weight, test$acceleration)
knn.pred <- knn(train.x, test.x, train$mpg01, k=30)
mean(knn.pred != test$mpg01)
mean(knn.pred == test$mpg01)
