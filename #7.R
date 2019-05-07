install.packages("xgboost")

data(agaricus.train, package="xgboost")
data(agaricus.test, package="xgboost")
train <- agaricus.train
test <- agaricus.test

str(train)
dim(train$data)
dim(test$data)

class(train$data)[1]
class(train$label)

bstSparse <- xgboost(data=train$data, label=train$label, max_depth=2, eta=1, nthread=2, nrounds=2, objective="binary:logistic")
bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

# verbose = 0, no message
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
# verbose = 1, print evaluation metric
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)
# verbose = 2, also print information about tree
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)

pred <- predict(bst, test$data)

# size of the prediction vector
print(length(pred))
# limit display of predictions to the first 10
print(head(pred))

prediction <- as.numeric(pred > 0.5)
print(head(prediction))
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")
bst <- xgb.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")
bst <- xgb.train(data=dtrain, booster = "gblinear", max_depth=2, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")

xgb.DMatrix.save(dtrain, "dtrain.buffer")
# to load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")
bst <- xgb.train(data=dtrain2, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
xgb.dump(bst, with_stats = T)
# xgb.plot.tree(model = bst)

# save model to binary local file
xgb.save(bst, "xgboost.model")
# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)

# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))
# save model to R's raw vector
rawVec <- xgb.save.raw(bst)

# print class
print(class(rawVec))
# load binary model to R
bst3 <- xgb.load(rawVec)
pred3 <- predict(bst3, test$data)

# pred2 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred2-pred))))


#################################################
path <- "C:/대학원/2019-1/1.전공/2.데이터마이닝/#7"
setwd(path)
library(data.table)
library(mlr)
setcol <- c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","target")
train <- read.table("adult.data", header = F, sep = ",", col.names = setcol, na.strings = c(" ?"), stringsAsFactors = F)
test <- read.table("adult.test",header = F,sep = ",",col.names = setcol,skip = 1, na.strings = c(" ?"),stringsAsFactors = F)
setDT(train)
setDT(test)

table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))*100
table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100

library(stringr)
test[, target := substr(target, start=1, stop=nchar(target)-1)]

char_col <- colnames(train)[sapply(test, is.character)]
for(i in char_col) set(train, j=i, value=str_trim(train[[i]], side="left"))
for(i in char_col) set(test, j=i, value=str_trim(test[[i]],side="left"))

train[is.na(train)] <- "Missing"
test[is.na(test)] <- "Missing"

labels <- train$target
ts_label <- test$target
new_tr <- model.matrix(~.+0, data=train[,-c("target"),with=F])
new_ts <- model.matrix(~.+0, data=test[,-c("target"), with=F])

labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

dtrain <- xgb.DMatrix(data=new_tr, label=labels)
dtest <- xgb.DMatrix(data=new_ts, label=ts_label)

params <- list(booster="gbtree", objective="binary:logistic",eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
# xgbcv <- xgb.cv(params=params, data=dtrain, nrounds=100, nfold=5, showsd=T, stratified=T, print.every.n=10, early.stop.round=20, maximize=F)

#8.8
#a
require(ISLR)
require(tree)
set.seed(42)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)

#b
tree.carseats <- tree(formula=Sales~.,data=Carseats,subset = train)
tree.pred <- predict(tree.carseats,Carseats[-train,])

mean((tree.pred-Carseats[-train,'Sales'])^2)

plot(tree.carseats)
text(tree.carseats)

#c
tree.carseats.cv <- cv.tree(tree.carseats) 
plot(tree.carseats.cv)

prune.carseats <- prune.tree(tree.carseats,best=5)

plot(prune.carseats)
text(prune.carseats) 

tree.pred=predict(prune.carseats,Carseats[-train,])
mean((tree.pred-Carseats[-train,'Sales'])^2)

plot(tree.pred,Carseats[-train,'Sales'],xlab='prediction',ylab='actual')
abline(0,1)

#d
require(randomForest)

d=ncol(Carseats)-1

set.seed(42)
carseats.rf=randomForest(Sales~.,data=Carseats,subset=train,mtry=d,importance=T,ntree=100)

tree.pred=predict(carseats.rf,Carseats[-train,])
mean((tree.pred-Carseats[-train,'Sales'])^2)

plot(carseats.rf)
varImpPlot(carseats.rf)
(importance(carseats.rf))

#e
mse=c()

for(i in 3:10){
  carseats.rf=randomForest(Sales~.,data=Carseats,subset=train,mtry=5,importance=T,ntree=100)
  
  tree.pred=predict(carseats.rf,Carseats[-train,])
  mse=rbind(mse,mean((tree.pred-Carseats[-train,'Sales'])^2))
}
plot(3:10,mse,type='b')

carseats.rf=randomForest(Sales~.,data=Carseats,subset=train,mtry=9,importance=T,ntree=100)

plot(carseats.rf)
varImpPlot(carseats.rf)
(importance(carseats.rf))

#8.9
#a
train=sample(1:nrow(OJ),800)

OJ.train=OJ[train,]
OJ.test=OJ[-train,]

#b
OJ.tree=tree(Purchase~.,data=OJ.train) 
summary(OJ.tree)

#c
OJ.tree

#d
plot(OJ.tree)
text(OJ.tree)

#e
OJ.pred.train=predict(OJ.tree,OJ.train,type = 'class')
# table(OJ.train[,'Purchase'],OJ.pred.train)
# table(OJ.train[,'Purchase'],OJ.pred.train)/nrow(OJ.train)
OJ.pred.test=predict(OJ.tree,OJ.test,type = 'class')
table(OJ.test[,'Purchase'],OJ.pred.test)
table(OJ.test[,'Purchase'],OJ.pred.test)/nrow(OJ.test)

#f
OJ.tree.cv=cv.tree(OJ.tree,K=10,FUN = prune.misclass)
plot(OJ.tree.cv)

plot(OJ.tree.cv$size,OJ.tree.cv$dev, type="b",xlab="size",ylab="deviance")

OJ.tree=prune.misclass(OJ.tree,best = 2)

OJ.pred.train=predict(OJ.tree,OJ.train,type = 'class')
table(OJ.train[,'Purchase'],OJ.pred.train)

OJ.pred.test=predict(OJ.tree,OJ.test,type = 'class')
table(OJ.test[,'Purchase'],OJ.pred.test)

plot(OJ.tree)
text(OJ.tree)

#8.10
#a
Hitters.unknownSal=is.na(Hitters[,"Salary"])
Hitters=Hitters[!Hitters.unknownSal,]
Hitters[,"Salary"]=log(Hitters[,"Salary"])

summary(Hitters)

#b
Hitters.train=Hitters[1:200,]
Hitters.test=Hitters[-c(1:200),]

#c
require(gbm)
train.mse=c()
test.mse=c()

for(shr in seq(0,0.08,0.002)){
  Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = shr,n.trees = 1000,distribution = 'gaussian')
  
  Hitters.pred=predict(Hitters.gbm,Hitters.train,n.trees = 1000)
  train.mse=rbind(train.mse,mean((Hitters.pred-Hitters.train[,'Salary'])^2))
  
  Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
  test.mse=rbind(test.mse,mean((Hitters.pred-Hitters.test[,'Salary'])^2))
}

plot(seq(0,0.08,0.002),test.mse,type='l',xlab='shrinkage',xlim = c(0.003,0.07),ylab='MSE')
lines(seq(0,0.08,0.002),test.mse,col='red')
legend(x=0.045,y=0.7,legend = c('train MSE','test MSE'),col=c('black','red'),lty=1)
which.min(test.mse)

#e
tb=c()

Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = 0.01,n.trees = 1000,distribution = 'gaussian')
Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
tb=cbind(tb,'Boost'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch3 - linear regression
Hitters.lm=lm(Salary~.,Hitters.train)
Hitters.pred=predict(Hitters.lm,Hitters.test)
tb=cbind(tb,'Linear'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch6 - ridge regression
require(glmnet)

x = model.matrix(Salary ~ ., data = Hitters.train)
x.test = model.matrix(Salary ~ ., data = Hitters.test)
y = Hitters.train$Salary

Hitters.glm=glmnet(x,y,alpha = 0)
Hitters.pred=predict(Hitters.glm,x.test)
tb=cbind(tb,'Ridge'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

kable(tb)

#f
summary(Hitters.gbm)

#g
require(randomForest)
Hitters.rf=randomForest(Salary~.,data = Hitters.train,mtry=ncol(Hitters.train)-1) # bagging m=p
Hitters.pred=predict(Hitters.rf,Hitters.test)
mean((Hitters.pred-Hitters.test[,'Salary'])^2)




