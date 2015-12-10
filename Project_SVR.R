library(nlme)
library(plyr)
library(randomForest)
library(e1071)
library(glmnet)
# read in the data set that contains only with people who 
boom <- read.csv('boommagain.csv')
boom <- boom[, -c(1, 2)]

boomm <- model.matrix(~., boom)
boommm <- boomm[, -1]
# split train and test
train.ind <- sample((1:dim(boommm)[1]), 40000)
train <- boommm[train.ind, ]
test <- boommm[-train.ind, ]
train.x <- train[, -4]
train.y <- train[, 4]
test.x <- test[, -4]
test.y <- test[, 4]
# run svm
svr <- svm(train.x, train.y)
svr.predict <- predict(svr, test.x)
RMSE.svm <- sqrt(mean((svr.predict - test.y)^2))

mean.svm <- mean(test.y)
SST.svm <- sum((test.y - mean.svm)^2)
SSR.svm <- sum((svr.predict - mean.svm)^2)
R.square.svm <- SSR.svm/SST.svm

#### support vector machine for each artist
all <- boom
train.ind <- sample((1:dim(boom)[1]), 40000)
train <- all[train.ind, ]
test <- all[-train.ind, ]
test <- model.matrix(~., test)
test <- test[, -1]
test.y <- test[, 4]
test.x <- test[, -4]
test.x <- as.data.frame(test.x)
summary(test.x)
colnames(train)
svr.artist <- dlply(train, .(Artist), function (x) {
  x <- model.matrix(~., x)
  x <- x[, -1]
  rating <- x[, 4]
  x <- x[, -4]
  return(svm(x, rating, kernel = 'linear'))
}, .progress = 'text')

pred <- numeric(nrow(test.x))
for (i in 0:max(test.x$Artist)) {
  ids <- test.x$Artist == i
  if (sum(ids) > 0) {
    pred[ids] <- predict(svr.artist[[as.character(i)]], test.x[ids,])
  }
}
summary(pred)
head(test.y)
RMSE.svm <- sqrt(mean((pred - test.y)^2))

mean.svm <- mean(test.y)
SST.svm1 <- sum((test.y - mean.svm)^2)
SSR.svm1 <- sum((pred - mean.svm)^2)
R.square.svm1 <- SSR.svm1/SST.svm1



### run svm for each user group
user.1 <- model.matrix(~., user.1)[, -1]
user.2 <- model.matrix(~., user.2)[, -1]
user.3 <- model.matrix(~., user.3)[, -1]
user.4 <- model.matrix(~., user.4)[, -1]
user.1.t <- model.matrix(~., user.1.t)[, -1]
user.2.t <- model.matrix(~., user.2.t)[, -1]
user.3.t <- model.matrix(~., user.3.t)[, -1]
user.4.t <- model.matrix(~., user.4.t)[, -1]

svm.1 <- svm(user.1[, -3], user.1[, 3])
svm.2 <- svm(user.2[, -3], user.2[, 3])
svm.3 <- svm(user.3[, -3], user.3[, 3])
svm.4 <- svm(user.4[, -3], user.4[, 3])

pred.1 <- predict(svm.1, user.1.t[, -3])
pred.2 <- predict(svm.2, user.2.t[, -3])
pred.3 <- predict(svm.3, user.3.t[, -3])
pred.4 <- predict(svm.4, user.4.t[, -3])

RMSE.1 <- sqrt(mean((pred.1 - user.1.t[, 3])^2))
RMSE.2 <- sqrt(mean((pred.2 - user.2.t[, 3])^2))
RMSE.3 <- sqrt(mean((pred.3 - user.3.t[, 3])^2))
RMSE.4 <- sqrt(mean((pred.4 - user.4.t[, 3])^2))
summary(user.1.t)
mean.1 <- mean(user.1.t[3])
SST.1 <- sum((user.1.t[3] - mean.1)^2)
SSR.1 <- sum((pred.1 - mean.1)^2)
R.square.1 <- SSR.1/SST.1

mean.2 <- mean(user.2.t[3])
SST.2 <- sum((user.2.t[3] - mean.2)^2)
SSR.2 <- sum((pred.2 - mean.2)^2)
R.square.2 <- SSR.2/SST.2
# somehow the R square is infinity...
# maybe indicate that this way is totally wrong somehow

RMSE.all <- RMSE.1 * (dim(user.1.t)[1]/total) + RMSE.2 * (dim(user.2.t)[1]/total) + RMSE.3 * (dim(user.3.t)[1]/total) + RMSE.4 * (dim(user.4.t)[1]/total)
