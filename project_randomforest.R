library(nlme)
library(plyr)
library(randomForest)
library(e1071)
library(glmnet)
# read in the data set that contains only with people who 
boom <- read.csv('boommagain.csv')
boom <- boom[, -c(1, 2)]

# random forest by artist
all.x <- boom[, -c(4)]
all.y <- boom[, 4]
train.ind <- sample((1:dim(boom)[1]), 40000)
train.x <- all.x[train.ind, ]
train.y <- all.y[train.ind]
test.x <- all.x[-train.ind, ]
test.y <- all.y[-train.ind]

train.x$rating <- train.y
rfs <- dlply(train.x, .(Artist), function (x) {
  rating <- x$rating
  x$rating <- NULL
  return(randomForest(x, rating, ntree = 100, nodesize = 10, do.trace = F, keep.forest = TRUE))
}, .progress = 'text')

pred <- numeric(nrow(test.x))
for (i in 0:max(test.x$Artist)) {
  ids <- test.x$Artist == i
  if (sum(ids) > 0) {
    pred[ids] <- predict(rfs[[as.character(i)]], test.x[ids,])
  }
}
head(pred)
y.bar <- mean(test.y)
SST <- sum((test.y - y.bar)^2)
SSR <- sum((pred - y.bar)^2)
R.square.rf <- SSR/SST
RMSE.rfs <- sqrt(mean((pred - test.y)^2))

# Since we can predict by artist, what will the result be if we bin different users by their music preference?
summary(boom$LIKE_ARTIST)
summary(booom$MUSIC)
booom <- boom
booom$User <- NULL
booom$MUSIC[boom$MUSIC == 'Music is important to me but not necessarily more important than other hobbies or interests'] <- 'Music is important to me but not necessarily more important'
booom$MUSIC[boom$MUSIC == 'Music is no longer as important as it used to be to me'] <- 'Music has no particular interest for me'
train.ind <- sample((1:dim(booom)[1]), 40000)
train <- booom[train.ind, ]
test <- booom[-train.ind, ]
test.x <- all.x[-train.ind, ]
test.y <- all.y[-train.ind]

colnames(user.1)

user.1 <- train[which(train$MUSIC == 'Music has no particular interest for me'),]
user.2 <- train[which(train$MUSIC == 'I like music but it does not feature heavily in my life'),]
user.3 <- train[which(train$MUSIC == 'Music is important to me but not necessarily more important'),]
user.4 <- train[which(train$MUSIC == 'Music means a lot to me and is a passion of mine'),]
user.1.t <- test[which(test$MUSIC == 'Music has no particular interest for me'),]
user.2.t <- test[which(test$MUSIC == 'I like music but it does not feature heavily in my life'),]
user.3.t <- test[which(test$MUSIC == 'Music is important to me but not necessarily more important'),]
user.4.t <- test[which(test$MUSIC == 'Music means a lot to me and is a passion of mine'),]
user.1$MUSIC <- NULL
user.2$MUSIC <- NULL
user.3$MUSIC <- NULL
user.4$MUSIC <- NULL
user.1.t$MUSIC <- NULL
user.2.t$MUSIC <- NULL
user.3.t$MUSIC <- NULL
user.4.t$MUSIC <- NULL
summary(user.1)
##### random forest for each binned user group
rf.1 <- randomForest(user.1[, -3], user.1$Rating, ntree = 100, nodesize = 10, do.trace = F, keep.forest = TRUE)
rf.2 <- randomForest(user.2[, -3], user.2$Rating, ntree = 100, nodesize = 10, do.trace = F, keep.forest = TRUE)
rf.3 <- randomForest(user.3[, -3], user.3$Rating, ntree = 100, nodesize = 10, do.trace = F, keep.forest = TRUE)
rf.4 <- randomForest(user.4[, -3], user.4$Rating, ntree = 100, nodesize = 10, do.trace = F, keep.forest = TRUE)
rf.1$importance
rf.1.pred <- predict(rf.1, user.1.t[, -3])
rf.2.pred <- predict(rf.2, user.2.t[, -3])
rf.3.pred <- predict(rf.3, user.3.t[, -3])
rf.4.pred <- predict(rf.4, user.4.t[, -3])

mean.1 <- mean(user.1.t$Rating)
SST.1 <- sum((user.1.t$Rating - mean.1)^2)
SSR.1 <- sum((rf.1.pred - mean.1)^2)
R.square.1 <- SSR.1/SST.1

mean.2 <- mean(user.2.t$Rating)
SST.2 <- sum((user.2.t$Rating - mean.2)^2)
SSR.2 <- sum((rf.2.pred - mean.2)^2)
R.square.2 <- SSR.2/SST.2

mean.3 <- mean(user.3.t$Rating)
SST.3 <- sum((user.3.t$Rating - mean.3)^2)
SSR.3 <- sum((rf.3.pred - mean.3)^2)
R.square.3 <- SSR.3/SST.3

mean.4 <- mean(user.4.t$Rating)
SST.4 <- sum((user.4.t$Rating - mean.4)^2)
SSR.4 <- sum((rf.4.pred - mean.4)^2)
R.square.4 <- SSR.4/SST.4

summary(rf.1.pred)
summary(rf.2.pred)
summary(rf.3.pred)
summary(rf.4.pred)

RMSE.1 <- sqrt(mean((rf.1.pred - user.1.t$Rating)^2))
RMSE.2 <- sqrt(mean((rf.2.pred - user.2.t$Rating)^2))
RMSE.3 <- sqrt(mean((rf.3.pred - user.3.t$Rating)^2))
RMSE.4 <- sqrt(mean((rf.4.pred - user.4.t$Rating)^2))

total <- dim(test)[1]
RMSE.all <- RMSE.1 * (dim(user.1.t)[1]/total) + RMSE.2 * (dim(user.2.t)[1]/total) + RMSE.3 * (dim(user.3.t)[1]/total) + RMSE.4 * (dim(user.4.t)[1]/total)
RMSE.all <- 18.55 * (dim(user.1.t)[1]/total) + 13.76 * (dim(user.2.t)[1]/total) + 15.18 * (dim(user.3.t)[1]/total) + 15.75 * (dim(user.4.t)[1]/total)
