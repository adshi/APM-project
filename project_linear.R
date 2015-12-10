library(nlme)
library(plyr)
library(randomForest)
library(e1071)
library(glmnet)

boom <- read.csv('boommagain.csv')
boom <- boom[, -c(1, 2)]

# run linear regression on all data

train.ind <- sample((1:dim(boom)[1]), 40000)
train <- boom[train.ind, ]
test <- boom[-train.ind, ]
test.y <- test$Rating
test.x <- test
test.x$Rating <- NULL
lr.all <- lm(Rating~., data = train)
lr.pred <- predict(lr.all, test.x)
lr.rmse <- sqrt(mean((lr.pred - test.y)^2))
summary(lr.all)

# run regularized linear regression on all data

train <- boom[train.ind, ]
test <- boom[-train.ind, ]
dim(train)
isf <- which(sapply(1:ncol(train), function (n) is.factor(train[,n])))
for (i in isf) {
  train[, i] <- as.integer(train[, i])
  test[, i] <- as.integer(test[, i])
}
dim(train)
train.na <- na.omit(train)
dim(train.na)
train.y <- train$Rating
train.x <- train
train.x$Rating <- NULL
train.y <- as.matrix(train.y)
train.x <- as.matrix(train.x)
test.y <- test$Rating
test.x <- test
test.x$Rating <- NULL
test.x <- as.matrix(test.x)
dim(test.x)
dim(train.x)
colnames(test.x)
colnames(train.x)
lasso <- cv.glmnet(train.x, train.y, family = 'gaussian', alpha = 1)
coef <- coef(lasso)
df <- data.frame(name = row.names(coef), coef = as.matrix(coef))
ind <- order(df$X1, decreasing = TRUE)
df.sorted <- df[ind, ]
lasso.pred <- predict(lasso, test.x, s = 'lambda.min')
summary(lasso.pred)
lasso.pred[, 99]
RMSE <- sqrt(mean((lasso.pred - test.y)^2))

# linear regression by artist
all.x <- boom[, -4]
all.y <- boom[, 4]
isf <- which(sapply(1:ncol(all.x), function (n) is.factor(all.x[,n])))
for (i in isf) {
  all.x[, i] <- as.integer(all.x[, i])
}
train.ind <- sample((1:dim(boom)[1]), 40000)
train.x <- all.x[train.ind, ]
train.y <- all.y[train.ind]
test.x <- all.x[-train.ind, ]
test.y <- all.y[-train.ind]

train.x$rating <- train.y
lms <- dlply(train.x, .(Artist), function (x) {
  rating <- x$Rating
  x$Rating <- NULL
  return(lm(rating~., data = x))
}, .progress = 'text')

pred <- numeric(nrow(test.x))
for (i in 0:max(test.x$Artist)) {
  ids <- test.x$Artist == i
  if (sum(ids) > 0) {
    pred[ids] <- predict(lms[[as.character(i)]], test.x[ids,])
  }
}
head(pred)
RMSE.lms <- sqrt(mean((pred - test.y)^2))