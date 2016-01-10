library(data.table)
library(xgboost)
library(dummies)

rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Rossmann/Script_Filesv2')
train <- fread('data/preProcessedTrain.csv')
test <- fread('data/preProcessedTest.csv')
testClosedData <- fread('data/storeClosed.csv')

#convert numberOfPromo2Days to numeric
train$numberOfPromo2Days <- as.numeric(train$numberOfPromo2Days)
test$numberOfPromo2Days <- as.numeric(test$numberOfPromo2Days)

#convert sundayOpen from logical to numeric
train$sundayOpen <- as.numeric(train$sundayOpen)
test$sundayOpen <- as.numeric(test$sundayOpen)
#convert character columns to factors and then to dummy variables
train$State <- as.factor(train$State)
train$StateHoliday <- as.factor(train$StateHoliday)
train <- dummy.data.frame(train)
names(train)

test$State <- as.factor(test$State)
test$StateHoliday <- as.factor(test$StateHoliday)
test <- dummy.data.frame(test)
names(test)

sapply(train, function(x) class(x))
sapply(test, function(x) class(x))

#build model
train_Y <- subset(train, select = c('cuberootSales'))
train_X <- train[, !colnames(train) %in% c('Sales', 'logSales', 'cuberootSales')]
test_X <- test[, colnames(test) != 'Id']

train_Y <- as.matrix(train_Y)
train_X <- as.matrix(train_X)
test_X <- as.matrix(test_X)

rmpse <- function(predictions, dtrain){
  labels <- getinfo(dtrain, 'label')
  elab <- as.numeric(labels) ^ 3
  epreds <- as.numeric(predictions) ^ 3
  err <- sqrt(mean(epreds / elab - 1) ^ 2)
  return (list(metric = 'rmpse', value = err))
}


params <- list(booster = 'gbtree',
               objective = 'reg:linear', 
               eval_metric = 'rmse',
               eta = 0.01, 
               max_depth = 13, 
               min_child_weight = 6, 
               subsample = 0.9, 
               colsample_bytree = 0.7
)

num_round <- 8000
xgtrain <- xgb.DMatrix(train_X, label = train_Y)
xgtest <- xgb.DMatrix(test_X)
bst <- xgb.train(params, xgtrain, num_round)
predictions <- predict(bst, xgtest)
preds <- predictions ^ 3

#create data frame of predicted submissions
submission <- data.frame(Id = test$Id, Sales = preds)
names(submission) <- c('Id', 'Sales')

#add information about closed stores
closedStoreSub <- subset(testClosedData, select = c('Id'))
closedStoreSub$Sales <- 0
finalSub <- rbind(submission, closedStoreSub)
write.csv(finalSub, 'xgb.csv', row.names = FALSE)
