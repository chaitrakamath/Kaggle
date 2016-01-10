###########################xgboost with only department description######
#########################################################################
library(data.table)
library(bit64)
library(xgboost)
library(plyr)
library(dummies)
rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Walmart')
train <- fread('data/train.csv')
test <- fread('data/test.csv')

#remove columns that would not used
train[, c('Weekday', 'Upc', 'ScanCount', 'FinelineNumber') := NULL]
test[, c('Weekday', 'Upc', 'ScanCount', 'FinelineNumber') := NULL]

#convert character columns to factors
train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$DepartmentDescription <- as.factor(test$DepartmentDescription)

#since we have one less department description in test, we would add this level to test data set
levels(test$DepartmentDescription) <- levels(train$DepartmentDescription)
setcolorder(train, c('VisitNumber', 'DepartmentDescription','TripType'))

#create dummies
train <- dummy.data.frame(train)
test <- dummy.data.frame(test)

#add missing column "DepartmentDescriptionWIRELESS" to test
test$DepartmentDescriptionWIRELESS <- 0

#prepare data
y <- as.integer(train[, 'TripType']) - 1
numberOfClasses <- max(y) + 1

train_label <- as.factor(train[, 'TripType'])
train_data <- as.data.frame(apply(train[, 1:70], 2, function(x) as.numeric(x)))
test_data <- as.data.frame(apply(test, 2, function(x) as.numeric(x)))
trainMatrix <- as.matrix(sapply(train[, 1:70], as.numeric))
testMatrix <- as.matrix(sapply(test[, 1:70], as.numeric))

#create DMatrix for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
dtest <- xgb.DMatrix(data = as.matrix(test_data))

#create params to be used for 
param_1 <- list(objective = 'multi:softprob', 
                eta = 0.01, #0.5, 0.7
                nrounds = 1000, #900, 800
                eval_metric = 'mlogloss',
                booster = 'gbtree', 
                max_depth = 10, #20, 40 
                min_child_weight = 2, #4, 6
                gamma = 2, #4, 10
                sumbsample = 0.5,
                colsample_bytree = 1,
                max_delta_step = 2, #0, 5
                silent = 0,
                nthread = 4, 
                maximize = FALSE
)

bst_1.cv <- xgb.cv(params = param_1, nrounds = 1000, dtrain, nfold = 5,
                   prediction = TRUE,verbose = TRUE, num_class = 39, early.stop.round = 30)


param_2 <- list(objective = 'multi:softprob', 
                eta = 0.5, #0.01, 0.7
                nrounds = 1000,
                eval_metric = 'mlogloss',
                booster = 'gbtree', 
                max_depth = 20, #20, 40 
                min_child_weight = 4, #2, 6
                gamma = 4, #2, 10
                sumbsample = 0.5,
                colsample_bytree = 1,
                max_delta_step = 2, #0, 5
                silent = 0,
                nthread = 8, 
                maximize = FALSE
) #best iteration at nrounds = 129. test-mlogloss:1.523572+0.020121

bst_2.cv <- xgb.cv(params = param_2, nrounds = 1000, dtrain, nfold = 5,
                   prediction = TRUE,verbose = TRUE, num_class = 39, early.stop.round = 30)


param_3 <- list(objective = 'multi:softprob', 
                eta = 0.7, #0.01, 0.5
                nrounds = 1000,
                eval_metric = 'mlogloss',
                booster = 'gbtree', 
                max_depth = 40, #20, 40 
                min_child_weight = 4, #2, 6
                gamma = 4, #2, 10
                sumbsample = 0.7,
                colsample_bytree = 1,
                max_delta_step = 2, #0, 5
                silent = 0,
                nthread = 8, 
                maximize = FALSE
) #best iteration at nrounds = 92. CV test set error: 1.2749

bst_3.cv <- xgb.cv(params = param_3, nrounds = 1000, dtrain, nfold = 5,
                   prediction = FALSE,verbose = TRUE, num_class = numberOfClasses, 
                   early.stop.round = 10)

bst_3.cv <- xgb.cv(params = param_3, nrounds = 1000, data = trainMatrix, label = train_label,
                   nfold = 5,prediction = TRUE,verbose = TRUE, num_class = numberOfClasses, 
                   early.stop.round = 10)


# train_data <- train[, 2:70]
# test_data <- test[, 2:70]
# train_data <- as.data.frame(apply(train_data, 2, function(x) as.numeric(x)))
# test_data <- as.data.frame(apply(test, 2, function(x) as.numeric(x)))
# dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
# dtest <- xgb.DMatrix(data = as.matrix(test_data))
# 
# param_4 <- list(objective = 'multi:softprob', 
#                 eta = 0.7, #0.01, 0.5
#                 nrounds = 1000,
#                 eval_metric = 'mlogloss',
#                 booster = 'gbtree', 
#                 max_depth = 40, #20, 40 
#                 min_child_weight = 4, #2, 6
#                 gamma = 4, #2, 10
#                 sumbsample = 0.5,
#                 colsample_bytree = 1,
#                 max_delta_step = 2, #0, 5
#                 silent = 0,
#                 nthread = 8, 
#                 maximize = FALSE
# )
# 
# bst_4.cv <- xgb.cv(params = param_4, nrounds = 1000, dtrain, nfold = 5,
#                    prediction = TRUE,verbose = TRUE, num_class = 39, early.stop.round = 30) #stop at nrounds = 38. Best error:test-mlogloss:2.272748+0.002855
# 

bst <- xgboost(data = trainMatrix, label = train_label, params = param_3, 
               nrounds = 92, verbose = 1, num_class = numberOfClasses)

predictions <- predict(bst, dtest)
str(predictions)
predictions <- as.data.frame(predictions)
str(predictions)
colnames(predictions)[1] <- 'VisitNumber'
submission <- as.data.table(predictions)
submission[, c('PredictedClass') := NULL]
sub <- aggregate(. ~ VisitNumber, data = submission, mean)
sub <- sub[order(sub$VisitNumber), ]
sub <- as.data.table(sub)
columnNames <- c('VisitNumber', 'TripType_3', 'TripType_4', 'TripType_5', 'TripType_6', 'TripType_7',
                 'TripType_8', 'TripType_9', 'TripType_12', 'TripType_14','TripType_15', 
                 'TripType_18', 'TripType_19','TripType_20', 'TripType_21', 'TripType_22', 
                 'TripType_23', 'TripType_24', 'TripType_25', 'TripType_26','TripType_27', 
                 'TripType_28', 'TripType_29', 'TripType_30', 'TripType_31', 'TripType_32', 
                 'TripType_33', 'TripType_34', 'TripType_35', 'TripType_36', 'TripType_37', 
                 'TripType_38', 'TripType_39', 'TripType_40','TripType_41', 'TripType_42', 
                 'TripType_43', 'TripType_44','TripType_999')
setcolorder(sub, neworder = columnNames)
write.csv(sub, 'xgboost_WithMeanProb.csv', row.names = FALSE)

###########################xgboost with all predictors###################
#########################################################################
library(data.table)
library(bit64)
library(glmnet)
library(dummies)
library(plyr)
rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Walmart')
train <- fread('data/train.csv')
test <- fread('data/test.csv')

#impute missing values
train$FinelineNumber[is.na(train$FinelineNumber)] <- -1000
train$Upc[is.na(train$Upc)] <- 1000000000000
test$FinelineNumber[is.na(test$FinelineNumber)] <- -1000
test$Upc[is.na(test$Upc)] <- 1000000000000

#convert character columns to factors
train$Weekday <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$Weekday <- as.factor(test$Weekday)
test$DepartmentDescription <- as.factor(test$DepartmentDescription)

#since we have one less department description in test, we would add this level to test data set
levels(test$DepartmentDescription) <- levels(train$DepartmentDescription)
setcolorder(train, c('VisitNumber', 'Weekday', 'Upc', 'ScanCount',
                     'DepartmentDescription', 'FinelineNumber', 'TripType'))
