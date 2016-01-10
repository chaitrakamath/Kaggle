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

#convert factor variables to dummies
sapply(train, function(x) class(x))
sapply(test, function(x) class(x))
setcolorder(train, c('VisitNumber', 'Weekday', 'Upc', 'ScanCount',
                     'DepartmentDescription', 'FinelineNumber', 'TripType'))
train <- dummy.data.frame(train)
test <- dummy.data.frame(test)
test[, c("DepartmentDescriptionWIRELESS")] <- 0
train$Upc <- NULL
test$Upc <- NULL
setcolorder(test, neworder = names(train)[1:79])

#set trip type to factor
train$TripType <- as.factor(train$TripType)

## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex<-as.h2o(train)

## Set up variable to use all features other than those specified here
features <- colnames(train)[(colnames(train) != 'TripType')]

## Train a random forest using all default parameters
glmHex <- h2o.glm(x=features,
                  y="TripType",
                  training_frame=trainHex,
                  model_id = 'h2o_glm',
                  family = 'multinomial',
                  lambda_search = TRUE,
                  nlambdas = 5,
                  lambda_min_ratio = 0.0001 #default
)

testHex <- as.h2o(test)
predictions <- as.data.frame(predict(glmHex, testHex))
class_pred <- t(apply(predictions[2:39], 1, function(z){
            1 * (z == max(z))
            }))
class_pred <- as.data.frame(class_pred)
class_pred <- cbind(test$VisitNumber, class_pred)
names(class_pred)[1] <- 'VisitNumber'
colnames(class_pred) <- gsub('p', 'TripType_', colnames(class_pred))
sub <- unique(class_pred)
sub <- ddply(sub, 'VisitNumber', numcolwise(sum))
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
write.csv(sub, 'h2oGLM1.csv', row.names = FALSE)

colnames(predictions)[1] <- 'PredictedClass' 
colnames(predictions) <- gsub('p', 'TripType_', colnames(predictions))
predictions <- cbind(test$VisitNumber, predictions)
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
write.csv(sub, 'h2oGLM1_WithMeanProb.csv', row.names = FALSE)

##################################################################################################
#############GLM With only VisitNumber, dummy variables of Weekday and DepartmentDescription######
##################################################################################################
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

#remove all columns except visitnumber, weekday, triptype and department description
train[, c('Upc', 'ScanCount', 'FinelineNumber') := NULL]
test[, c('Upc', 'ScanCount', 'FinelineNumber') := NULL]

#convert character columns to factors
train$Weekday <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$Weekday <- as.factor(test$Weekday)
test$DepartmentDescription <- as.factor(test$DepartmentDescription)

#since we have one less department description in test, we would add this level to test data set
levels(test$DepartmentDescription) <- levels(train$DepartmentDescription)

#convert factor variables to dummies
sapply(train, function(x) class(x))
sapply(test, function(x) class(x))
setcolorder(train, c('VisitNumber', 'Weekday', 'DepartmentDescription', 'TripType'))
train <- dummy.data.frame(train)
test <- dummy.data.frame(test)
test[, c("DepartmentDescriptionWIRELESS")] <- 0

#set trip type to factor
train$TripType <- as.factor(train$TripType)

## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex<-as.h2o(train)

## Set up variable to use all features other than those specified here
features <- colnames(train)[(colnames(train) != 'TripType')]

## Train a random forest using all default parameters
glmHex <- h2o.glm(x=features,
                  y="TripType",
                  training_frame=trainHex,
                  model_id = 'h2o_glm',
                  family = 'multinomial',
                  lambda_search = TRUE,
                  nlambdas = 5,
                  lambda_min_ratio = 0.0001 #default
)

testHex <- as.h2o(test)
predictions <- as.data.frame(predict(glmHex, testHex))
colnames(predictions)[1] <- 'PredictedClass' 
colnames(predictions) <- gsub('p', 'TripType_', colnames(predictions))
predictions <- cbind(test$VisitNumber, predictions)
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
write.csv(sub, 'h2oGLM2_WithMeanProb.csv', row.names = FALSE)


#################################################################################################
########GLM With only VisitNumber,dummy variables of Weekday and DepartmentDescription & Upc######
#################################################################################################
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

#remove all columns except visitnumber, weekday, triptype and department description
train[, c('ScanCount', 'FinelineNumber') := NULL]
test[, c('ScanCount', 'FinelineNumber') := NULL]

#impute missing values of Upc in train and test
train$Upc[is.na(train$Upc)] <- 1000000000000
test$Upc[is.na(test$Upc)] <- 1000000000000

#convert character columns to factors
train$Weekday <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$Weekday <- as.factor(test$Weekday)
test$DepartmentDescription <- as.factor(test$DepartmentDescription)

#since we have one less department description in test, we would add this level to test data set
levels(test$DepartmentDescription) <- levels(train$DepartmentDescription)

#convert factor variables to dummies
sapply(train, function(x) class(x))
sapply(test, function(x) class(x))
setcolorder(train, c('VisitNumber', 'Weekday', 'DepartmentDescription', 'TripType'))
train <- dummy.data.frame(train)
test <- dummy.data.frame(test)
test[, c("DepartmentDescriptionWIRELESS")] <- 0

#set trip type to factor
train$TripType <- as.factor(train$TripType)

## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex<-as.h2o(train)

## Set up variable to use all features other than those specified here
features <- colnames(train)[(colnames(train) != 'TripType')]

## Train a random forest using all default parameters
glmHex <- h2o.glm(x=features,
                  y="TripType",
                  training_frame=trainHex,
                  model_id = 'h2o_glm',
                  family = 'multinomial',
                  lambda_search = TRUE,
                  nlambdas = 5,
                  lambda_min_ratio = 0.0001 #default
)

testHex <- as.h2o(test)
predictions <- as.data.frame(predict(glmHex, testHex))
colnames(predictions)[1] <- 'PredictedClass' 
colnames(predictions) <- gsub('p', 'TripType_', colnames(predictions))
predictions <- cbind(test$VisitNumber, predictions)
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
write.csv(sub, 'h2oGLM3_WithMeanProb.csv', row.names = FALSE)