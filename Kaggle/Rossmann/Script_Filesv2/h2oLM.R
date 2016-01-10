library(data.table)
library(h2o)

rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Rossmann/Script_Filesv2')
train <- fread('data/preProcessedTrain.csv')
test <- fread('data/preProcessedTest.csv')
testClosedData <- fread('data/storeClosed.csv')

#convert character columns to factors
train$State <- as.factor(train$State)
train$Store <- as.factor(train$Store)
train$StateHoliday <- as.factor(train$StateHoliday)

test$State <- as.factor(test$State)
test$Store <- as.factor(test$Store)
test$StateHoliday <- as.factor(test$StateHoliday)

sapply(train, function(x) class(x))
sapply(test, function(x) class(x))

#create different data sets by state
train_bw <- subset(train, train$State == 'BW')
train_by <- subset(train, train$State == 'BY')
train_hbni <- subset(train, train$State == 'HB, NI')
train_he <- subset(train, train$State == 'HE')
train_hh <- subset(train, train$State == 'HH')
train_nw <- subset(train, train$State == 'NW')
train_rp <- subset(train, train$State == 'RP')
train_sh <- subset(train, train$State == 'SH')

test_bw <- subset(test, test$State == 'BW')
test_by <- subset(test, test$State == 'BY')
test_hbni <- subset(test, test$State == 'HB, NI')
test_he <- subset(test, test$State == 'HE')
test_hh <- subset(test, test$State == 'HH')
test_nw <- subset(test, test$State == 'NW')
test_rp <- subset(test, test$State == 'RP')
test_sh <- subset(test, test$State == 'SH')

#Since each dataset has only one state, remove state column
train_bw[, c('State') := NULL]
train_by[, c('State') := NULL]  
train_hbni[, c('State') := NULL]  
train_he[, c('State') := NULL] 
train_hh[, c('State') := NULL]  
train_nw[, c('State') := NULL]  
train_rp[, c('State') := NULL]  
train_sh[, c('State') := NULL]  

test_bw[, c('State') := NULL]
test_by[, c('State') := NULL]  
test_hbni[, c('State') := NULL]  
test_he[, c('State') := NULL] 
test_hh[, c('State') := NULL]  
test_nw[, c('State') := NULL]  
test_rp[, c('State') := NULL]  
test_sh[, c('State') := NULL]  

## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex_bw <-as.h2o(train_bw)
trainHex_by <-as.h2o(train_by)
trainHex_hbni <-as.h2o(train_hbni)
trainHex_he <-as.h2o(train_he)
trainHex_hh <-as.h2o(train_hh)
trainHex_nw <-as.h2o(train_nw)
trainHex_rp <-as.h2o(train_rp)
trainHex_sh <-as.h2o(train_sh)

## Set up variable to use all features other than those specified here
features <- colnames(train)[!(colnames(train) %in% c('State','Sales', 'logSales', 'cuberootSales'))]

gaussianHex_bw <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_bw,
                        family = 'gaussian')
gaussianHex_by <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_by,
                        family = 'gaussian')
gaussianHex_hbni <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_hbni,
                        family = 'gaussian')
gaussianHex_he <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_he,
                        family = 'gaussian')
gaussianHex_hh <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_hh,
                        family = 'gaussian')
gaussianHex_nw <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_nw,
                        family = 'gaussian')
gaussianHex_rp <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_rp,
                        family = 'gaussian')
gaussianHex_sh <- h2o.glm(x = features, 
                        y = 'cuberootSales', 
                        training_frame = trainHex_sh,
                        family = 'gaussian')

#predict on test data set
testHex_bw <- as.h2o(test_bw)
predictions_bw <- as.data.frame(predict(gaussianHex_bw, testHex_bw))
preds_bw <- predictions_bw ^ 3

testHex_by <- as.h2o(test_by)
predictions_by <- as.data.frame(predict(gaussianHex_by, testHex_by))
preds_by <- predictions_by ^ 3

testHex_hbni <- as.h2o(test_hbni)
predictions_hbni <- as.data.frame(predict(gaussianHex_hbni, testHex_hbni))
preds_hbni <- predictions_hbni ^ 3

testHex_he <- as.h2o(test_he)
predictions_he <- as.data.frame(predict(gaussianHex_he, testHex_he))
preds_he <- predictions_he ^ 3

testHex_hh <- as.h2o(test_hh)
predictions_hh <- as.data.frame(predict(gaussianHex_hh, testHex_hh))
preds_hh <- predictions_hh ^ 3

testHex_nw <- as.h2o(test_nw)
predictions_nw <- as.data.frame(predict(gaussianHex_nw, testHex_nw))
preds_nw <- predictions_nw ^ 3

testHex_rp <- as.h2o(test_rp)
predictions_rp <- as.data.frame(predict(gaussianHex_rp, testHex_rp))
preds_rp <- predictions_rp ^ 3

testHex_sh <- as.h2o(test_sh)
predictions_sh <- as.data.frame(predict(gaussianHex_sh, testHex_sh))
preds_sh <- predictions_sh ^ 3

#create data frame of predicted submissions
submission_bw <- data.frame(Id = test_bw$Id, Sales = preds_bw)
names(submission_bw) <- c('Id', 'Sales')

submission_by <- data.frame(Id = test_by$Id, Sales = preds_by)
names(submission_by) <- c('Id', 'Sales')

submission_hbni <- data.frame(Id = test_hbni$Id, Sales = preds_hbni)
names(submission_hbni) <- c('Id', 'Sales')

submission_he <- data.frame(Id = test_he$Id, Sales = preds_he)
names(submission_he) <- c('Id', 'Sales')

submission_hh <- data.frame(Id = test_hh$Id, Sales = preds_hh)
names(submission_hh) <- c('Id', 'Sales')

submission_nw <- data.frame(Id = test_nw$Id, Sales = preds_nw)
names(submission_nw) <- c('Id', 'Sales')

submission_rp <- data.frame(Id = test_rp$Id, Sales = preds_rp)
names(submission_rp) <- c('Id', 'Sales')

submission_sh <- data.frame(Id = test_sh$Id, Sales = preds_sh)
names(submission_sh) <- c('Id', 'Sales')

submission <- rbind(submission_bw, submission_by, submission_he,
                    submission_hbni, submission_hh, submission_nw, submission_rp, submission_sh)
submission$Sales[submission$Sales < 0] <- 0

#add information about closed stores
closedStoreSub <- subset(testClosedData, select = c('Id'))
closedStoreSub$Sales <- 0
finalSub <- rbind(submission, closedStoreSub)
write.csv(finalSub, 'h2oClusteredLM.csv', row.names = FALSE)
