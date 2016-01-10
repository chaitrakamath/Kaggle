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

## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex<-as.h2o(train)

## Set up variable to use all features other than those specified here
features <- colnames(train)[!(colnames(train) %in% c('Sales', 'logSales', 'cuberootSales'))]

## Train a random forest using all default parameters
gbmHex <- h2o.gbm(x=features, 
                  y="cuberootSales", 
                  training_frame=trainHex, 
                  model_id="introGBM", 
                  nbins_cats=1115, 
                  sample_rate = 0.5, 
                  col_sample_rate = 0.5, 
                  max_depth = 20, 
                  learn_rate=0.05, 
                  seed = 12345678, #Seed for random numbers (affects sampling) 
                  ntrees = 250
) 

#predict on test data set
testHex <- as.h2o(test)
predictions <- as.data.frame(predict(gbmHex, testHex))
preds <- predictions ^ 3

#create data frame of predicted submissions
submission <- data.frame(Id = test$Id, Sales = preds)
names(submission) <- c('Id', 'Sales')

#add information about closed stores
closedStoreSub <- subset(testClosedData, select = c('Id'))
closedStoreSub$Sales <- 0
finalSub <- rbind(submission, closedStoreSub)
write.csv(finalSub, 'h2oGBMSubmission.csv', row.names = FALSE)










