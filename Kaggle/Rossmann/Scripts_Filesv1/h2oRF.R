library(data.table)
library(h2o)

rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Rossmann')
train <- fread('data/preProcessedTrain.csv')
test <- fread('data/preProcessedTest.csv')
testClosedData <- fread('data/storeClosedTest.csv')

#convert character columns to factors
train$State <- as.factor(train$State)
train$Store <- as.factor(train$Store)
train$StateHoliday <- as.factor(train$StateHoliday)
train$StoreType <- as.factor(train$StoreType)
train$Assortment <- as.factor(train$Assortment)
train$PromoInterval <- as.factor(train$PromoInterval)
train$Events <- as.factor(train$Events)
train$HolidayType <- as.factor(train$HolidayType)

test$State <- as.factor(test$State)
test$Store <- as.factor(test$Store)
test$StateHoliday <- as.factor(test$StateHoliday)
test$StoreType <- as.factor(test$StoreType)
test$Assortment <- as.factor(test$Assortment)
test$PromoInterval <- as.factor(test$PromoInterval)
test$Events <- as.factor(test$Events)
test$HolidayType <- as.factor(test$HolidayType)

sapply(train, function(x) class(x))
sapply(test, function(x) class(x))

#remove some columns from train and test data sets
train[, c('refurbishmentPeriod', 'MeanDew_PointC', 'Mean_Wind_SpeedKm_h', 'Precipitationmm', 
          'Events', 'HolidayType', 'medianSalesByWeekOfYear', 'Quarter', 'medianCustomersByStorePromoDayOfWeek',
          'dayOnFIFA', 'dayOneAfterFIFA', 'dayOneBeforeFIFA', 'dayTwoAfterFIFA', 
          'dayTwoBeforeFIFA', 'Mean_Sea_Level_PressurehPa', 'Mean_TemperatureC','Area',
          'Population', 'Density', 'GDP', 'medianSalesByStorePromoDayOfWeek',
          'daily_rossmann', 'Mean_Humidity', 'isHoliday') := NULL]
test[,c('refurbishmentPeriod', 'MeanDew_PointC', 'Mean_Wind_SpeedKm_h', 'Precipitationmm', 
        'Events', 'HolidayType', 'medianSalesByWeekOfYear', 'Quarter', 'medianCustomersByStorePromoDayOfWeek',
        'dayOnFIFA', 'dayOneAfterFIFA', 'dayOneBeforeFIFA', 'dayTwoAfterFIFA', 
        'dayTwoBeforeFIFA', 'Mean_Sea_Level_PressurehPa', 'Mean_TemperatureC','Area',
        'Population', 'Density', 'GDP', 'medianSalesByStorePromoDayOfWeek',
        'daily_rossmann', 'Mean_Humidity', 'isHoliday') := NULL]


## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex<-as.h2o(train)

## Set up variable to use all features other than those specified here
features <- colnames(train)[!(colnames(train) %in% c('Sales', 'logSales'))]

## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="logSales",
                          mtries = -1, #default
                          sample_rate = 0.632, #default
                          model_id = 'intro_RF',
                          ntrees = 100,
                          max_depth = 30,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=trainHex, 
                          nfolds = 0, 
                          seed = 12345678 #Seed for random numbers (affects sampling)
                          )

#predict on test data set
testHex <- as.h2o(test)
predictions <- as.data.frame(predict(rfHex, testHex))
preds <- expm1(predictions)

#create data frame of predicted submissions
submission <- data.frame(Id = test$Id, State = test$State, Sales = preds)
names(submission) <- c('Id', 'State', 'Sales')

#since we have stores with same storeNum in HB and NI, add their predictions
HBPredictions <- subset(submission, submission$State == 'HB')
NIPredictions <- subset(submission, submission$State == 'NI')
sub <- subset(submission, !submission$State %in% c('HB', 'NI'))

HB_NIPredictions <- data.frame(Id = HBPredictions$Id, State = 'HB, NI', Sales = HBPredictions$Sales + NIPredictions$Sales)
sub2 <- rbind(sub, HB_NIPredictions)

#add information about closed stores
closedStoreSub <- subset(testClosedData, select = c('Id'))
closedStoreSub$Sales <- 0
finalSub <- rbind(sub2[, c('Id', 'Sales')], closedStoreSub)
write.csv(finalSub, 'h2oRFSubmission.csv', row.names = FALSE)










