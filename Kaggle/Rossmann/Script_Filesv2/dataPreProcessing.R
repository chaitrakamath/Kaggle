library(data.table)
library(lubridate)

rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Rossmann/Script_Filesv2')
train <- fread('data/train.csv')
test <- fread('data/test.csv')
store <- fread('data/store.csv')

#impute missing information about store open in test as closed
test$Open[is.na(test$Open)] <- 0

#check if store is open on Sundays
train$sundayOpen <- is.numeric(train$DayOfWeek == 7 & train$Open == 1)
test$sundayOpen <- is.numeric(test$DayOfWeek == 7 & test$Open == 1)

#separate out stores that were closed and remove open column from datasets
storeClosed <- subset(test, subset = test$Open == 0)

train <- subset(train, subset = train$Open == 1)
test <- subset(test, subset = test$Open == 1)

train[, c('Open') := NULL]
test[, c('Open') := NULL]

#check if store is refurbished
df <- data.frame(table(train$Store))
unique(df$Freq) #most of stores have 942 entries. However, stores with missing information have 758 rows of data
refurbishedStores <- as.numeric(df$Var1[df$Freq < 900])
store$refurbished <- as.numeric(store$Store %in% refurbishedStores)

#create various parts of dates
train$Date <- ymd(train$Date)
train$DayOfYear <- yday(train$Date)
train$WeekOfYear <- week(train$Date)
train$Quarter <- quarter(train$Date, with_year = FALSE)
train$StartOfMonth <- floor_date(train$Date, 'month')
train$EndOfMonth <- ceiling_date(train$Date, 'month') - days(1)
train$StartOfQuarter <- floor_date(train$Date, 'quarter') 
train$EndOfQuarter <- ceiling_date(train$Date, 'quarter') - days(1)
train$saleYear <- year(train$Date)
train$MonthOfYear <- month(train$Date)
train$isStartOfMonth <- as.numeric(as.character(train$StartOfMonth) == as.character(train$Date))
train$isEndOfMonth <- as.numeric(as.character(train$EndOfMonth) == as.character(train$Date))
train$isStartOfQuarter <- as.numeric(as.character(train$StartOfQuarter) == as.character(train$Date))
train$isEndOfQuarter <- as.numeric(as.character(train$EndOfQuarter) == as.character(train$Date))

test$Date <- ymd(test$Date)
test$DayOfYear <- yday(test$Date)
test$WeekOfYear <- week(test$Date)
test$Quarter <- quarter(test$Date, with_year = FALSE)
test$StartOfMonth <- floor_date(test$Date, 'month')
test$EndOfMonth <- ceiling_date(test$Date, 'month') - days(1)
test$StartOfQuarter <- floor_date(test$Date, 'quarter') 
test$EndOfQuarter <- ceiling_date(test$Date, 'quarter') - days(1)
test$saleYear <- year(test$Date)
test$MonthOfYear <- month(test$Date)
test$isStartOfMonth <- as.numeric(as.character(test$StartOfMonth) == as.character(test$Date))
test$isEndOfMonth <- as.numeric(as.character(test$EndOfMonth) == as.character(test$Date))
test$isStartOfQuarter <- as.numeric(as.character(test$StartOfQuarter) == as.character(test$Date))
test$isEndOfQuarter <- as.numeric(as.character(test$EndOfQuarter) == as.character(test$Date))

#check if it is first day or second of promotions
train[, firstDayOfPromotions := ifelse(shift(train$Promo, 1L, 'lead') == 1, 1, 0), by = Store]
train[, secondDayOfPromotions := ifelse(shift(train$Promo, 2L, 'lead') == 1, 1, 0), by = Store]
train$firstDayOfPromotions[is.na(train$firstDayOfPromotions)] <- 0
train$secondDayOfPromotions[is.na(train$secondDayOfPromotions)] <- 0

test[, firstDayOfPromotions := ifelse(shift(test$Promo, 1L, 'lead') == 1, 1, 0), by = Store]
test[, secondDayOfPromotions := ifelse(shift(test$Promo, 2L, 'lead') == 1, 1, 0), by = Store]
test$firstDayOfPromotions[is.na(test$firstDayOfPromotions)] <- 0
test$secondDayOfPromotions[is.na(test$secondDayOfPromotions)] <- 0

#check if it is day before / after the store was closed
train[, dayAfterClose := ifelse(shift(train$Open, type = 'lead') == 0, 1, 0), by = Store]
train[, dayBeforeClose := ifelse(shift(train$Open, type = 'lag') == 0, 1, 0), by = Store]
train$dayAfterClose[is.na(train$dayAfterClose)] <- 0
train$dayBeforeClose[is.na(train$dayBeforeClose)] <- 0

test[, dayAfterClose := ifelse(shift(test$Open, type = 'lead') == 0, 1, 0), by = Store]
test[, dayBeforeClose := ifelse(shift(test$Open, type = 'lag') == 0, 1, 0), by = Store]
test$dayAfterClose[is.na(test$dayAfterClose)] <- 0
test$dayBeforeClose[is.na(test$dayBeforeClose)] <- 0

#find number of stores in each storeType
store [, StoreTypeCount := .N, by = StoreType]

#create interaction variable of StoreType and Assortment
x <- as.data.frame(model.matrix(~ (StoreType + Assortment) ^ 2 - 1, store))
store <- cbind(store, x)

#convert promo interval to dummy variable
store$PromoInterval[store$PromoInterval == ""] <- 'None'
x <- as.data.frame(model.matrix(~ PromoInterval - 1, store))
store <- cbind(store, x)

#merge train and test datasets with store
trainWithStore <- merge(train, store, by = 'Store')
testWithStore <- merge(test, store, by = 'Store')


#add competition feature
trainWithStore$CompetitionDistance[is.na(trainWithStore$CompetitionDistance)] <- 100000
trainWithStore$CompetitionOpenSinceYear[is.na(trainWithStore$CompetitionDistance)] <- 2016
trainWithStore$CompetitionOpenSinceMonth[is.na(trainWithStore$CompetitionDistance)] <- 12

trainWithStore$CompetitionOpenSinceMonth[is.na(trainWithStore$CompetitionOpenSinceMonth)] <- 01
trainWithStore$CompetitionOpenSinceYear[is.na(trainWithStore$CompetitionOpenSinceYear)] <- 2013

trainWithStore$Competition <- (sqrt(max(trainWithStore$CompetitionDistance, na.rm = TRUE) - trainWithStore$CompetitionDistance)) * 
  (((trainWithStore$saleYear - trainWithStore$CompetitionOpenSinceYear) * 12) - (trainWithStore$CompetitionOpenSinceMonth-trainWithStore$MonthOfYear)) 

testWithStore$CompetitionDistance[is.na(testWithStore$CompetitionDistance)] <- 100000
testWithStore$CompetitionOpenSinceYear[is.na(testWithStore$CompetitionDistance)] <- 2016
testWithStore$CompetitionOpenSinceMonth[is.na(testWithStore$CompetitionDistance)] <- 12

testWithStore$CompetitionOpenSinceMonth[is.na(testWithStore$CompetitionOpenSinceMonth)] <- 01
testWithStore$CompetitionOpenSinceYear[is.na(testWithStore$CompetitionOpenSinceYear)] <- 2013
testWithStore$Competition <- (sqrt(max(testWithStore$CompetitionDistance, na.rm = TRUE) - testWithStore$CompetitionDistance))* 
  (((testWithStore$saleYear - testWithStore$CompetitionOpenSinceYear) * 12) - (testWithStore$CompetitionOpenSinceMonth-testWithStore$MonthOfYear))

#add column of date the competition opened and compute the number of days since competition is open
trainWithStore$CompetitionOpenedDay <- '01' #assume competition opened on 1st of every month
trainWithStore$CompetitionOpenSinceDate <- paste(trainWithStore$CompetitionOpenSinceYear, trainWithStore$CompetitionOpenSinceMonth, trainWithStore$CompetitionOpenedDay, sep = '-')
trainWithStore$CompetitionOpenSinceDate <- as.Date(trainWithStore$CompetitionOpenSinceDate)
trainWithStore$CompetitionOpenDuration <- difftime(trainWithStore$Date, trainWithStore$CompetitionOpenSinceDate, units = 'days')
trainWithStore$CompetitionOpenDuration[trainWithStore$CompetitionOpenDuration < 0] <- 0

trainWithStore$CompetitionOpenedDay <- NULL


testWithStore$CompetitionOpenedDay <- '01' #assume competition opened on 1st of every month
testWithStore$CompetitionOpenSinceDate <- paste(testWithStore$CompetitionOpenSinceYear, testWithStore$CompetitionOpenSinceMonth, testWithStore$CompetitionOpenedDay, sep = '-')
testWithStore$CompetitionOpenSinceDate <- as.Date(testWithStore$CompetitionOpenSinceDate)
testWithStore$CompetitionOpenDuration <- difftime(testWithStore$Date, testWithStore$CompetitionOpenSinceDate, units = 'days')
testWithStore$CompetitionOpenDuration[testWithStore$CompetitionOpenDuration < 0] <- 0

testWithStore$CompetitionOpenedDay <- NULL


#add binary column to check if promo existed before or after the competition opened
trainWithStore$CompetitionOpenSinceWeek <- format(trainWithStore$CompetitionOpenSinceDate, '%W')
trainWithStore$CompetitionOpenSinceWeekYear <- paste(trainWithStore$CompetitionOpenSinceWeek, trainWithStore$CompetitionOpenSinceYear, sep = '-')
trainWithStore$Promo2SinceWeekYear <- ifelse(is.na(trainWithStore$Promo2SinceWeek), NA, paste(trainWithStore$Promo2SinceWeek, trainWithStore$Promo2SinceYear, sep = '-'))
trainWithStore$Promo2BeforeCompetitionOpened <- ifelse(is.na(trainWithStore$Promo2SinceWeek) | is.na(trainWithStore$CompetitionOpenSinceWeek) | is.na(trainWithStore$CompetitionOpenSinceYear), 0, as.numeric(trainWithStore$Promo2SinceWeekYear < trainWithStore$CompetitionOpenSinceWeekYear))

trainWithStore$CompetitionOpenSinceDate <- NULL
trainWithStore$CompetitionOpenSinceWeek <- NULL
trainWithStore$CompetitionOpenSinceWeekYear <- NULL
trainWithStore$Promo2SinceWeekYear <- NULL

testWithStore$CompetitionOpenSinceWeek <- format(testWithStore$CompetitionOpenSinceDate, '%W')
testWithStore$CompetitionOpenSinceWeekYear <- paste(testWithStore$CompetitionOpenSinceWeek, testWithStore$CompetitionOpenSinceYear, sep = '-')
testWithStore$Promo2SinceWeekYear <- ifelse(is.na(testWithStore$Promo2SinceWeek), NA, paste(testWithStore$Promo2SinceWeek, testWithStore$Promo2SinceYear, sep = '-'))
testWithStore$Promo2BeforeCompetitionOpened <- ifelse(is.na(testWithStore$Promo2SinceWeek) | is.na(testWithStore$CompetitionOpenSinceWeek) | is.na(testWithStore$CompetitionOpenSinceYear), 0, as.numeric(testWithStore$Promo2SinceWeekYear < testWithStore$CompetitionOpenSinceWeekYear))

testWithStore$CompetitionOpenSinceDate <- NULL
testWithStore$CompetitionOpenSinceWeek <- NULL
testWithStore$CompetitionOpenSinceWeekYear <- NULL
testWithStore$Promo2SinceWeekYear <- NULL


####add column to compute number of days of promotion2 has been running before the transaction date###
#if Promo2SinceYear is NA, product is not on promotion. So, numberOfPromo2Days is 0
trainWithStore$numberOfPromo2Days <- ifelse(is.na(trainWithStore$Promo2SinceYear), 0, 'dummy')
table(is.na(trainWithStore$numberOfPromo2Days))
table(trainWithStore$numberOfPromo2Days < 0)
#if year of sale is before the promotion started or week of sale is before the promotion started in the same year, numberOfPromo2Days is 0
trainWithStore$numberOfPromo2Days[!is.na(trainWithStore$Promo2SinceYear) & (trainWithStore$Promo2SinceYear > trainWithStore$saleYear)] <- 0
table(is.na(trainWithStore$numberOfPromo2Days))
table(trainWithStore$numberOfPromo2Days < 0)
trainWithStore$numberOfPromo2Days[!is.na(trainWithStore$Promo2SinceYear) & (trainWithStore$Promo2SinceYear == trainWithStore$saleYear) & (trainWithStore$Promo2SinceWeek > trainWithStore$WeekOfYear)] <- 0
table(is.na(trainWithStore$numberOfPromo2Days))
table(trainWithStore$numberOfPromo2Days < 0)
#compute numberOfPromo2Days when promotion and sale was made in the same year
trainWithStore$sameYearDiffDays <- (trainWithStore$WeekOfYear - trainWithStore$Promo2SinceWeek) * 7 - 1
trainWithStore$sameYearDiffDays[trainWithStore$sameYearDiffDays == -1] <- 0
trainWithStore$numberOfPromo2Days[!is.na(trainWithStore$Promo2SinceYear) & (trainWithStore$Promo2SinceYear == trainWithStore$saleYear) & (trainWithStore$Promo2SinceWeek <= trainWithStore$WeekOfYear)] <- 
  trainWithStore$sameYearDiffDays[!is.na(trainWithStore$Promo2SinceYear) & (trainWithStore$Promo2SinceYear == trainWithStore$saleYear) & (trainWithStore$Promo2SinceWeek <= trainWithStore$WeekOfYear)]
table(trainWithStore$numberOfPromo2Days < 0)
table(is.na(trainWithStore$numberOfPromo2Days))
#compute numberOfPromo2Days when promotion exists since year(s) before the sale date
trainWithStore$diffYearDiffDays <- 365 * (trainWithStore$saleYear - trainWithStore$Promo2SinceYear) +  (52 - trainWithStore$Promo2SinceWeek) * 7 + trainWithStore$WeekOfYear * 7 - 1
trainWithStore$numberOfPromo2Days[!is.na(trainWithStore$Promo2SinceYear) & (trainWithStore$Promo2SinceYear < trainWithStore$saleYear)] <- trainWithStore$diffYearDiffDays[!is.na(trainWithStore$Promo2SinceYear) & (trainWithStore$Promo2SinceYear < trainWithStore$saleYear)]
table(is.na(trainWithStore$numberOfPromo2Days))
table(trainWithStore$numberOfPromo2Days < 0)

trainWithStore$sameYearDiffDays <- NULL
trainWithStore$diffYearDiffDays <- NULL

#if Promo2SinceYear is NA, product is not on promotion. So, numberOfPromo2Days is 0
testWithStore$numberOfPromo2Days <- ifelse(is.na(testWithStore$Promo2SinceYear), 0, 'dummy')
table(is.na(testWithStore$numberOfPromo2Days))
table(testWithStore$numberOfPromo2Days < 0)
#if year of sale is before the promotion started or week of sale is before the promotion started in the same year, numberOfPromo2Days is 0
testWithStore$numberOfPromo2Days[!is.na(testWithStore$Promo2SinceYear) & (testWithStore$Promo2SinceYear > testWithStore$saleYear)] <- 0
table(is.na(testWithStore$numberOfPromo2Days))
table(testWithStore$numberOfPromo2Days < 0)
testWithStore$numberOfPromo2Days[!is.na(testWithStore$Promo2SinceYear) & (testWithStore$Promo2SinceYear == testWithStore$saleYear) & (testWithStore$Promo2SinceWeek > testWithStore$WeekOfYear)] <- 0
table(is.na(testWithStore$numberOfPromo2Days))
table(testWithStore$numberOfPromo2Days < 0)
#compute numberOfPromo2Days when promotion and sale was made in the same year
testWithStore$sameYearDiffDays <- (testWithStore$WeekOfYear - testWithStore$Promo2SinceWeek) * 7 - 1
testWithStore$sameYearDiffDays[testWithStore$sameYearDiffDays == -1] <- 0
testWithStore$numberOfPromo2Days[!is.na(testWithStore$Promo2SinceYear) & (testWithStore$Promo2SinceYear == testWithStore$saleYear) & (testWithStore$Promo2SinceWeek <= testWithStore$WeekOfYear)] <- 
  testWithStore$sameYearDiffDays[!is.na(testWithStore$Promo2SinceYear) & (testWithStore$Promo2SinceYear == testWithStore$saleYear) & (testWithStore$Promo2SinceWeek <= testWithStore$WeekOfYear)]
table(testWithStore$numberOfPromo2Days < 0)
table(is.na(testWithStore$numberOfPromo2Days))
#compute numberOfPromo2Days when promotion exists since year(s) before the sale date
testWithStore$diffYearDiffDays <- 365 * (testWithStore$saleYear - testWithStore$Promo2SinceYear) +  (52 - testWithStore$Promo2SinceWeek) * 7 + testWithStore$WeekOfYear * 7 - 1
testWithStore$numberOfPromo2Days[!is.na(testWithStore$Promo2SinceYear) & (testWithStore$Promo2SinceYear < testWithStore$saleYear)] <- testWithStore$diffYearDiffDays[!is.na(testWithStore$Promo2SinceYear) & (testWithStore$Promo2SinceYear < testWithStore$saleYear)]
table(is.na(testWithStore$numberOfPromo2Days))
table(testWithStore$numberOfPromo2Days < 0)

testWithStore$sameYearDiffDays <- NULL
testWithStore$diffYearDiffDays <- NULL

#impute missing values of Promo2SinceWeek and Promo2SinceYear
trainWithStore$Promo2SinceWeek[is.na(trainWithStore$Promo2SinceWeek)] <- 52
trainWithStore$Promo2SinceYear[is.na(trainWithStore$Promo2SinceYear)] <- 2016

testWithStore$Promo2SinceWeek[is.na(testWithStore$Promo2SinceWeek)] <- 52
testWithStore$Promo2SinceYear[is.na(testWithStore$Promo2SinceYear)] <- 2016

#add log and cuberoot of sales
trainWithStore$logSales <- log1p(trainWithStore$Sales)
trainWithStore$cuberootSales <- (trainWithStore$Sales) ^ (1/3)

#add count of Stores, StoreTypes and States
trainWithStore[, countByStore := .N, by = Store]
trainWithStore[, countByStoreType := .N, by = StoreType]
trainWithStore[, countByState := .N, by = State]

testWithStore[, countByStore := .N, by = Store]
testWithStore[, countByStoreType := .N, by = StoreType]
testWithStore[, countByState := .N, by = State]

#add median Sales / Customer by Store, DayOfWeek & Promo
trainWithStore[, medianSalesPerCustomerByStoreDayOfWeekPromo := median(Sales) / median(Customers), 
               by = c('Store', 'DayOfWeek', 'Promo')]
x <- subset(trainWithStore, select = c('Store', 'DayOfWeek', 'Promo', 'medianSalesPerCustomerByStoreDayOfWeekPromo'))
setkeyv(x, cols = c('Store', 'DayOfWeek', 'Promo'))
x <- unique(x)
testWithStore <- merge(testWithStore, x, by = c('Store', 'DayOfWeek', 'Promo'), 
                       all.x = TRUE, all.y = FALSE)

#remove columns that would not be used
trainWithStore[, c('Date', 'Customers', 'StartOfMonth', 'EndOfMonth',
                   'StartOfQuarter', 'EndOfQuarter','StoreType', 'Assortment', 
                   'PromoInterval') := NULL]

testWithStore[, c('Date', 'StartOfMonth', 'EndOfMonth',
                   'StartOfQuarter', 'EndOfQuarter','StoreType', 'Assortment', 
                   'PromoInterval') := NULL]

#write datasets into files
write.csv(trainWithStore, 'data/preProcessedTrain.csv', row.names = FALSE)
write.csv(testWithStore, 'data/preProcessedTest.csv', row.names = FALSE)
write.csv(storeClosed, 'data/storeClosed.csv', row.names = FALSE)
