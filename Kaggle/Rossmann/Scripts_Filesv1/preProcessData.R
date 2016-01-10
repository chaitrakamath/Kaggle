setwd('/Users/admin/Documents/DSCompetitions/Rossman - Kaggle')
rm(list = ls())
##############LOAD LIBRARIES AND READ DATA###########
library(data.table)
library(h2o)
library(dygraphs)
library(lubridate)

train <- fread('data/train.csv')
test <- fread('data/test.csv')
store <- fread('data/store.csv')
weather <- fread('data/weatherData.csv')
fifa <- fread('data/fifaDates.csv')
holidays <- fread('data/holidays.csv')
trends <- fread('data/gt_trends_DE.csv')
state_stats <- fread('data/state_stats.csv')
##############IMPUTE MISSING VALUES#################
#Convert Date column to Date type
train$Date <- ymd(train$Date)
test$Date <- ymd(test$Date)
weather$Date <- ymd(weather$Date)
fifa$Date <- mdy(fifa$Date)
holidays$Date <- mdy(holidays$Date)
trends$Day <- ymd(trends$Day)

#Replace odd characters from ObservedIn column in holidays dataset
holidays$ObservedIn <- gsub('\xca', '', holidays$ObservedIn)

#Convert SchoolHoliday to numeric
train$SchoolHoliday <- as.numeric(train$SchoolHoliday)
test$SchoolHoliday <- as.numeric(test$SchoolHoliday)

#Impute missing values of Open in test data set as Open = 0 (per comp admin)
test$Open[is.na(test$Open)] <- 0

#If PromoInterval == "" & Promo2SinceWeek and Promo2SinceYear are missing then, there
#are no Promo2 running as of now. Set appropriate values for these
store$Promo2SinceWeek[is.na(store$Promo2SinceWeek)] <- 100
store$Promo2SinceYear[is.na(store$Promo2SinceYear)] <- 2999
store$PromoInterval[store$PromoInterval == ""] <- 'None'

#if competition distance & competitionOpenSinceMonth and competitionOpenSinceYear are missing, 
#assume that there is no competition nearby and impute sensible values
store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 100000
store$CompetitionOpenSinceMonth[is.na(store$CompetitionDistance)] <- 20
store$CompetitionOpenSinceYear[is.na(store$CompetitionDistance)] <- 2999

store$CompetitionOpenSinceYear[is.na(store$CompetitionOpenSinceYear)] <- 2013
store$CompetitionOpenSinceMonth[is.na(store$CompetitionOpenSinceMonth)] <- 9

#use log transform of Sales instead of Sales itself as Sales spans on a larger scale
train$logSales <- log1p (train$Sales)

#impute missing data from weather 
weather$MonthOfYear <- month(weather$Date)
weather$Year <- year(weather$Date)


###############FEATURE ENGINEERING###################
#Create various time types from date column
train$DayOfYear <- yday(train$Date)
train$WeekOfYear <- week(train$Date)
train$Quarter <- quarter(train$Date, with_year = TRUE)
train$StartOfMonth <- floor_date(train$Date, 'month')
train$EndOfMonth <- ceiling_date(train$Date, 'month') - days(1)
train$StartOfQuarter <- floor_date(train$Date, 'quarter') 
train$EndOfQuarter <- ceiling_date(train$Date, 'quarter') - days(1)
train$Year <- year(train$Date)
train$MonthOfYear <- month(train$Date)
train$isStartOfMonth <- as.numeric(as.character(train$StartOfMonth) == as.character(train$Date))
train$isEndOfMonth <- as.numeric(as.character(train$EndOfMonth) == as.character(train$Date))
train$isStartOfQuarter <- as.numeric(as.character(train$StartOfQuarter) == as.character(train$Date))
train$isEndOfQuarter <- as.numeric(as.character(train$EndOfQuarter) == as.character(train$Date))

test$DayOfYear <- yday(test$Date)
test$WeekOfYear <- week(test$Date)
test$Quarter <- quarter(test$Date, with_year = TRUE)
test$StartOfMonth <- floor_date(test$Date, 'month')
test$EndOfMonth <- ceiling_date(test$Date, 'month') - days(1)
test$StartOfQuarter <- floor_date(test$Date, 'quarter') 
test$EndOfQuarter <- ceiling_date(test$Date, 'quarter') - days(1)
test$Year <- year(test$Date)
test$MonthOfYear <- month(test$Date)
test$isStartOfMonth <- as.numeric(as.character(test$StartOfMonth) == as.character(test$Date))
test$isEndOfMonth <- as.numeric(as.character(test$EndOfMonth) == as.character(test$Date))
test$isStartOfQuarter <- as.numeric(as.character(test$StartOfQuarter) == as.character(test$Date))
test$isEndOfQuarter <- as.numeric(as.character(test$EndOfQuarter) == as.character(test$Date))


#for all the stores, we typically have information from 2013-01-01 to 2015-07-31
#however, for some stores, we have information missing since 2014-07-01 to 2014-12-31
df <- data.frame(table(train$Store))
sort(unique(df$Freq)) #most of stores more than 900 entries. However, stores with missing information have 758 rows of data
refurbishedStores <- as.numeric(df$Var1[df$Freq < 900])
#add flag to store if it was refurbished
store$refurbished <- as.numeric(store$Store %in% refurbishedStores)

#add flag if there exists data for these refurbished stores in train and test
train$postRefurb <- ifelse(train$Store %in% refurbishedStores & as.character(train$Date) > '2014-12-31', 1, 0)
test$postRefurb <- ifelse(test$Store %in% refurbishedStores & as.character(test$Date) > '2014-12-31', 1, 0)

#Median Sales by Store, Promo, DayOfWeek
train[, medianSalesByStorePromoDayOfWeek := median(as.numeric(Sales)), by = c('Store', 'Promo', 'DayOfWeek')]
x <- subset(train, select = c('Store', 'Promo', 'DayOfWeek', 'medianSalesByStorePromoDayOfWeek'))
x <- unique(x)
setkey(x, Store, Promo, DayOfWeek)
test <- merge(test, x, by = c('Store', 'Promo', 'DayOfWeek'), all.x = TRUE, all.y = FALSE)

#Median Customers by Store, Promo, DayOfWeek
train[, medianCustomersByStorePromoDayOfWeek := median(as.numeric(Sales)), by = c('Store', 'Promo', 'DayOfWeek')]
x <- subset(train, select = c('Store', 'Promo', 'DayOfWeek', 'medianCustomersByStorePromoDayOfWeek'))
x <- unique(x)
setkey(x, Store, Promo, DayOfWeek)
test <- merge(test, x, by = c('Store', 'Promo', 'DayOfWeek'), all.x = TRUE, all.y = FALSE)

#Median Sales by WeekOfYear, WeekOfMonth across each store
train[, medianSalesByWeekOfYear := median(as.numeric(Sales)), by = c('Store', 'WeekOfYear')]
x <- subset(train, select = c('Store','WeekOfYear', 'medianSalesByWeekOfYear'))
x <- unique(x)
setkey(x, Store, WeekOfYear)
test <- merge(test, x, by = c('Store', 'WeekOfYear'), all.x = TRUE, all.y = FALSE)

#check if store is open on weekends
store$sundayOpen <- as.numeric(store$Store %in% unique(train$Store[train$DayOfWeek == 7 & train$Open == 1]))

#add flags to mark first and second day of promotions
train[, firstDayOfPromotions := ifelse(shift(train$Promo, 1L, 'lead') == 1, 1, 0), by = Store]
train[, secondDayOfPromotions := ifelse(shift(train$Promo, 2L, 'lead') == 1, 1, 0), by = Store]
train$firstDayOfPromotions[is.na(train$firstDayOfPromotions)] <- 0
train$secondDayOfPromotions[is.na(train$secondDayOfPromotions)] <- 0

test[, firstDayOfPromotions := ifelse(shift(test$Promo, 1L, 'lead') == 1, 1, 0), by = Store]
test[, secondDayOfPromotions := ifelse(shift(test$Promo, 2L, 'lead') == 1, 1, 0), by = Store]
test$firstDayOfPromotions[is.na(test$firstDayOfPromotions)] <- 0
test$secondDayOfPromotions[is.na(test$secondDayOfPromotions)] <- 0

#add flags to mark day before store was closed and day after store was closed
train[, dayAfterClose := ifelse(shift(train$Open, type = 'lead') == 0, 1, 0), by = Store]
train[, dayBeforeClose := ifelse(shift(train$Open, type = 'lag') == 0, 1, 0), by = Store]
train$dayAfterClose[is.na(train$dayAfterClose)] <- 0
train$dayBeforeClose[is.na(train$dayBeforeClose)] <- 0

test[, dayAfterClose := ifelse(shift(test$Open, type = 'lead') == 0, 1, 0), by = Store]
test[, dayBeforeClose := ifelse(shift(test$Open, type = 'lag') == 0, 1, 0), by = Store]
test$dayAfterClose[is.na(test$dayAfterClose)] <- 0
test$dayBeforeClose[is.na(test$dayBeforeClose)] <- 0

#compute refurbishment period(days from 2014-07-01 to 2014-12-31)
refurbishmentPeriod <- difftime(as.Date('2014-12-31'), as.Date('2014-06-30'), units = 'days')
store$refurbishmentPeriod <- ifelse(store$refurbished == 1, refurbishmentPeriod, 0)

#Compute number of days between transaction date and date when refurbished store came back
daysAfterRefurb <- difftime(train$Date, as.Date('2015-01-01'), units = 'days')
train$daysAfterRefurb <- ifelse((train$Store %in% refurbishedStores) & (as.character(train$Date) > '2015-01-01'), daysAfterRefurb, 0)
daysBeforeRefurb <- difftime(as.Date('2014-07-01'), train$Date, units = 'days')
train$daysBeforeRefurb <- ifelse((train$Store %in% refurbishedStores)  & (as.character(train$Date) < '2014-07-01'), daysBeforeRefurb, 0)

daysAfterRefurb <- difftime(test$Date, as.Date('2015-01-01'), units = 'days')
test$daysAfterRefurb <- ifelse((test$Store %in% refurbishedStores) & (as.character(test$Date) > '2015-01-01'), daysAfterRefurb, 0)
daysBeforeRefurb <- difftime(as.Date('2014-07-01'), test$Date, units = 'days')
test$daysBeforeRefurb <- ifelse((test$Store %in% refurbishedStores)  & (as.character(test$Date) < '2014-07-01'), daysBeforeRefurb, 0)

train$isDayBeforeRefurb <- ifelse(train$daysBeforeRefurb == 1, 1, 0)
train$isDayAfterRefurb <- ifelse(train$daysAfterRefurb == 1, 1, 0)

test$isDayBeforeRefurb <- ifelse(test$daysBeforeRefurb == 1, 1, 0)
test$isDayAfterRefurb <- ifelse(test$daysAfterRefurb == 1, 1, 0)

#If Customers = 0 or Sales = 0, impute Open = 0
train$Open[train$Customers == 0 | train$Sales == 0] <- 0

#Separate out rows of data when Store was closed. Assign Sales of these stores = 0
storeClosedTrain <- subset(train, subset = train$Open == 0)
storeClosedTest <- subset(test, subset = test$Open == 0)

train <- subset(train, train$Open == 1)
test <- subset(test, test$Open == 1)

train$Open <- NULL
test$Open <- NULL

#mark dates one or two days before and after FIFA 
train$dayOnFIFA <- ifelse(as.character(train$Date) %in% as.character(fifa$Date), 1, 0)
train$dayOneAfterFIFA <- ifelse(as.character(train$Date + days(1)) %in% as.character(fifa$Date), 1, 0)
train$dayOneBeforeFIFA <- ifelse(as.character(train$Date - days(1)) %in% as.character(fifa$Date), 1, 0)
train$dayTwoAfterFIFA <- ifelse(as.character(train$Date + days(2)) %in% as.character(fifa$Date), 1, 0)
train$dayTwoBeforeFIFA <- ifelse(as.character(train$Date - days(2)) %in% as.character(fifa$Date), 1, 0)

test$dayOnFIFA <- ifelse(as.character(test$Date) %in% as.character(fifa$Date), 1, 0)
test$dayOneAfterFIFA <- ifelse(as.character(test$Date + days(1)) %in% as.character(fifa$Date), 1, 0)
test$dayOneBeforeFIFA <- ifelse(as.character(test$Date - days(1)) %in% as.character(fifa$Date), 1, 0)
test$dayTwoAfterFIFA <- ifelse(as.character(test$Date + days(2)) %in% as.character(fifa$Date), 1, 0)
test$dayTwoBeforeFIFA <- ifelse(as.character(test$Date - days(2)) %in% as.character(fifa$Date), 1, 0)

#add information about google trends
train <- merge(train, trends, by.x = 'Date', by.y = 'Day', all.x = TRUE, all.y = FALSE)
test <- merge(test, trends, by.x = 'Date', by.y = 'Day', all.x = TRUE, all.y = FALSE)

#add information about store
train <- merge(train, store, by = 'Store', all.x = TRUE, all.y = TRUE)
test <- merge(test, store, by = 'Store', all.x = TRUE, all.y = FALSE)
#Since we have some stores that are in two different states:HB, NI, we will separate these rows out from the train and test sets
HBTrain <- subset(train, train$State == "HB, NI")
NITrain <- subset(train, train$State == "HB, NI")
HBTrain$State <- 'HB'
NITrain$State <- 'NI'
doubleStatesTrain <- rbind(HBTrain, NITrain)


HBTest <- subset(test, test$State == "HB, NI")
NITest <- subset(test, test$State == "HB, NI")
HBTest$State <- 'HB'
NITest$State <- 'NI'
doubleStatesTest <- rbind(HBTest, NITest)

train <- subset(train, train$State != "HB, NI")
test <- subset(test, test$State != "HB, NI")

train <- rbind(train, doubleStatesTrain)
test <- rbind(test, doubleStatesTest)

#add competition feature
train$Competition <- (sqrt(max(train$CompetitionDistance, na.rm = TRUE) - train$CompetitionDistance)) * 
  (((train$Year - train$CompetitionOpenSinceYear) * 12) - (train$CompetitionOpenSinceMonth-train$MonthOfYear)) 
test$Competition <- (sqrt(max(test$CompetitionDistance, na.rm = TRUE) - test$CompetitionDistance))* 
  (((test$Year - test$CompetitionOpenSinceYear) * 12) - (test$CompetitionOpenSinceMonth-test$MonthOfYear))

#add information about weather to all train and test data sets
train <- merge(train, weather, by = c('State', 'Date'), all.x = TRUE, all.y = FALSE)
###############################doubleStatesTrain <- merge(doubleStatesTrain, weather, by = c('State', 'Date'), all.x = TRUE, all.y = FALSE)

test <- merge(test, weather, by = c('State', 'Date'), all.x = TRUE, all.y = FALSE)
###############################doubleStatesTest <- merge(doubleStatesTest, weather, by = c('State', 'Date'), all.x = TRUE, all.y = FALSE)

#add flag to check if that date is a holiday
holidays <- subset(holidays, select = c('Date', 'HolidayName', 'HolidayType', 'ObservedIn'))
train <- merge(train, holidays, by = 'Date', all.x = TRUE, all.y = FALSE)
train$isHoliday <- mapply(grepl, train$State, train$ObservedIn)
train$isHoliday <- ifelse(train$ObservedIn == 'All', TRUE, train$isHoliday)
train$isHoliday <- as.numeric(train$isHoliday)
train$isHoliday[is.na(train$isHoliday)] <- 0

test <- merge(test, holidays, by = 'Date', all.x = TRUE, all.y = FALSE)
test$isHoliday <- mapply(grepl, test$State, test$ObservedIn)
test$isHoliday <- ifelse(test$ObservedIn == 'All', TRUE, test$isHoliday)
test$isHoliday <- as.numeric(test$isHoliday)
test$isHoliday[is.na(test$isHoliday)] <- 0

###############################doubleStatesTrain <- merge(doubleStatesTrain, holidays, by = 'Date', all.x = TRUE, all.y = FALSE)
###############################doubleStatesTrain$isHoliday <- mapply(grepl, doubleStatesTrain$State, doubleStatesTrain$ObservedIn)
###############################doubleStatesTrain$isHoliday <- ifelse(doubleStatesTrain$ObservedIn == 'All', TRUE, doubleStatesTrain$isHoliday)
###############################doubleStatesTrain$isHoliday <- as.numeric(doubleStatesTrain$isHoliday)
###############################doubleStatesTrain$isHoliday[is.na(doubleStatesTrain$isHoliday)] <- 0

###############################doubleStatesTest <- merge(doubleStatesTest, holidays, by = 'Date', all.x = TRUE, all.y = FALSE)
###############################doubleStatesTest$isHoliday <- mapply(grepl, doubleStatesTest$State, doubleStatesTest$ObservedIn)
###############################doubleStatesTest$isHoliday <- ifelse(doubleStatesTest$ObservedIn == 'All', TRUE, doubleStatesTest$isHoliday)
###############################doubleStatesTest$isHoliday <- as.numeric(doubleStatesTest$isHoliday)
###############################doubleStatesTest$isHoliday[is.na(doubleStatesTest$isHoliday)] <- 0

#add information about geo 
train <- merge(train, state_stats, by = 'State', all.x = TRUE, all.y = FALSE)
train$Area[train$State == "HB, NI"] <- state_stats$Area[state_stats$State == 'HB'] + state_stats$Area[state_stats$State == 'NI']
###############################train$Population[train$State == "HB, NI"] <- state_stats$Population[state_stats$State == 'HB'] + state_stats$Population[state_stats$State == 'NI']
###############################train$Density[train$State == "HB, NI"] <- state_stats$Density[state_stats$State == 'HB'] + state_stats$Density[state_stats$State == 'NI']
###############################train$GDP[train$State == "HB, NI"] <- state_stats$GDP[state_stats$State == 'HB'] + state_stats$GDP[state_stats$State == 'NI']

test <- merge(test, state_stats, by = 'State', all.x = TRUE, all.y = FALSE)
###############################test$Area[test$State == "HB, NI"] <- state_stats$Area[state_stats$State == 'HB'] + state_stats$Area[state_stats$State == 'NI']
###############################test$Population[test$State == "HB, NI"] <- state_stats$Population[state_stats$State == 'HB'] + state_stats$Population[state_stats$State == 'NI']
###############################test$Density[test$State == "HB, NI"] <- state_stats$Density[state_stats$State == 'HB'] + state_stats$Density[state_stats$State == 'NI']
###############################test$GDP[test$State == "HB, NI"] <- state_stats$GDP[state_stats$State == 'HB'] + state_stats$GDP[state_stats$State == 'NI']

#remove columns that would not be used
train[,c('Date', 'Customers', 'StartOfMonth', 'EndOfMonth',
         'MonthOfYear.y', 'Year.y', 'StartOfQuarter', 'EndOfQuarter', 'HolidayName', 
         'ObservedIn', 'Max_TemperatureC', 'Min_TemperatureC','Dew_PointC', 'Min_DewpointC', 
         'Max_Humidity', 'Min_Humidity', 'Max_Sea_Level_PressurehPa', 'Min_Sea_Level_PressurehPa', 
         'Max_VisibilityKm', 'Mean_VisibilityKm','Min_VisibilitykM', 'Max_Wind_SpeedKm_h',
         'Max_Gust_SpeedKm_h', 'WindDirDegrees', 'CloudCover') := NULL]
test [, c('Date', 'StartOfMonth', 'EndOfMonth',
          'MonthOfYear.y', 'Year.y', 'StartOfQuarter', 'EndOfQuarter', 
          'HolidayName', 'ObservedIn', 'Max_TemperatureC', 'Min_TemperatureC',
          'Dew_PointC', 'Min_DewpointC', 'Max_Humidity', 'Min_Humidity', 
          'Max_Sea_Level_PressurehPa', 'Min_Sea_Level_PressurehPa', 
          'Max_VisibilityKm', 'Mean_VisibilityKm','Min_VisibilitykM', 
          'Max_Wind_SpeedKm_h', 'Max_Gust_SpeedKm_h', 'WindDirDegrees', 'CloudCover') := NULL]

#Impute all NA values
train$HolidayType[is.na(train$HolidayType)] <- 'None'
train$Events[is.na(train$Events) | train$Events == ""] <- 'None'
test$Events[is.na(test$Events) | test$Events == ""] <- 'None'
test$HolidayType[is.na(test$HolidayType)] <- 'None'

write.csv(train, 'data/preProcessedTrain.csv', row.names = FALSE)
write.csv(test, 'data/preProcessedTest.csv', row.names = FALSE)
write.csv(storeClosedTest, 'data/storeClosedTest.csv', row.names = FALSE)