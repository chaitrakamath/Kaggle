library(data.table)
library(bit64)
library(h2o)
library(randomForest)
library(dummies)
library(ggplot2)
library(plyr)
rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Walmart')
train <- fread('data/train.csv')
test <- fread('data/test.csv')

#explore a bit
str(train)
str(test)
head(train)
head(test)
names(train)
names(test)

sapply(train, function(x) any(is.na(x)))
sapply(test, function(x) any(is.na(x)))
sapply(train, function(x) any(is.nan(x)))
sapply(test, function(x) any(is.nan(x)))
sapply(train, function(x) any(is.infinite(x)))
sapply(test, function(x) any(is.infinite(x)))

#convert character columns to factors
train$Weekday <- as.factor(train$Weekday)
train$DepartmentDescription <- as.factor(train$DepartmentDescription)
test$Weekday <- as.factor(test$Weekday)
test$DepartmentDescription <- as.factor(test$DepartmentDescription)

#impute missing values
train$FinelineNumber[is.na(train$FinelineNumber)] <- -1000
train$Upc[is.na(train$Upc)] <- 1000000000000
test$FinelineNumber[is.na(test$FinelineNumber)] <- -1000
test$Upc[is.na(test$Upc)] <- 1000000000000

#convert output column to factors
train$TripType <- as.factor(train$TripType)

## Start cluster with all available threads
h2o.init(nthreads=-1,min_mem_size = '6G', max_mem_size='8G', assertion = FALSE)

## Load data into cluster from R
trainHex<-as.h2o(train)

## Set up variable to use all features other than those specified here
features <- colnames(train)[(colnames(train) != 'TripType')]

## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="TripType",
                          ntrees = 100,
                          max_depth = 50,
                          training_frame=trainHex,
                          seed = 12345678 #Seed for random numbers (affects sampling)
)
h2o.varimp(rfHex)

testHex <- as.h2o(test)
predictions <- as.data.frame(predict(rfHex, testHex))
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
write.csv(sub, 'h2oRF1_WithMeanProb.csv', row.names = FALSE)
h2o.shutdown()

#EDA
barplot(table(train$Weekday))

barplot(table(train$TripType))
x <- prop.table(table(train$TripType, train$DepartmentDescription))
apply(x, 2, function(y) names(which.max(y)))

departments <- names(apply(x, 2, function(y) names(which.max(y))))
tripType <- as.numeric(apply(x, 2, function(y) names(which.max(y))))
dept_tripType <- data.frame(department = departments, TripType = tripType)

ggplot(train, aes(x = TripType, y = VisitNumber)) + 
  geom_point(pch = 15) + geom_line() + theme_bw()

ggplot(train, aes(x = TripType, y = DepartmentDescription)) + 
  geom_point(pch = 15) + geom_line() + theme_bw() 

ggplot(train, aes(x = TripType, y = Weekday)) + 
  geom_point(pch = 15) + geom_line() + theme_bw()

ggplot(train, aes(x = TripType, y = ScanCount)) + 
  geom_point(pch = 15) + geom_line() + theme_bw() 

ggplot(train, aes(x = Weekday, y = ScanCount)) + 
  geom_point(pch = 15) + geom_line() + theme_bw()

ggplot(train, aes(x = DepartmentDescription, y = ScanCount)) + 
  geom_point(pch = 15) + geom_line() + theme_bw()

#simple model based on most frequent trip type by department
test <- merge(test, dept_tripType, by.x = 'DepartmentDescription', 
              by.y = 'department', all.x = TRUE, all.y = FALSE)

submission <- subset(test, select = c('VisitNumber', 'TripType'))
sort(unique(submission$TripType))
submission <- unique(submission)
submission$TripType <- as.factor(submission$TripType)
submission <- dummy.data.frame(submission)
colnames(submission) <- gsub('TripType', 'TripType_', colnames(submission))
submission$TripType_4 <- 0
submission$TripType_8 <- 0
submission$TripType_12 <- 0
submission$TripType_14 <- 0
submission$TripType_19 <- 0
submission$TripType_29 <- 0
submission$TripType_33 <- 0
submission$TripType_34 <- 0
submission$TripType_35 <- 0
submission$TripType_37 <- 0
submission$TripType_38 <- 0
submission$TripType_39 <- 0
submission$TripType_41 <- 0
submission$TripType_43 <- 0
submission$TripType_44 <- 0

sub <- ddply(submission, 'VisitNumber', numcolwise(sum))
sub <- sub[order(sub$VisitNumber), ]
write.csv(sub, 'benchmarkModel.csv', row.names = FALSE)


#simple model based on most frequent trip type by department and weekday
deptWeekday_tripType <- as.data.table(prop.table(table(train$TripType, train$DepartmentDescription, train$Weekday)))
colnames(deptWeekday_tripType) <- c('TripType', 'DepartmentDescription', 'Weekday', 'Proportion')
deptWeekday_tripType[, predictedTripType := TripType[which.max(Proportion)], by = c('DepartmentDescription', 'Weekday')]
deptWeekday_tripType[, c('Proportion', 'TripType') := NULL]
deptWeekday_tripType <- unique(deptWeekday_tripType)

dt <- merge(test, deptWeekday_tripType, by = c('DepartmentDescription', 'Weekday'), 
            all.x = TRUE, all.y = FALSE)
submission <- subset(dt, select = c('VisitNumber', 'predictedTripType'))
names(submission) <- c('VisitNumber', 'TripType')
sort(unique(submission$TripType))
sort(unique(train$TripType))
submission <- unique(submission)
submission$TripType <- as.factor(submission$TripType)
submission <- dummy.data.frame(submission)
colnames(submission) <- gsub('TripType', 'TripType_', colnames(submission))
submission$TripType_4 <- 0
submission$TripType_8 <- 0
submission$TripType_12 <- 0
submission$TripType_14 <- 0
submission$TripType_29 <- 0
submission$TripType_33 <- 0
submission$TripType_35 <- 0
submission$TripType_41 <- 0
submission$TripType_43 <- 0

sub <- ddply(submission, 'VisitNumber', numcolwise(sum))
sub <- sub[order(sub$VisitNumber), ]
write.csv(sub, 'TripTypeByDeptWeekday.csv', row.names = FALSE)
