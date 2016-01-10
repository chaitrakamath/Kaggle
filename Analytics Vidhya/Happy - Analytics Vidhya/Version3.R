getwd()
setwd('/Users/Jaan/Documents/KaggleCompetitions/Happy/')
getwd()

rm(list = ls())
train <- read.csv('train.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv')
alcoholConsumption <- read.csv('NewVariable_Alcohol.csv', stringsAsFactors = FALSE)
str(train)
str(test)
str(alcoholConsumption)

#merge train and test data sets
test$Happy <- NA
allWithoutAlcohol <- rbind(train, test)
all <- merge(allWithoutAlcohol, alcoholConsumption, by.x = 'ID', by.y = 'ID')
str(all)

all$Alcohol_Consumption[is.na(all$Alcohol_Consumption)] <- 'dummy'
all$Divorce[is.na(all$Divorce)] <- 'dummy'
all$WorkStatus[is.na(all$WorkStatus)] <- 'dummy'
all$income[is.na(all$income)] <- 'dummy'
all$Var2[is.na(all$Var2)] <- 'dummy'
all$Widowed[is.na(all$Widowed)] <- 'dummy'
all$Unemployed10[is.na(all$Unemployed10)] <- 'dummy'

all$Alcohol_Consumption <- as.factor(all$Alcohol_Consumption)
all$Divorce <- as.factor(all$Divorce)
all$WorkStatus <- as.factor(all$WorkStatus)
all$income <- as.factor(all$income)
all$Var2 <- as.factor(all$Var2)
all$Widowed <- as.factor(all$Widowed)
all$Unemployed10 <- as.factor(all$Unemployed10)

#split train and test
train <- subset(all, !is.na(all$Happy))
train$Happy <- as.factor(train$Happy)
test <- subset(all, is.na(all$Happy))


#simple rf model
library(randomForest)
rfTrain <- train[, colnames(train) %in% c('Alcohol_Consumption', 
              'Divorce', 'WorkStatus', 'income', 'Var2', 'Unemployed10', 'Widowed')]
rfTest <- test[, colnames(test) %in% c('Alcohol_Consumption', 
                                        'Divorce', 'WorkStatus', 'income', 'Var2', 'Unemployed10', 'Widowed')]
tuneRF(x = rfTrain , 
       y = train$Happy, ntreeTry = 1000)
rfModel <- randomForest(x = rfTrain, y = train$Happy, mtry = 1, ntree = 1000)
print(rfModel)
rfClasses <- predict(rfModel, newdata = rfTest, type = 'class')
submission <- data.frame(ID = test$ID, Happy = rfClasses)
write.csv(submission, 'rfModel.csv', row.names = FALSE)
