getwd()
setwd('/Users/Jaan/Documents/KaggleCompetitions/Happy/')
getwd()

rm(list = ls())
train <- read.csv('train.csv')
test <- read.csv('test.csv')
alcoholConsumption <- read.csv('NewVariable_Alcohol.csv')
str(train)
str(test)
str(alcoholConsumption)

#merge train and test data sets
test$Happy <- NA
allWithoutAlcohol <- rbind(train, test)
all <- merge(allWithoutAlcohol, alcoholConsumption, by.x = 'ID', by.y = 'ID')
str(all)

#MISSING VALUE IMPUTATIONS
#Impute missing WorkStatus with Other
all$WorkStatus <- ifelse(is.na(all$WorkStatus), 'Other', all$WorkStatus)
all$WorkStatus <- as.factor(all$WorkStatus)

#Impute Divorced status as 'Unmarried'
all$Divorce <- as.character(all$Divorce)
all$Divorce[is.na(all$Divorce)] <- 'Unmarried'
all$Divorce <- as.factor(all$Divorce)

#Impute Widowed Status as 'Unmarried'
all$Widowed <- as.character(all$Widowed)
all$Widowed[is.na(all$Widowed)] <-  'Unmarried'
all$Widowed <- as.factor(all$Widowed)

#Impute missing values of Score by predicting on decision tree
library(rpart)
library(rpart.plot)
decisionTree_Score <- rpart(Score ~ Var1 + WorkStatus + Divorce + Widowed + Education + Residence_Region + 
                              babies + preteen + teens + income + Engagement_Religion + Var2 + 
                              Gender + Unemployed10 + Happy, data = all, method = 'anova')
all[is.na(all$Score), 'Score'] <- round(predict(decisionTree_Score, all[is.na(all$Score), ]))

#Impute missing education values by creating decision tree on all other variables
decisionTree_Education <- rpart(Education ~ . - ID, data = all, method = 'anova')
prp(decisionTree_Education)
all$Education[is.na(all$Education)] <- round(predict(decisionTree_Education, all[is.na(all$Education), ]))

#Impute missing value for Var1 based on most frequent value across residence_region
tapply(all$Var1, all$Residence_Region, function(x) names(which.max(table(x))))
all$Var1 <- as.character(all$Var1)
all$Var1[is.na(all$Var1) & all$Residence_Region == 'new england'] <- 'B'
all$Var1[is.na(all$Var1) & all$Residence_Region %in% 
           c('e. sou. central', 'middle atlantic', 'w. nor. central', 'w. sou. central')] <- 'A'
all$Var1[is.na(all$Var1) & all$Residence_Region %in% 
           c ('e. nor. central', 'foreign', 'mountain', 'pacific', 'south atlantic')] <- 'G'
all$Var1 <- as.factor(all$Var1)

#Impute missing number of babies as 0
all$babies[is.na(all$babies)] <- 0

#Impute missing number of preteens and teens as 0
all$preteen[is.na(all$preteen)] <- 0
all$teens[is.na(all$teens)] <- 0

#Impute missing values of income 
tapply(all$income, list(all$Education, all$Residence_Region), function(x) names(which.max(table(x))))

all$income[is.na(all$income) & all$Residence_Region == 'e. nor. central' & all$Education > 7] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'e. sou. central' & all$Education > 11] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'foreign' & all$Education > 1 & all$Education != 4 & all$Education != 7] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region %in% c('middle atlantic', 'south atlantic') & all$Education > 7] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region %in% c('mountain', 'new england') & all$Education > 9] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'pacific' & all$Education > 6] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'w. nor. central' & all$Education > 10] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'w. sou. central' & all$Education > 9] <- '$25000 or more'

tapply(all$income, all$Residence_Region, function(x) (length(x[is.na(x)]))) #look for regions with more missing values left out

all$income[is.na(all$income) & all$Residence_Region == 'south atlantic' & all$Education %in% c(2, 4)] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'south atlantic' & all$Education %in% c(3, 6, 7)] <- '$10000 - 14999'
all$income[is.na(all$income) & all$Residence_Region == 'south atlantic' & all$Education == 5] <- '$8000 to 9999'


all$income[is.na(all$income) & all$Residence_Region == 'w. sou. central' & all$Education %in% c(6, 9)] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'w. sou. central' & all$Education == 3] <- '$10000 - 14999'
all$income[is.na(all$income) & all$Residence_Region == 'w. sou. central' & all$Education == 7] <- '$7000 to 7999'
all$income[is.na(all$income) & all$Residence_Region == 'w. sou. central' & all$Education == 8] <- '$8000 to 9999'

all$income[is.na(all$income) & all$Residence_Region == 'e. sou. central' & all$Education %in% c(7, 10)] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'e. sou. central' & all$Education %in% c(0, 6)] <- '$6000 to 6999'
all$income[is.na(all$income) & all$Residence_Region == 'e. sou. central' & all$Education %in% c(4, 9, 8, 11)] <- '$10000 - 14999'
all$income[is.na(all$income) & all$Residence_Region == 'e. sou. central' & all$Education == 3] <- '$5000 to 5999'

all$income[is.na(all$income) & all$Residence_Region == 'foreign' & all$Education == 0] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'foreign' & all$Education == 4] <- '$10000 - 14999'
all$income[is.na(all$income) & all$Residence_Region == 'foreign' & all$Education == 7] <- '$20000 - 24999'


all$income[is.na(all$income) & all$Residence_Region == 'new england' & all$Education %in% c(7, 8) ] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'new england' & all$Education == 5] <- '$5000 to 5999'
all$income[is.na(all$income) & all$Residence_Region == 'new england' & all$Education == 9] <- '$10000 - 14999'

all$income[is.na(all$income) & all$Residence_Region == 'w. nor. central' & all$Education %in% c(10, 8) ] <- '$10000 - 14999'
all$income[is.na(all$income) & all$Residence_Region == 'w. nor. central' & all$Education == 6 ] <- '$5000 to 5999'
all$income[is.na(all$income) & all$Residence_Region == 'w. nor. central' & all$Education == 9 ] <- '$25000 or more'


all$income[is.na(all$income) & all$Residence_Region == 'middle atlantic' & all$Education ==  2] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'middle atlantic' & all$Education ==  3] <- 'lt $1000'
all$income[is.na(all$income) & all$Residence_Region == 'middle atlantic' & all$Education ==  6] <- '$10000 - 14999'
all$income[is.na(all$income) & all$Residence_Region == 'middle atlantic' & all$Education ==  7] <- '$15000 - 19999'

all$income[is.na(all$income) & all$Residence_Region == 'mountain' & all$Education ==  8] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'mountain' & all$Education ==  5] <- '$1000 to 2999'

all$income[is.na(all$income) & all$Residence_Region == 'pacific' & all$Education ==  2] <- '$25000 or more'
all$income[is.na(all$income) & all$Residence_Region == 'pacific' & all$Education ==  6] <- '$10000 - 14999'

all$income[is.na(all$income) & all$Residence_Region == 'e. nor. central' & all$Education %in% c(5, 6)] <- '$25000 or more'
all$income <- as.factor(all$income)

#impute engagement_religion with most frequent value: never
all$Engagement_Religion[is.na(all$Engagement_Religion)] <- 'never'
all$Engagement_Religion <- as.factor(all$Engagement_Religion)

#impute TVhours using decision tree
decisionTree_tvhours <- rpart(TVhours ~ . - ID, data = all, method = 'anova')
prp(decisionTree_tvhours)
all$TVhours[is.na(all$TVhours)] <- round(predict(decisionTree_tvhours, newdata = all[is.na(all$TVhours), ]))

#impute unemployed using decision tree
decisionTree_unemployed <- rpart(Unemployed10 ~ . - ID, data = all)
prp(decisionTree_unemployed)
unemp_prob <- predict(decisionTree_unemployed, newdata = all[is.na(all$Unemployed10), ])
all$Unemployed10[is.na(all$Unemployed10)] <- ifelse(unemp_prob < 0.5, 0, 1)

#impute alcohol consumption using decision tree
decisionTree_alcohol <- rpart(Alcohol_Consumption ~ . - ID, data = all)
prp(decisionTree_alcohol)
all$Alcohol_Consumption[all$Alcohol_Consumption == ""] <- predict(decisionTree_alcohol, newdata = all[all$Alcohol_Consumption == "", ], type = 'class')

#Convert all factor variables to dummy variables
dummies <- model.matrix(~ Var1 + WorkStatus + Divorce + Widowed + income + Engagement_Religion + Alcohol_Consumption - 1, 
                        data = all)

allData <- cbind(all[, sapply(all, function(x) !is.factor(x))], dummies, Happy = all$Happy)

#Create column names
colnames(allData) <- make.names(colnames(allData))

#split train and test data
train <- subset(allData, !is.na(all$Happy))
test <- subset(allData, is.na(all$Happy))
test$Happy <- NULL

#split train and test without dummies
trainWithoutDummies <- subset(all, !is.na(all$Happy))
testWithoutDummies <- subset(all, is.na(all$Happy))
test$Happy <- NULL


#Create correlation matrix
library(caret)
train_dummies <- model.matrix(~ Happy - 1, data = train)
allTrainDummies <- cbind(train, train_dummies)
allTrainDummies$Happy <- NULL


#simple rf model
library(randomForest)
trainWithoutDummies$Unemployed10 <- as.factor(trainWithoutDummies$Unemployed10)
testWithoutDummies$Unemployed10 <- as.factor(testWithoutDummies$Unemployed10)

tuneRF(x = trainWithoutDummies[, colnames(trainWithoutDummies) %in% c('Alcohol_Consumption', 
                                                                      'Divorce', 'WorkStatus', 'income', 'Var2', 'Unemployed10', 'Widowed')], 
       y = trainWithoutDummies$Happy, ntreeTry = 1000)
rfModel <- randomForest(Happy ~ Alcohol_Consumption * Divorce * WorkStatus * income * Var2 * Unemployed10 * Widowed, data = trainWithoutDummies, mtry = 2, 
                        ntree = 1000)
rfModel2 <- randomForest(Happy ~ Alcohol_Consumption * Divorce * WorkStatus * income * Var2 * Unemployed10 * Widowed, data = trainWithoutDummies, 
                         mtry = 2, ntree = 1000, cutoff = c(0.1, 0.7, 0.2), importance = TRUE)
rfClasses <- predict(rfModel2, newdata = testWithoutDummies[, colnames(testWithoutDummies) %in% 
                    c('Alcohol_Consumption', 'Divorce', 'WorkStatus', 'income', 'Var2', 'Unemployed10', 'Widowed')], type = 'class')

rfModel3 <- randomForest(Happy ~ Alcohol_Consumption + Divorce + WorkStatus + income + Var2 + Unemployed10 + Widowed, data = trainWithoutDummies, 
                         mtry = 2, ntree = 1000)
rfClasses <- predict(rfModel3, newdata = testWithoutDummies[, colnames(testWithoutDummies) %in% 
    c('Alcohol_Consumption', 'Divorce', 'WorkStatus', 'income', 'Var2', 'Unemployed10', 'Widowed')], type = 'class')


#submission file
submission <- data.frame(ID = test$ID, Happy = as.factor(rfClasses))
write.csv(submission, 'rfModel.csv', row.names = FALSE)


#simple multinomial model
library(nnet)
logModel <- multinom(Happy ~ Alcohol_Consumption * Divorce * WorkStatus * income * Var2 * Unemployed10 * Widowed, data = trainWithoutDummies)
logProb <- as.data.frame(predict(logModel, newdata = test, type = 'probs'))
logClasses <- predict(logModel, newdata = test)
logitDF <- data.frame(ID = test$ID, Happy = logClasses)
write.csv(logitDF, 'multinomialClass.csv', row.names = FALSE)