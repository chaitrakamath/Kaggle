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


#simple rf model
library(randomForest)
tuneRF(x = train[, !(colnames(train) %in% c('Happy', 'ID'))], y = train[, c('Happy')], ntreeTry = 5000)
rfModel <- randomForest(Happy ~ . - ID, data = train , mtry = 4, maxnodes = 1000, nodesize = 1)
rfPredictions <- as.data.frame(predict(rfModel, newdata = test, type = 'prob'))
rfClasses <- predict(rfModel, newdata = test, type = 'response')
rfDF <- data.frame(ID = test$ID, Happy = rfClasses)
write.csv(rfDF, 'randomForestModel.csv', row.names = FALSE)


#simple xgboost model (cross validated)
train.copy <- train
library(xgboost)
library(Matrix)
library(caret)
train_full_spare <- sparse.model.matrix(Happy ~ . - ID - 1, data = train)
xgbtrain <- xgb.DMatrix(data = train_full_spare, label = as.numeric(train$Happy))
test_full_sparse <- sparse.model.matrix(~. - 1, data = test)
xgbtest <- xgb.DMatrix(data = test_full_sparse)

param1 <- list(objective = 'multi:softmax', num_class = 4, booster = 'gbtree', 
               eta = 0.005, min_child_weight = 10, subsample = 0.7, 
               colsample_bytree = 0.6, scale_pos_weight = 0.8, silent = 0, 
               max_depth = 5, max_delta_step = 2, seed = 0)
model1 <- xgb.train(params = param1, xgbtrain, nrounds = 1100)
xgbPredictions <- predict(model1, newdata = as.matrix(test))
xgbClasses <- rep(NA, length(test$ID))
xgbClasses[xgbPredictions == 1] <- 'Not Happy'
xgbClasses[xgbPredictions == 2] <- 'Pretty Happy'
xgbClasses[xgbPredictions == 3] <- 'Very Happy'
xgbDF <- data.frame(ID = test$ID, Happy = xgbClasses)
write.csv(xgbDF, 'xgboostModel.csv', row.names = FALSE)

param2 <- list(objective = 'multi:softprob', num_class = 4, booster = 'gbtree', 
               eta = 0.005, min_child_weight = 10, subsample = 0.7, 
               colsample_bytree = 0.6, scale_pos_weight = 0.8, silent = 0, 
               max_depth = 5, max_delta_step = 2)
model2 <- xgb.train(params = param2, xgbtrain, nrounds = 1100)
xgbProbabilities <- predict(model2, newdata = as.matrix(test))
rows <- 3 * nrow(test)
xgbProbMat <- matrix(xgbProbabilities, nrow = nrow(test), ncol = 4, byrow = TRUE)
xgbProbDF <- as.data.frame(xgbProbMat)
xgbProbDF$Happy <- apply(xgbProbDF, 1, function(x) names(which.max(x)))
xgbProbDF$Class <- rep(NA, nrow(test))
xgbProbDF$Class[xgbProbDF$Happy == 'V2'] <- 'Not Happy'
xgbProbDF$Class[xgbProbDF$Happy == 'V3'] <- 'Pretty Happy'
xgbProbDF$Class[xgbProbDF$Happy == 'V4'] <- 'Very Happy'


xgbClasses <- rep(NA, length(test$ID))
xgbClasses[xgbPredictions == 1] <- 'Not Happy'
xgbClasses[xgbPredictions == 2] <- 'Pretty Happy'
xgbClasses[xgbPredictions == 3] <- 'Very Happy'
xgbDF <- data.frame(ID = test$ID, Happy = xgbClasses)
write.csv(xgbDF, 'xgboostModel.csv', row.names = FALSE)


param2 <- list(objective = 'multi:softmax', num_class = 4, booster = 'gbtree', 
               eta = 0.005, min_child_weight = 6, subsample = 0.7, 
               colsample_bytree = 0.6, scale_pos_weight = 0.8, silent = 0, 
               max_depth = 8, max_delta_step = 2, seed = 10)
model2 <- xgb.train(params = param2, xgbtrain, nrounds = 800)
xgbPredictions <- predict(model2, newdata = xgbtest)
xgbClasses <- rep(NA, length(test$ID))
xgbClasses[xgbPredictions == 1] <- 'Not Happy'
xgbClasses[xgbPredictions == 2] <- 'Pretty Happy'
xgbClasses[xgbPredictions == 3] <- 'Very Happy'
xgbDF <- data.frame(ID = test$ID, Happy = xgbClasses)
write.csv(xgbDF, 'xgboostModel.csv', row.names = FALSE)

#simple multinomial model
library(nnet)
logModel <- multinom(Happy ~ . - ID, data = train)
logProb <- as.data.frame(predict(logModel, newdata = test, type = 'probs'))
logClasses <- predict(logModel, newdata = test)
logitDF <- data.frame(ID = test$ID, Happy = logClasses)
write.csv(logitDF, 'multinomialClass.csv', row.names = FALSE)

#Average output of multinomial model and rfmodel
xgb <- xgbProbDF
xgb$V1 <- NULL
xgb$Happy <- NULL
xgb$Class <- NULL
colnames(xgb) <- c('Not Happy', 'Pretty Happy', 'Very Happy')
ensembleProb <- (rfPredictions + logProb + xgb) / 3
colnames(ensembleProb) <- make.names(colnames(ensembleProb))
ensembleClasses <- cbind(ensembleProb, maxValue = apply(ensembleProb, 1, max), 
                    Happy = names(ensembleProb)[apply(ensembleProb, 1, which.max)])
ensembleClasses$Happy <- as.character(ensembleClasses$Happy)
ensembleClasses$Happy[ensembleClasses$Happy == 'Not.Happy'] <- 'Not Happy'
ensembleClasses$Happy[ensembleClasses$Happy == 'Pretty.Happy'] <- 'Pretty Happy'
ensembleClasses$Happy[ensembleClasses$Happy == 'Very.Happy'] <- 'Very Happy'

ensembleDF <- data.frame(ID = test$ID, Happy = ensembleClasses$Happy)
write.csv(ensembleDF, 'logRFEnsemble.csv', row.names = FALSE)
