rm(list = ls())
setwd('/Users/Jaan/Documents/KaggleCompetitions/Titanic')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

#combine train and test
test$Survived <- 'dummy'
data <- rbind(train, test)

#Feature engg: create new interaction feature
data <- cbind(data, model.matrix(~ (Sex +SibSp + Parch)^2 - 1, data = data))

#Feature engg: add feature to extract number of characters in full name
data$NameCount <- nchar(as.character(data$Name))

#Feature engg: add feature extracting last name 
data$lastName <- sapply(strsplit(as.character(data$Name), ', '), '[', 1)

#Feature engg: add age_estimated flag column
pattern <- '\\d+.5'
data$ageEstFlag <- ifelse(grepl(pattern, data$Age), 1, 0)

#Scale fare column using modified adjusted score
data$AdjFare <- (data$Fare - median(data$Fare, na.rm = TRUE)) / sd(data$Fare, na.rm = TRUE)
data$Fare <- NULL

#Convert all factor variables to dummy variables
library(caret)

dmy1 <- dummyVars(~ Sex, data = data, fullRank = T)
sex <- predict(dmy1, newdata = data)

dmy2 <- dummyVars(~ Cabin, data = data, fullRank = T)
cabin <- predict(dmy2, newdata = data)

dmy3 <- dummyVars(~ Embarked, data = data, fullRank = T)
embarked<- predict(dmy3, newdata = data)


fullData <- cbind(data, sex,cabin, embarked)

#impute missing values in age and adjusted fare to 0
fullData[is.na(fullData$Age), 'Age'] <- -1
fullData[is.na(fullData$AdjFare), 'AdjFare'] <- median(fullData$AdjFare, na.rm = TRUE)

#remove columns not needed
fullData$Pclass <- NULL
fullData$Ticket <- NULL
fullData$Sex <- NULL
fullData$Cabin <- NULL
fullData$Embarked <- NULL
fullData$lastName <- NULL

#feature selection

#a) Zero Variances
zero.var = nearZeroVar(fullData, saveMetrics=TRUE)
print (zero.var[zero.var$nzv, ])

#remove all zero variance columns
colNums <- as.numeric(rownames(zero.var[zero.var$nzv, ]))
fullData <- fullData[, - colNums]

#b) Build a lvq model
colnames(fullData) <- make.names(colnames(fullData), unique = TRUE)
fullData$Survived <- as.factor(fullData$Survived)

library(caret)
library(mlbench)
library(e1071)
library(pROC)
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
model <- train(Survived ~. - (PassengerId), data=fullData, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)

#Separate test and train data sets
train <- subset(fullData, fullData$Survived != 'dummy')
test <- subset(fullData, fullData$Survived == 'dummy')

train$Survived <- as.factor(train$Survived)
test$Survived <- NULL

#--------------------Simple classifier------------
#Create a simple classification model and make predictions
logModel <- glm(Survived ~ . - (PassengerId + Name), data = train, family = binomial)
logModel$xlevels[['Name']] <- union(logModel$xlevels[['Name']], levels(as.factor(test$Name)))
logPred <- predict(logModel, newdata = test, type = 'response')
output <- ifelse(logPred < 0.5, 0, 1)

#Create submission file
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- output
#Gave score of 0.76077
write.csv(submission, 'LogModel.csv', row.names = FALSE)

#-----------------glmnet--------------------------
#Create a glmnet model
library(glmnet)
train$Survived <- as.numeric(train$Survived)
glmModel <- cv.glmnet(x = data.matrix(train[, - c(1, 2, 3)]), y = train[, c('Survived')], family = 'binomial', 
                      type.measure = 'auc')
cv <- cv.glmnet(x = data.matrix(train[, - c(1, 2, 3)]), y = train[, c('Survived')], nfolds = 20)
glmPred <- data.frame(predict(glmModel, newx = data.matrix(test[, - c(1, 2)]), type = 'response', s = cv$lambda.min))
output <- ifelse(glmPred$X1 < 0.5, 0, 1)

#Create submission file
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- output
#gave a score of 0.77512
write.csv(submission, 'GLMNet.csv', row.names = FALSE)


#---------------random forest---------------
library(randomForest)
train$Survived <- as.factor(train$Survived)
x <- train[, -c(1, 2, 3)]
y <- train$Survived
colnames(train) <- make.names(colnames(train), unique = TRUE)
tuneRF(x = x, y = y)
rfModel <- randomForest(Survived ~ .-(PassengerId + Name), data = train, method = 'class')

colnames(test) <- make.names(colnames(test), unique = TRUE)
rfPred <- predict(rfModel, newdata = test)
rfProb <- data.frame(predict(rfModel, newdata = test, type = 'prob'))
library(plyr)
output <- revalue(rfPred, c('1' = '0', '2' = '1'))

#Create submission file
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- output
#gave a score of 0.76555
write.csv(submission, 'RFModel.csv', row.names = FALSE)

#-----------------logModel + Random Forest Ensemble-----------------
predProb <- (logPred + rfProb[, c('X2')])/ 2
output <- ifelse(predProb < 0.5, 0, 1)

#Create submission file
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- output
#gave a score of 0.76555
write.csv(submission, 'RF+LogEnsemble.csv', row.names = FALSE)

#----------------CART----------------------

#-----------------xgboost------------------