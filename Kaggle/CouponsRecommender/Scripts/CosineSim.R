#read in all the input data
cpdtr <- read.csv("/Users/Jaan/Documents/KaggleCompetitions/CouponsRecommender/Data/coupon_detail_train.csv")
cpltr <- read.csv("/Users/Jaan/Documents/KaggleCompetitions/CouponsRecommender/Data/coupon_list_train.csv")
cplte <- read.csv("/Users/Jaan/Documents/KaggleCompetitions/CouponsRecommender/Data/coupon_list_test.csv")
ulist <- read.csv("/Users/Jaan/Documents/KaggleCompetitions/CouponsRecommender/Data/user_list.csv")

#making of the train set
train <- merge(cpdtr,cpltr)
train <- train[,c("COUPON_ID_hash","USER_ID_hash",
                  "GENRE_NAME","DISCOUNT_PRICE",
                  "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                  "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                  "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name")]
#combine the test set with the train
cplte$USER_ID_hash <- "dummyuser"
cpchar <- cplte[,c("COUPON_ID_hash","USER_ID_hash",
                   "GENRE_NAME","DISCOUNT_PRICE",
                   "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
                   "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
                   "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name")]

train <- rbind(train,cpchar)
str(train)
#NA imputation
train[is.na(train)] <- 1
#feature engineering
train$DISCOUNT_PRICE <- 1/log10(train$DISCOUNT_PRICE)
#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
      contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], 
      is.factor)==TRUE))], contrasts, contrasts=FALSE)))

#separate the test from train
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
train <- train[train$USER_ID_hash!="dummyuser",]

#data frame of user characteristics
uchar <- aggregate(.~USER_ID_hash, data=train[,-1],FUN=mean)
uchar$DISCOUNT_PRICE <- 1

#Weight Matrix: GENRE_NAME DISCOUNT_PRICE USABLE_DATE_ ken_name small_area_name
require(Matrix)
W <- as.matrix(Diagonal(x=c(rep(1,13), rep(1,1), rep(0,9), rep(1,47), rep(0.01,55))))

#calculation of cosine similairties of users and coupons
score = as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))
#order the list of coupons according to similairties and take only first 10 coupons
uchar$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
  purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
  return(purchased_cp)
}))

#make submission
submission <- merge(ulist, uchar, all.x=TRUE)
submission <- submission[,c("USER_ID_hash","PURCHASED_COUPONS")]
write.csv(submission, file="cosine_sim.csv", row.names=FALSE)