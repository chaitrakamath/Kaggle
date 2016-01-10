cpdtr <- read.csv("/Users/Jaan/Documents/KaggleCompetitions/CouponsRecommender/Data/coupon_detail_train.csv")
str(cpdtr)

#Remove columns that are not to be used
cpdtr$I_DATE = NULL
cpdtr$SMALL_AREA_NAME = NULL
cpdtr$PURCHASEID_hash = NULL
str(cpdtr)

#Rearrange columns and remove dups, if any
cpdtr <- cpdtr[c(2, 3, 1)]
cpdtrUnique <- unique(cpdtr)
str(cpdtrUnique)

#Convert data frame into matrix
 
itemCnt.mat <- with (cpdtrUnique, {
  out <- matrix(nrow = nlevels(USER_ID_hash), ncol = nlevels(COUPON_ID_hash), 
                dimnames = list(levels(USER_ID_hash), levels(COUPON_ID_hash)))
  out[cbind(USER_ID_hash, COUPON_ID_hash)] <- ITEM_COUNT
  out
})
str(itemCnt.mat)
head(itemCnt.mat)
