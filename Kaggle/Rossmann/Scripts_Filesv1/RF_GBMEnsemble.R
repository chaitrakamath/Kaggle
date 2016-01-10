rm(list = ls())
#read data
setwd('/Users/Jaan/Documents/KaggleCompetitions/Rossmann')
rf <- fread('h2oRFSubmission.csv')
gbm <- fread('h2oGBMSubmission.csv')

rf <- rf[with(rf, sort(rf$Id)), ]
gbm <- gbm[with(gbm, sort(gbm$Id)), ]

finalSub <- data.frame(Id = rf$Id)
finalSub$Sales <- (rf$Sales + gbm$Sales) / 2

write.csv(finalSub, 'rf_gbmEnsemble.csv', row.names = FALSE)
