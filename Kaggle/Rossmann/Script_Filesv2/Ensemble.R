setwd('/Users/Jaan/Documents/KaggleCompetitions/Rossmann/Script_Filesv2')
rm(list = ls())
rf_predictions <- fread('h2oRFSubmission.csv') #0.13486
rf_predictions <- rf_predictions[with(rf_predictions, order(Id)), ]
gbm_predictions <- fread('h2oGBMSubmission.csv')
gbm_predictions <- gbm_predictions[with(gbm_predictions, order(Id)), ]
lm_predictions <- fread('h2oClusteredLM.csv')
lm_predictions <- lm_predictions[with(lm_predictions, order(Id)), ]
xgb_predictions <- fread('xgb.csv')
xgb_predictions <- xgb_predictions[with(xgb_predictions, order(Id)), ]

finalPredictions <- (rf_predictions$Sales + gbm_predictions$Sales + xgb_predictions$Sales) / 3
submission <- data.frame(Id = rf_predictions$Id, Sales = finalPredictions)
write.csv(submission, 'ensemble.csv', row.names = FALSE)
