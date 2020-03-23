rm(list=objects())

library(tidyverse)
library(lubridate)
library(xts)
library(dygraphs)
library(ranger)
setwd("/Users/Nefzaoui/Documents/SIM202/tp1")

train <- read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')

prediction_xgboost_simple_struct=read.csv(file="submission_simple_xgboost.csv", sep=",", dec='.')
prediction_xgboost_simple=prediction_xgboost_simple_struct$Appliances
length(prediction_xgboost_simple)

vrai_test=test[4655:5771]
past_predictions=prediction_xgboost_simple[4655:5771]
predictions_sures=prediction_xgboost_simple[1:4654]

date_test=as.POSIXct(strptime(test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_vrai_test=date_test[4655:5771]
library(dplyr)

yes_is_in_test=rep(1, times = length(test$date))
test$is_in_test=yes_is_in_test
not_in_test=rep(0, times = length(train$date))
train$is_in_test=not_in_test

rmse = function(ychap, y){
  sqrt(mean((ychap - y)^2))
}




library(mltools)
library(data.table)
library(xgboost)
library(caret)





train_dt=setDT(train, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_sp=test_dt[1:4654]
test_sp$Appliances=prediction_xgboost_simple[1:4654]
vrai_test=test_dt[4655:5771]
total= merge(train_dt,test_sp,all=TRUE)
test_purified=(vrai_test %>% select(-c(Id,is_in_test,rv1,rv2,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                       BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))
test_purified$Day_of_week=as.numeric(test_purified$Day_of_week)







library(dplyr)
total_sorted<-arrange(total ,as.Date(total$date))
date_total=as.POSIXct(strptime(total$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
Appliances_reference=total_sorted$Appliances
total_purified=(total_sorted %>% select(-c(Appliances,Id,is_in_test,rv1,rv2,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                           BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))




total_purified$Day_of_week=as.numeric(total_purified$Day_of_week)

######train_complete)

##############completion par foret aléatoire

library(doParallel)
library(missForest)
registerDoParallel(cores=4)

new_total=missForest(total_purified, maxiter = 1000, ntree = 1000, variablewise = FALSE,
                     decreasing = FALSE, verbose = TRUE,
                     
                     mtry = floor(sqrt(ncol(total_purified))),
                     replace = TRUE,
                     classwt = NULL, cutoff = NULL, strata = NULL,
                     sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                     xtrue = NA, parallelize = 'forests')


train_completed_augmented=new_total$ximp
train_completed_augmented$Day_of_week=as.numeric(train_completed_augmented$Day_of_week)
X_train=(train_completed_augmented  %>% select(-Appliances))
try=train_completed_augmented
x_train_xgb=xgb.DMatrix(as.matrix(try %>% select(-Appliances)))
try_x=(as.matrix(try %>% select(-Appliances)))
x_train_xgb=xgb.DMatrix(try_x)
rm(x_train_xgb)
try_x
summary(train_o)
train_o$Appliances[1]
apply(train_o, 2, function(x) any(is.na(x)))
complete.cases(train_o)
#####################"
rmse(Appliances_reference,train_completed$Appliances)
rm(total)
rm(total_purified)
rm(total_sorted)
rm(new_total)
rm(train)
rm(test)
rm(test_sp)
rm(test_dt)
rm(try)
rm(train_completed)
try$NSM=as.numeric(try$NSM)
rm(try)
rm(X)
rm(X_train)
rm(try_x)









#####################methode xgboost
library(xgboost) 



xgb_trcontrol = trainControl(method = "cv", number = 20, allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)
xgbGrid <- expand.grid(nrounds = 1000,  max_depth = 50,colsample_bytree = seq(0.5, 0.9, length.out = 5), eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.5)
set.seed(0)



xgbGrid_heavy <- expand.grid(nrounds = c(500,1000),  
                             max_depth =50 ,
                             colsample_bytree = seq(0.5, 0.9, length.out = 5),
                             ## valeurs par défaut : 
                             eta = 0.1,
                             gamma=0,
                             
                             min_child_weight = 1,
                             subsample = 0.95)

X_train_xgb=xgb.DMatrix(as.matrix(train_completed_augmented), label=Appliances_reference)




xgb_model = xgb.train(params=list(booster="gbtree"),data=X_train_xgb , trControl = xgb_trcontrol, tuneGrid = xgbGrid_heavy,  method = "xgbTree",nrounds=10000,verbose=2,print_every_n = 1)


xgb_model$best_tune
#########################prediction



new_test=missForest(test_purified, maxiter = 1000, ntree = 2000, variablewise = FALSE,
                    decreasing = FALSE, verbose = TRUE,
                    
                    mtry = floor(sqrt(ncol(test_purified))),
                    replace = TRUE,
                    classwt = NULL, cutoff = NULL, strata = NULL,
                    sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                    xtrue = NA, parallelize = 'forests')

test_to_predict=new_test$ximp
X_test = xgb.DMatrix(as.matrix(test_to_predict))
test_to_predict$Day_of_week=as.numeric(test_to_predict$Day_of_week)

complete.cases(test_to_predict)

predictions=predict(xgb_model,newdata=X_test)
ts_predictions=ts(predictions)
ts_Appliances=ts(Appliances_reference)
xts_Appliances=xts(ts_Appliances,order.by = date_total)
date_test=Date = as.POSIXct(strptime(test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_vrai_test=as.POSIXct(strptime(vrai_test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
xts_predictions=xts(ts_predictions,order.by=date_vrai_test, color: "red")


graphes=cbind(xts_predictions,xts_Appliances)
length(predictions)

dygraph(graphes)




rmse(predictions,past_predictions)


total_prediction=c(predictions_sures,predictions)
plot(total_prediction)
length(total_prediction)

submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- total_prediction

write.table(submit, file="submission_new_methode.csv", quote=F, sep=",", dec='.',row.names = F)
plot(total_prediction)



