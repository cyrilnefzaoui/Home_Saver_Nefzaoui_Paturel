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

past_predictions=prediction_xgboost_simple[4655:5771]
predictions_sures=prediction_xgboost_simple[1:4654]

date_test=as.POSIXct(strptime(test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_vrai_test=date_test[4655:5771]



library(dplyr)
test=(test %>% select(-Id))
yes_is_in_test=rep(1, times = length(test$date))
test$is_in_test=yes_is_in_test
not_in_test=rep(0, times = length(train$date))
train$is_in_test=not_in_test
test$Appliances=rep(NA, times = length(test$date))
rmse = function(ychap, y){
  sqrt(mean((ychap - y)^2))
}



ts_Appliances=ts(train$Appliances)
xts_Appliances=xts(ts_Appliances,order.by = date_train)
date_train=Date = as.POSIXct(strptime(train$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))

date_test=Date = as.POSIXct(strptime(test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_test_futur=date_test[4655:5771]
date_vrai_test=as.POSIXct(strptime(vrai_test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_test_imputation_factors=total_sorted$date[which(total_sorted$is_in_test==1)]
date_test_imputation=as.POSIXct(strptime(date_test_imputation_f,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_test_futur=date_test[4655:5771]

rm(date_test_imputation)

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

rm(test_purified)
length(which(is.na(total$Appliances)))




library(dplyr)
total_sorted<-arrange(total ,as.Date(total$date))
date_total=as.POSIXct(strptime(total$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
Appliances_reference=total_sorted$Appliances

#####si on veut entrainer un modele avec peu de variables mais toutes tres plausibles (parcimonie)
total_purified=(total_sorted %>% select(-c(,rv1,rv2,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                           BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))



########## si on veut entrainer un modèle lourd


test_heavy=(vrai_test  %>% select(-c(Id,rv1,rv2)))
test_heavy_numeric=(lapply(test_heavy, function(x) as.numeric(x)))
test_heavy_numeric_df=setDT(test_heavy_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)

######train_complete)


###########Si on veut faire un train classique

true_train=(train%>% select(-c(Appliances,rv1,rv2)))
true_train_numeric=(lapply(true_train, function(x) as.numeric(x)))
true_train_numeric_df=setDT(true_train_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)




true_test=(test%>% select(-c(is_in_test,rv1,rv2,Appliances)))
true_test_numeric=(lapply(true_test, function(x) as.numeric(x)))
true_test_numeric_df=setDT(true_test_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)

test_prediction=true_test_numeric_df[4655:5771]
rm(test_prediction)

total_heavy=(total_sorted %>% select(-c(rv1,rv2,is_in_test)))
total_heavy_matrix=data.matrix(total_heavy, rownames.force = NA)
total_heavy_numeric= (lapply(total_heavy, function(x) as.numeric(x)))
total_heavy_numeric_df=setDT(total_heavy_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)

######train_complete)
length(which(is.na(total_heavy_numeric_df$Appliances)))


##############completion par foret aléatoire
  which(is.na(total_sorted$Appliances))
  
  
  
  
  
  
  
  
  
  

####################Si on veut imputer un jeu de données qui comporte DEJA les APPLIANCES à imputer
library(doParallel)
library(missForest)
registerDoParallel(cores=4)


total_imputed=missForest(total_heavy_numeric_df, maxiter = 100 ,ntree = 100, variablewise = FALSE,
                     decreasing = FALSE, verbose = TRUE,
                     
                     mtry = floor(sqrt(ncol(total_heavy_numeric_df))),
                     replace = TRUE,
                     classwt = NULL, cutoff = NULL, strata = NULL,
                     sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                     xtrue = NA, parallelize = 'forests')


train_complete_heavy=total_imputed$ximp
Appliances_total=train_complete_heavy$Appliances

Appliances_imputed_in_test=train_complete_heavy$Appliances[which(total_sorted$is_in_test==1)]
train_complete_heavy=(train_complete_heavy %>% select(-is_in_test))


length(which(is.na(train_complete_heavy$Appliances)))
plot(train_complete_heavy$Appliances)
plot(Appliances_total)
plot(Appliances_imputed_in_test)
rm(train_complete_heavy)

####################Si on veut imputer un jeu de données qui ne comporte PAS les appliances à completer


true_train_imputed=missForest(true_train_numeric_df, maxiter = 1000 ,ntree = 100, variablewise = FALSE,
                         decreasing = FALSE, verbose = TRUE,
                         
                         mtry = floor(sqrt(ncol(true_train_numeric_df))),
                         replace = TRUE,
                         classwt = NULL, cutoff = NULL, strata = NULL,
                         sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                         xtrue = NA, parallelize = 'forests')


true_train_complete=true_train_imputed$ximp





#####################methode xgboost
library(xgboost) 



xgb_trcontrol = trainControl(method = "oob", number = 100, allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds = 1000,  max_depth =6,colsample_bytree = c(0.1, 1, length.out = 10), eta = 0.01,gamma=0,min_child_weight = 1,subsample = 0.5)
set.seed(0)



xgbGrid_heavy <- expand.grid(nrounds = c(500,1000,1500,2000),  
                             max_depth =c(1:16,length.out = 15), 
                             colsample_bytree = seq(0.5, 0.9, length.out = 5),
                             ## valeurs par défaut : 
                             eta = 0.01,
                             gamma=0,
                             
                             min_child_weight = 1,
                             subsample = 1)



total_purified=(train_complete_heavy %>% select(-c(Appliances,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                                     BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))


train_complete_heavy_without_Appliances=(train_complete_heavy %>% select(-c(Appliances)))
X_train_xgb=xgb.DMatrix(as.matrix(train_complete_heavy_without_Appliances ), label=Appliances_total)
#X_train_xgb_purified=xgb.DMatrix(as.matrix( total_purified), label=Appliances_total)

X_true_train_xgb=xgb.DMatrix(as.matrix(true_train_complete), label=train$Appliances)



xgb_model = xgb.train(params=list(booster="gbtree",eval_metric=rmse),data=X_train_xgb_purified , trControl = xgb_trcontrol, tuneGrid = xgbGrid_heavy,  method = "xgbTree",nrounds=10000,verbose=2,print_every_n = 1)
rm(xgb_model)
#########################prediction



  test_imputed_heavy=missForest(test_heavy_numeric_df, maxiter = 1000, ntree = 100, variablewise = FALSE,
                    decreasing = FALSE, verbose = TRUE,
                    
                    mtry = floor(sqrt(ncol(test_heavy_numeric_df))),
                    replace = TRUE,
                    classwt = NULL, cutoff = NULL, strata = NULL,
                    sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                    xtrue = NA, parallelize = 'forests')


  
  
true_test_imputed=missForest(true_test_numeric_df, maxiter = 1000, ntree = 100, variablewise = FALSE,
                                decreasing = FALSE, verbose = TRUE,
                                
                                mtry = floor(sqrt(ncol(true_test_numeric_df))),
                                replace = TRUE,
                                classwt = NULL, cutoff = NULL, strata = NULL,
                                sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                                xtrue = NA, parallelize = 'forests')


test_to_predict=new_test$ximp
true_test_to_predict=true_test_imputed$ximp
test_prediction_only=true_test_to_predict[4655:5771]
  





"

complete.cases(test_to_predict)

#predictions=predict(xgb_model,newdata=X_test)
true_predictions=predict(xgb_model,newdata=X_true_test)
plot(predictions_only)


xts_prediction_futur=xts(ts_prediction_futur,order.by=date_test_futur, color: "red")
ts_Appliances_imputed_in_test=ts(Appliances_imputed_in_test)

xts_Appliances_imputed_in_test=xts(ts_Appliances_imputed_in_test,order.by=date_test_imputation)
ts_Appliances_imputed_in_test=ts(Appliances_imputed_in_test)



"

#X_test = xgb.DMatrix(as.matrix(test_to_predict))
#X_true_test=xgb.DMatrix(as.matrix(true_test_to_predict))
test_purified=(test_prediction_only %>%select(-c(date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                        BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))

X_test_prediction=xgb.DMatrix(as.matrix(test_prediction_only))
X_test_purified=xgb.DMatrix(as.matrix(test_purified))


#########

prediction_futur=predict(xgb_model,newdata=X_test_purified)
ts_prediction_futur=ts(prediction_futur)
xts_prediction_futur=xts(ts_prediction_futur,order.by=date_test_futur)


graphes=cbind(xts_prediction_futur,xts_Appliances,xts_Appliances_imputed_in_test)

dygraph(graphes)



dygraph(xts_prediction_futur)
d
rmse(true_predictions,prediction_xgboost_simple)


total_prediction=c(Appliances_imputed_in_test,prediction_futur)
plot(total_prediction)
plot(total_prediction)
length(total_prediction)

submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- total_prediction

write.table(submit, file="submission_deja_impute_overfitte.csv", quote=F, sep=",", dec='.',row.names = F)
plot(total_prediction)



