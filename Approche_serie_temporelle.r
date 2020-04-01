rm(list=objects())

library(tidyverse)
library(lubridate)
library(xts)
library(dygraphs)
library(ranger)
setwd("/Users/Nefzaoui/Documents/SIM202/tp1")
###
train <- read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')
date_train=Date = as.POSIXct(strptime(train$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_test=Date = as.POSIXct(strptime(test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))


prediction_xgboost_simple_struct=read.csv(file="submission_simple_xgboost.csv", sep=",", dec='.')$Appliances[1:4654]

prediction_xgboost_simple=prediction_xgboost_simple_struct$Appliances

best_prediction_struct=read.csv(file="submission_Appliances_bertrand_best_score.csv", sep=",", dec='.')





p_xgb=xts((read.csv(file="submission_simple_xgboost.csv", sep=",", dec='.')$Appliances),order.by=date_test)
p1=xts((read.csv(file="submission_72.9.csv", sep=",", dec='.')$Appliances),order.by=date_test)
p2=xts((read.csv(file="submission_73.2.csv", sep=",", dec='.')$Appliances),order.by=date_test)
p3=xts((read.csv(file="submission_73.3.csv", sep=",", dec='.')$Appliances),order.by=date_test)
p4=xts((read.csv(file="submission_73.9.csv", sep=",", dec='.')$Appliances),order.by=date_test)
p_mf500=xts((read.csv(file="submission_mf500_permuted.csv", sep=",", dec='.')$Appliances[4655:5771]),order.by=date_test[4655:5771])
p_mf1000=xts((read.csv(file="submission_mf1000.csv", sep=",", dec='.')$Appliances[4655:5771]),order.by=date_test[4655:5771])
p_best=xts((read.csv(file="reg_726.csv", sep=",", dec='.')$Appliances[4655:5771]),order.by=date_test[4655:5771])
p_moy=(p1+p2+p3)/3

length(p_mf)
appliances_imputed=read.csv(file="submission_composite_xgboost.csv", sep=",", dec='.')$Appliances[1:4654]

p_mf_permuted=xts((read.csv(file="submission_mf500_permuted.csv", sep=",", dec='.')$Appliances),order.by=date_test)
appliances_bertrand=read.csv(file="submission_72.9.csv", sep=",", dec='.')$Appliances[1:4654]
app=read.csv(file="submission_composite_xgboost.csv", sep=",", dec='.')$Appliances[1:4654]

plot(app,prediction_xgboost_simple_struct)

best_Appliances_permutation=c(appliances_imputed,Appliances1000)

plot(ts(best_Appliances_permutation))
ts_Appliances=ts(train$Appliances)
xts_Appliances=xts(ts_Appliances,order.by = date_train)


p_mf=xts((read.csv(file="submission_composite_xgboost.csv", sep=",", dec='.')$Appliances),order.by=date_test)
plot(train$NSM)

fff=read.csv(file="submission_72.9.csv", sep=",", dec='.')$Appliances[4655:5771]
plot(appliances_bertrand)
  






p6=xts((read.csv(file="submission_mixgametreg.csv", sep=",", dec='.')$Appliances),order.by=date_test)


graphes=cbind(p5,xts_Appliances,p1)
dygraph(graphes)

new_predictions
fill=rep(0, times = 1117)
to_imput=c(Appliances_imputed,fill)

4654+1117

plot
diff=p_moy-p_mf
length(p1)

graphe=cbind(xts_Appliances,p_c_mf)

dygraph(graphe)
lines(p_xgb,col="red")
dygraph(diff)

ts_best_Appliances=ts(best_Appliances)
Xts_best_Appliances=xts(ts_best_Appliances,order.by=date_test)



ts_prediction_boost=ts(prediction_xgboost_simple)
xts_prediction_boost=xts(ts_prediction_boost,order.by=date_test)

ts_Appliances_reference=ts(prediction_xgboost_simple)
ts_Appliances_reference=xts(ts_Appliances_reference,order.by=date_total)
Appliances_to_impute=prediction_xgboost_simple[1:4654]

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



graphe=cbind(xts_Appliances,Xts_best_Appliances,xts_prediction_boost)
dygraph(graphe)


date_test_futur=date_test[4655:5771]
date_vrai_test=as.POSIXct(strptime(vrai_test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_test_imputation_factors=total_sorted$date[which(total_sorted$is_in_test==1)]
date_test_imputation=as.POSIXct(strptime(date_test_imputation_f,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_test_futur=date_test[4655:5771]
date_test_to_impute=as.POSIXct(strptime(test$date[1:4654],"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
rm(date_test_imputation)

library(mltools)
library(data.table)
library(xgboost)
library(caret)





train_dt=setDT(train, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test
test_sp=test_dt[1:4654]
test_total=test_dt[1:5771]
#test_sp$Appliances=prediction_xgboost_simple[1:4654]
#test_sp$Appliances=rep(NA, times = 4654)
test_sp$Appliances=appliances_imputed

fill_na=rep(NA, times = 1117)
appliances_test_total=c(appliances_imputed,fill_na)

test_total$Appliances=appliances_test_total
  
  
  rm(test_sp)
#Appliances_to_impute=xts(ts(test_sp$Appliances),order.by=date_test_to_impute,col="red")

graphe=cbind(xts_Appliances,Appliances_to_impute)
dygraph(graphe,main = "Imputation des valeurs manquantes de Appliances")
vrai_test=test_dt[4655:5771]
total= merge(train_dt,test_sp,all=TRUE)
total_sorted_test=merge(train_dt,test_total,all=TRUE)

test_purified=(vrai_test %>% select(-c(Id,is_in_test,rv1,rv2,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                       BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))
test_purified$Day_of_week=as.numeric(test_purified$Day_of_week)

rm(test_purified)
length(which(is.na(total$Appliances)))




library(dplyr)
total_sorted<-arrange(total ,as.Date(total$date))
total_with_test_sorted=arrange(total_sorted_test ,as.Date(total_sorted_test$date))
total_sorted$is_in_test
which(is.na(total_sorted$Appliances))
length(total_sorted$Appliances)
date_total=as.POSIXct(strptime(total$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))

#####si on veut entrainer un modele avec peu de variables mais toutes tres plausibles (parcimonie)
total_purified=(total_sorted %>% select(-c(rv1,rv2,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                           BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))


Appliances_train_and_imputed=total_sorted$Appliances

vraies_Appliances=xts(ts(Appliances_train_and_imputed),order.by=date_total)
########## si on veut entrainer un modèle lourd


test_heavy=(vrai_test  %>% select(-c(Id,rv1,rv2)))
test_heavy_numeric=(lapply(test_heavy, function(x) as.numeric(x)))
test_heavy_numeric_df=setDT(test_heavy_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)

######train_complete)


###########Si on veut faire un train classique

true_train=(train%>% select(-c(Appliances,rv1,rv2)))
true_train_numeric=(lapply(true_train, function(x) as.numeric(x)))
true_train_numeric_df=setDT(true_train_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)
true_test=(test%>% select(-c(rv1,rv2,Appliances)))
true_test_numeric=(lapply(true_test, function(x) as.numeric(x)))
true_test_numeric_df=setDT(true_test_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)

test_prediction=true_test_numeric_df[4655:5771]
rm(test_prediction)

total_heavy=(total_sorted_test %>% select(-c(rv1,rv2)))
total_heavy_matrix=data.matrix(total_heavy, rownames.force = NA)
total_heavy_numeric= (lapply(total_heavy, function(x) as.numeric(x)))
total_heavy_numeric_df=setDT(total_heavy_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)
######train_complete)


length(which(is.na(total_heavy_numeric_df$Appliances)))





############"essai avec missMDA
library(missMDA)
library(FactoMineR)

total_light=(total_heavy_numeric_df %>% select (-c(,rv1,rv2,date,
                        DayType,Instant,lights,Visibility,
                        InstantF,Posan,Month,Heure,
                        BE_load_actual_entsoe_transparency)))
      


ncomp <- estim_ncpPCA(total_heavy_numeric_df)
res.imp <- imputePCA(total_sorted, ncp = ncomp$ncp)

train_complete=res.imp$completeObs
train_complete_df <- as.data.frame(train_complete)












length(which(is.na(total_heavy_numeric_df_permuted$Appliances)))



##############completion par foret aléatoires
  which(is.na(total_purified$Appliances))
  

####################Si on veut imputer un jeu de données qui comporte DEJA les APPLIANCES à imputer
library(doParallel)
library(missForest)
registerDoParallel(cores=4)
total_heavy_numeric_df$is_in_test[18619:19735]=2
permutation=sample(1:19735)





total_heavy_numeric_df_permuted=total_heavy_numeric_df[permutation]
complete.cases(total_heavy_numeric_df_permuted_purified)

total_heavy_numeric_df_permuted_purified=(total_heavy_numeric_df_permuted %>% select(-c(lights,Tdewpoint,RH_6,Visibility,Instant,DayType,BE_load_actual_entsoe_transparency,
BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,Month,Windspeed,Press_mm_hg,InstantF,T2,T3,RH_2,RH_8,Day_of_week,Posan)))
rm(total_heavy_numeric_df_permuted_purified)


total_imputed50=missForest(total_heavy_numeric_df_permuted_purified, maxiter = 100 ,ntree = 100, variablewise = TRUE,
                     decreasing = FALSE, verbose = TRUE,
                     
                     mtry =50,
                     replace = TRUE,
                     classwt = NULL, cutoff = NULL, strata = NULL,
                     sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                     xtrue = NA, parallelize = 'forests')

data_complete10=total_imputed50$ximp

data_just_imputed10=data_complete30[which(data_complete10$is_in_test==2)]

  
data_sorted10<-arrange(data_just_imputed10 ,data_just_imputed10$date)
Appliances10=data_sorted10$Appliances
length(Appliances10)
xts_10=xts(ts(Appliances10),order.by=date_test[4655:5771])

to_kaggle=c(appliances_imputed,Appliances30)

best_Appliances=read.csv(file="submission_mf1000.csv", sep=",", dec='.')$Appliances

rmse(to_kaggle,best_Appliances)
plot(to_kaggle)
rmse
plot(Appliances100)
rmse(Appliances100,Appliances500)

graphes=cbind(xts_10,p_best)
dygraph(graphes)
length(to_kaggle)
plot(to_kaggle)

lines(Appliances1000,col="red")
















data_train_plus_validation=total_imputed$ximp
train_complete_validation=data_train_plus_validation[1:17418]

y_train=total_sorted$Appliances[1:17418]


total_to_validate_wtha=(total_to_validate %>% select(-Appliances))
total_to_validate_purified=(total_to_validate%>% select(-c(lights,Visibility,InstantF,Posan,BE_load_actual_entsoe_transparency,
                                                           BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))

total_validation=(total_heavy_numeric_df  %>% select(-Appliances))




train_complete_validation=(train_complete_validation %>% select (-Appliances))
y_train=total_sorted$Appliances[1:18618]
plot(y_train)
rm(Appliances_with_imput)


~#Appliances_imputed=train_complete_heavy$Appliances[which(total_purified$is_in_test==1)]
#length(Appliances_imputed)
#which(is.na(Appliances_with_imput))

p_c_mf=p_mf=xts((Appliances_imputed),order.by=date_test_impute)
date_test_impute=date_test[1:4654]

Appliances_total=train_complete_heavy$Appliances


Appliances_imputed_in_test=train_complete_heavy$Appliances[which(total_sorted$is_in_test==1)]

train_complete_heavy=(train_complete_heavy %>% select(-is_in_test))


length(which(is.na(total_purified$Appliances)))

plot(train_complete_heavy$Appliances)
plot(Appliances_total)
plot(Appliances_imputed_in_test)
rm(train_complete_heavy)




#####################Approche  xgboost
library(xgboost) 

xgb_trcontrol = trainControl(method = "repeatedcv", number = 1000 ,repeats=100,allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds =2000,  max_depth =c(1:15,length.out=15),colsample_bytree = c(0.1, 1, length.out = 10), eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.5)


"

#xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(3, 5, 10, 15, 20),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## valeurs par défaut : 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)




#xgbGrid <- expand.grid(.mtry = 10, .splitrule = , .min.node.size = 10)
"
"xgbGrid_heavy <- expand.grid(nrounds = c(500,10000),  
                             max_depth =c(1:16,length.out = 15), 
                             colsample_bytree = seq(0.5, 0.9, length.out = 5),
                             ## valeurs par défaut : 
                             eta = 0.01,
                             gamma=0,
                             
                             min_child_weight = 1,
                             subsample = 1)

"

#total_purified=(total_purified %>% select(-c(Appliances,date,DayType,Instant,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                                    # BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))


#train_complete_heavy_without_Appliances=(train_complete_heavy %>% select(-c(Appliances)))

#_train_xgb=xgb.DMatrix(as.matrix(train_complete_heavy_without_Appliances ), label=Appliances_total)


a_retirer=c(is_in_test,BE_load_actual_entsoe_transprency,BE_load_actual_entsoe_transparency)
rm(train_purified)

train_purified=(train_complete_validation %>% select(-c(llights,Visibility,Posan,BE_load_actual_entsoe_transparency,
                                       BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,date,InstantF,Instant)))


#X_train_validation=xgb.DMatrix(as.matrix(train_complete_validation ),label=y_train)
#y_train=Appliances_train_and_imputed[1:17498]
X_train_purified=xgb.DMatrix(as.matrix(train_complete_validation ),label=y_train)



#y_train=total_purified$Appliances[1:15618]

#X_train_xgb_purified=xgb.DMatrix(as.matrix( total_purified), label=y_train)

#X_true_train_xgb=xgb.DMatrix(as.matrix(true_train_complete), label=train$Appliances)




#xgb_model = xgb.train( params=list("gblinear"), X_train_validation , trControl = xgb_trcontrol, tuneGrid = xgbGrid,nrounds=10)



xgb_model=xgb.train(params=list(booster="gbtree"),data=X_train_purified, trControl = xgb_trcontrol, tuneGrid = xgbGrid,  method = "xgbTree",nrounds=10000,verbose=2,print_every_n = 1)
#####################validation

xgb.save(xgb_model,'our_current_best_xgb_model.r')



date_val=date_total[17419:18618]

Appliances_validation=total_sorted$Appliances[17419:18618]
q=xts(ts(Appliances_validation),order.by=date_val)

rm(data_validation)
data_validation=data_train_plus_validation[17419:18618]

data_validation=(data_validation %>% select(-c(date,lights,is_in_test,Instant,InstantF,Visibility,Posan,BE_load_actual_entsoe_transparency,
BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile)))



data_validation=(data_validation %>%select(-Appliances))
X_validation=xgb.DMatrix(as.matrix(data_validation))

Appliances_xgb=predict(xgb_model,X_validation)
xts_xgb=xts(ts(Appliances_xgb),order.by=date_val)
rmse(Appliances_xgb,Appliances_validation)

graphe=cbind(xts_xgb,q)
dygraph(graphe)

######################Vrai test


X_vraie_validation



plot(Appliances_train_and_imputed)








true_test_imputed=missForest(true_test_numeric_df, maxiter = 1000, ntree = 100, variablewise = FALSE,
                             decreasing = FALSE, verbose = TRUE,
                             
                             mtry = floor(sqrt(ncol(true_test_numeric_df))),
                             replace = TRUE,
                             classwt = NULL, cutoff = NULL, strata = NULL,
                             sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                             xtrue = NA, parallelize = 'forests')


data_test=true_test_imputed$ximp
test_to_predict_heavy=data_test[4655:5771]





rm(test_prediction_purified)



test_prediction_purified=(test_to_predict_heavy %>% select(-c(lights,Visibility,InstantF,Posan,BE_load_actual_entsoe_transparency,
                                                                        BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))




X_test_purified=xgb.DMatrix(as.matrix(test_prediction_purified))



Appliances_predicted=predict(xgb_model,X_test_purified)
Appliances_deja_connues=predict(xgb_model,X_validation)



plot(ts(Appliances_predicted))
plot(Appliances_predicted)
















  test_imputed_heavy=missForest(test_heavy_numeric_df, maxiter = 1000, ntree = 00, variablewise = FALSE,
                    decreasing = FALSE, verbose = TRUE,
                    
                    mtry = floor(sqrt(ncol(test_heavy_numeric_df))),
                    replace = TRUE,
                    classwt = NULL, cutoff = NULL, strata = NULL,
                    sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                    xtrue = NA, parallelize = 'forests')



test_to_predict=new_test$ximp
true_test_to_predict=true_est_imputed$ximp


























complete.cases(test_to_predict)

#predictions=predict(xgb_model,newdata=X_test)
true_predictions=predict(xgb_model,newdata=X_true_test)
plot(predictions_only)


xts_prediction_futur=xts(ts_prediction_futur,order.by=date_test_futur, color: "red")
ts_Appliances_imputed_in_test=ts(Appliances_imputed_in_test)

xts_Appliances_imputed_in_test=xts(ts_Appliances_imputed_in_test,order.by=date_test_imputation)
ts_Appliances_imputed_in_test=ts(Appliances_imputed_in_test)





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
submit$Appliances <- to_kaggle

write.table(submit, file="submission_mf_bertrand_cyril.csv", quote=F, sep=",", dec='.',row.names = F)
plot(to_kaggle)

length(p_moy)










####################Approche serie temporelle,
ts_Appliances_total=ts(total_sorted$Appliances)
plot(ts_Appliances)
xts_Appliances_total=xts(ts_Appliances_total,order.by=date_total)
dygraph(xts_Appliances)
h=decompose(xts_Appliances_total)
check.residuals(ts_Appliances)



dygraph(xts_Appliances_total)
stl(ts_Appliances_total)
spectrum(ts_Appliances_total)
acf(ts_Appliances_total)
pacf(ts_Appliances_total)

library(forecast)
modele_arima=auto.arima(new_Appliances)
p_lambda=BoxCox.lambda(ts_Appliances_total, method = c( "guerrero"), lower = -1, upper = 1)
new_Appliances= BoxCox(ts_Appliances_total,lambda = p_lambda)
arima.sim(modele_arima,n=100)

u=xts(new_Appliances,order.by=date_total)
dygraph(u)

decompose(new_Appliances)
