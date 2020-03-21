

rm(list=objects())
library(tidyverse)
library(lubridate)
library(ranger)
setwd("/Users/Nefzaoui/Documents/SIM202/tp1")

train <- read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')

# Data <- readRDS("Data/Data.rds")
# Data_target <- readRDS("Data/test.RDS")

Data <-rbind(train[,-2], test[,-ncol(test)])
dim(Data)

Data_WTHNA <- Data[-which(is.na(Data$RH_6)), ]
RF_NA <- ranger(RH_6 ~ RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_7 + RH_8, data=Data_WTHNA)
Data$RH_6[which(is.na(Data$RH_6))] <- predict(RF_NA,data=Data[which(is.na(Data$RH_6)),])$predictions

Data_WTHNA <- Data[-which(is.na(Data$Visibility)), ]
RF_NA <- ranger(Visibility ~ Tdewpoint + Windspeed + T_out + Instant, data=Data_WTHNA)
Data$Visibility[which(is.na(Data$Visibility))] <- predict(RF_NA, data=Data[which(is.na(Data$Visibility)),])$predictions

train[,-2] <- Data[1:nrow(train),]
test[,-ncol(test)] <- Data[(nrow(train)+1):nrow(Data),]

#######"""booleans

yes_is_in_test=rep(1, times = length(test$date))
test$is_in_test=yes_is_in_test
not_in_test=rep(0, times = length(train$date))
train$is_in_test=not_in_test

class(train)

"
train_special=train$date[1:length(test$date)]
Appliances_s=train$Appliances[1:length(test$date)]
################################################################################################

"


rmse = function(ychap, y){
  sqrt(mean((ychap - y)^2))
}




library(mltools)
library(data.table)
library(xgboost)
library(caret)





rm(train)
rm(total)
rm(total_sp)



train_dt=setDT(train, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_sp=test_dt[1:4654]
total= merge(train_dt,test_sp,all=TRUE)
length(which(is.na(total$Appliances)))
length(which(is.na(total$lights)))
length(which(is.na(total$RH_6)))
length(which(is.na(total_s$Visibility)))












############"imputation valeurs manquantes


total_sp=total%>% select(-Id)


library(dplyr)
total_s<-arrange(total_sp ,as.Date(total_sp$date))
which(is.na(total_s$Appliances))



class(total$RH_6)
plot(total$Appliances)
library(missMDA)
library(FactoMineR)
ncomp <- estim_ncpPCA(total_s)
res.imp <- imputePCA(total_s, ncp = ncomp$ncp)

train_complete=res.imp$completeObs
train_complete_df <- as.data.frame(train_complete)

plot(train_complete_df$Appliances)
class(train_complete$Appliances)
ts_appliances_factomine=ts(train_complete_df$Appliances)
plot(ts_appliances_factomine)
rmse(real_imputed,Appliances_rajoutees)

real_imputed=train_complete_df$Appliances[-train$Appliances]
lines(real_imputed,col="green")
train_complete_dt=setDT(train_complete, keep.rownames=FALSE, key=NULL, check.names=FALSE)
train_complete_df <- as.data.frame(train_complete_dt)

plot(res.imp$completeObs$Appliances)
plot(train_complete[Appliances])
plot(res.imp$fittedX)
library(psych)

###fin du train:2016-05-19 23:50:00	
###test correspondant:4654
ccf(real_imputed,Appliances_rajoutees)
corr.test(real_imputed,Appliances_rajoutees)

plot(real_imputed-Appliances_rajoutees)
total_s$WeekStatus=as.numeric(total_s$WeekStatus)
total_s$date=as.numeric(total_s$date)
total_s$DayType=as.numeric(total_s$DayType)
total_s$InstantF=as.numeric(total_s$InstantF)
total_s$Day_of_week=as.numeric(total_s$Day_of_week)



library(missForest)

new_total=missForest(total_s, maxiter = 10, ntree = 100, variablewise = FALSE,
           decreasing = FALSE, verbose = TRUE,
           mtry = floor(sqrt(ncol(total_s))), replace = TRUE,
           classwt = NULL, cutoff = NULL, strata = NULL,
           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
           xtrue = NA, parallelize = 'no')









rm(total)
rm(total_s)
rm(total_sp)
rm(train)
rm(test)

total_tes=total_s[1:1000]
plot(train$Appliances)
lines(liste_rajoutes,col="green")



enhanced_train=new_total$ximp
length(which(is.na(enhanced_train$Visibility)))
length(which(is.na(enhanced_train$RH_6)))




pseudo_test=enhanced_train$Appliances[which(enhanced_train$is_in_test==1)]
length(pseudo_test)


liste_rajoutes=which(enhanced_train$is_in_test==1)
Appliances_rajoutees=enhanced_train$Appliances[-train$Appliances]

lines(Appliances_rajoutees,col="blue")


ts_Appliances=ts(train$Appliances)
plot(ts_Appliances)
lines(Appliances_rajoutees,col="blue")


###################

length(liste_rajoutes)
plot(liste_rajoutes)
title(echantillonag)
length(pseudo_test)+length(predicted)


plot(total_predictions)
total_predictions=c(pseudo_test,predicted)
length(total_predictions)
pseudo_test

df[which(df$col == 1), ]




Appliances_ts=ts(Appliances_s,frequency = 24)
aide=decompose(Appliances_ts)
plot(aide)
mean(aide$seasonal)
mean(aide$random,na.rm=TRUE)
spectrum(aide$seasonal)
residus=ts(aide$random,frequency = 24)
plot(residus)
library(forecast)
arma=auto.arima(residus,,trace=TRUE,ic="aic",parallel = TRUE,stationary = TRUE,approximation = TRUE,allowmean = FALSE)
which((is.na(residus)))
residus_bis=residus[-which((is.na(residus)))]
which((is.na(residus_bis)))

acf(residus_bis)
pacf(residus_bis)
ts_tot=ts(train$Appliances,frequency = 24)
aux=decompose(ts_tot)
plot(aux)
r=aux$random

sig=r+aux$trend
which(is.na(sig))
sig[1:12]=ts_tot[1:12]
sig[13953:13964]=ts_tot[13953:13964]
obs
which(is.na(aux$trend))
which(is.na(aux$seasonal))
which(is.na(aux$random))


which(is.na(sig))
plot(sig)

plot(aux$random)
t=c(1:length(Appliances_ts))
w=2*pi/24
fourier<-cbind(cos(w*t), sin(w*t))
K<-5
for(i in c(2:50))
{
  fourier<-cbind(fourier,cos(i*w*t), sin(i*w*t))
}
dim(fourier)

#eg<-lm(TAVG_train~fourier[,1:2])
#reg<-lm(TAVG_train~fourier[,1:2])
##50
#reg<-lm(TAVG_train~fourier[,1:2])

###pour aller plus loin

###
reg_th_fourier=lm(aide$seasonal~fourier[,1:10])

summary(reg_th_fourier)
reg_th_fourier$coefficients

rmse(reg_th_fourier$fitted.values,aide$seasonal)
  prediction_seasonality=predict(reg_th_fourier,newdata = test)
plot(prediction_seasonality)
plot(reg_th_fourier$fitted.values)
lines(aide$seasonal,col="red")
plot(aide$seasonal,col="green")


#######################approche xgb_boost
library(mltools)
library(data.table)
library(xgboost)
library(caret)


train$WeekStatus=as.numeric(train$WeekStatus)
train$date=as.numeric(train$date)
train$DayType=as.numeric(train$DayType)
train$InstantF=as.numeric(train$InstantF)
train$Day_of_week=as.numeric(train$Day_of_week)



train_dt=setDT(train, keep.rownames=FALSE, key=NULL, check.names=FALSE)
which(is.character(train_dt))



which(is.na(new_total$Appliances))
new_total$ximp

train_sp=new_total$ximp

plot(enhanced_train$Appliances)

head(train$Day_of_week)
train_lite=(train_dt %>% select(-Appliances))
X_train_best=(train_dt %>% select(-c(date,Appliances,Instant,WeekStatus,Day_of_week,DayType,rv1,rv2)))
train_heavy=(enhanced_train %>% select(-c(date,Appliances,Instant,WeekStatus,Day_of_week,DayType,is_in_test)))
#wtf=xgb.DMatrix(as.matrix(train_lite))
head(train_lite)
plot(train_dt$rv1)
X_train_ohe=one_hot(train_dt)
wtf=one_hot(train$WeekStatus)
wtf
X_train_m=xgb.DMatrix(as.matrix(train_medium))
X_train_h=xgb.DMatrix(as.matrix(train_heavy))
X_train_xgb = xgb.DMatrix(as.matrix(train_lite))
class(train_medium$WeekStatus)

class(train_medium$DayType)
class(train_medium$Day_of_week)


##########


X_train_completed=missForest(train, maxiter = 10, ntree = 100, variablewise = FALSE,
                     decreasing = FALSE, verbose = TRUE,
                     mtry = floor(sqrt(ncol(total_s))), replace = TRUE,
                     classwt = NULL, cutoff = NULL, strata = NULL,
                     sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                     xtrue = NA, parallelize = 'no')

X_train_f=X_train_completed$ximp
X_train_sp=(X_train_f %>% select(-c(Appliances,is_in_test,lights,date,Instant,InstantF,Day_of_week,DayType)))
            



y_train = train$Appliances
y_train_heavy=enhanced_train$Appliances
X_train_sp=(X_train_f %>% select(-c(Appliances,is_in_test,lights)))
X_train=(train %>% select(-c(Appliances,is_in_test))

         
         
         
         
         
         
#################################
X_train_enhanced=(enhanced_train%>%select(-c(lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                                BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))



X_train_like_gbm=(train%>%select(-c(rv1,rv2,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                                                              BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))

train_gbm=(train %>% select(-c(Appliances,rv1,rv2,DayType,Instant,date,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                               BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))

         
  
  
         
         
         
as.numeric(levels(train$WeekStatus))[train$WeekStatus]


xgb_trcontrol = trainControl(method = "cv", number = 10, allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)
#xgb_trcontrol = trainControl(method = "cv", number = 2, allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)



xgbGrid <- expand.grid(nrounds = 100,  max_depth = 20,colsample_bytree = seq(0.5, 0.6, length.out = 5), eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.5)
#xgbGrid <- expand.grid(nrounds = 10,  max_depth = 2,colsample_bytree = seq(0.5, 0.6, length.out = 5), eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.5)

rm(X_train_sp)
set.seed(0)
#xgb_model = train(train_lite, y_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid,  method = "xgbTree")

xgbGrid_heavy <- expand.grid(nrounds = c(100,500),  
                             max_depth = seq(1,20,length.out=20),
                             colsample_bytree = seq(0.5, 0.9, length.out = 5),
                             ## valeurs par défaut : 
                             eta = 0.1,
                             gamma=0,
                             
                             min_child_weight = 1,
                             subsample = 1)




y_train=train$Appliances


seq(1,20,length.out=20)



xgb_model_heavy = train(train_gbm, y_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid_heavy,  method = "xgbTree")


rmse(xgb_model_heavy$fitted.val,y_train_heavy)

xgb_model_heavy$bestTune

xgb_model_heavy$bestTune
length(xgb_model_heavy$fitted.values)
plot(xgb_model_heavy$fitted.values)
which(is.na(xgb_model_heavy$fitted.values))
class(xgb_model_heavy$fi)
plot(xgb_model_heavy$fitted.values)
xgb_model_heavy$fit
length(xgb_model_heavy$results)
X_test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)

4654


test$WeekStatus=as.numeric(test$WeekStatus)
test$date=as.numeric(test$date)
test$DayType=as.numeric(test$DayType)
test$InstantF=as.numeric(test$InstantF)
test$Day_of_week=as.numeric(test$Day_of_week)
test$


  
  
which(is.na(total_s$Appliances))
which(is.na(train_sp$Appliances))

train_sp$Appliances[3923]-predicted[991]

length(predicted)
X_test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_lite=(X_test_dt %>% select(-Id))
X_test_best=(X_test_dt %>% select(-c(date,Id,Instant,WeekStatus,Day_of_week,DayType,rv1,rv2)))


#X_test_ohe=one_hot(X_test_dt, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE,dropCols = TRUE, dropUnusedLevels = FALSE)

ml



new_test_lite=test_lite[4655:5771]
new_test_lighted=(new_test_lite%>% select(-c(date,Instant,WeekStatus,Day_of_week,DayType,is_in_test)))

X_test = xgb.DMatrix(as.matrix(test_heavy))
test_heavy=(new_test_lite %>% select(-c(date,Instant,WeekStatus,Day_of_week,DayType,is_in_test)))
new_test_lighted$InstantF=as.numeric(new_test_lighted$InstantF)




X_test_sp=(new_test_lite %>% select(-c(is_in_test,lights,date,Instant,InstantF,Day_of_week,DayType)))


head(X_train)
head(X_test)
head(X_test)
predicted = predict(xgb_model_heavy, X_test_best)

plot(predicted)


predicted_train = predict(xgb_model_heavy, X_train_sp)
plot(predicted)
predicted_tot=predicted+prediction_seasonality
lines(predicted_tot,col="red")
X_test2= xgb.DMatrix(as.matrix(test_lite2))

inv_predict=InvBoxCox(predicted,lambda = p_lambda)


plot(total_prediction)


total_prediction=c(pseudo_test,predicted)

plot(inv_predict)

submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- total_prediction

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)
plot(total_prediction)


##################
library(caret)
library(dygraphs)
library(xts)
new_Appliances= BoxCox(train$Appliances,lambda = p_lambda)
ts_new_Appliances=ts(new_Appliances)
plot(ts_new_Appliances)
plot(new_Appliances)
plot(aya2)
bis=as.numeric(aya)
which(is.na(bis))
aya3=InvBoxCox(aya2,lambda = p_lambda)
p_lambda=BoxCox.lambda(train$Appliances, method = c( "guerrero"), lower = -1, upper = 1)

inv=InvBoxCox(as.numeric(aya),lambda = "auto")
rmse(train$Appliances,aya3)
plot(aya)
plot(aya$fudge)
length(aya$fudge)
rmse(inv_predict,train$Appliances[100:5771+100])





test_lite=(X_test_dt %>% select(-c(date,Id,Instant,WeekStatus,Day_of_week,DayType)))



plot(train_sp$Appliances)
lines(predicted,col="red")
plot(predicted)


ts_train=ts(train_sp$Appliances)
plot(ts_train)

ts_test=ts(predicted)
lines(ts_test,col="green")



##################Utilisation d'un gradient Boosting Machine
##Cette methode a ici du sens dans le cadre de modèles lineaires



"

train_gbm=(train %>% select(-c(lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                               BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))

rm(train_gbm)
train_gbm=(train %>% select(-c(lights,Visibility)))
                    
           
           
           
    d
    
           
                                 
library(gbm)
object_gbm=gbm(formula =my_formula ,
                                        
               data =train_gbm, distribution = "gaussian",
              , var.monotone = NULL, n.trees = 10000,
               interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.05,
               bag.fraction = 0.5, train.fraction = 1, cv.folds = 20,
               keep.data = TRUE, verbose = TRUE,   n.cores = 4)



my_formula=Appliances~NSM+T1+RH_1+
  T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+T6+RH_6+ 
  T7+RH_7+T8+RH_8+T9+RH_9+T_out+Press_mm_hg+Windspeed+
  Tdewpoint+Day_of_week

plot(object_gbm$cv.error)


T1,RH1,T2,RH2,T3,RH3,
T4,RH4,T5,RH5,T6,RH6, T7,RH7,T8,TH8,T9,RH9,To,
Pressure,Rho,WindSpeed,
Tdewpoint,NSM, WeekStatus,
Day of Week
summary(object_gbm)


Tdewpoint,, WeekStatus,
Day of Week

Tdewpoint,NSM, WeekStatus,
Day of Week
Pressure,Rho
Tdewpoint,NSM, WeekStatus,
Day of Week

plot(ts(object_gbm$oobag.improve))
plot(object_gbm$train.error)
rmse(object_gbm$fit,enhanced_train$Appliances)
plot(object_gbm$fit)
rmse(object_gbm$fit,train$Appliances)
plot(train$Appliances)
lines(object_gbm$fit,col="green")

prediction_gbm=predict.gbm(object_gbm,newdata = test)


rmse(prediction_gbm,predicted)
plot(prediction_gbm)
lines(predicted,col="red")
moy=0.5*(prediction_gbm+predicted)
plot(moy)
rmse(moy,predicted)
rmse(moy,prediction_gbm)



##########Visualisation des résultats

plot(total_prediction)
lines(y_train_heavy,col="blue")
max(y_train_heavy)
lines(train$Appliances,col="green")
max(train$Appliances)

library(dygraphs)
library(xts)

ts_predicted=ts(predicted)
length(ts_predicted)

length(enhanced_train$Appliances)
imputed_Appliances=enhanced_train$Appliances[which(enhanced_train$is_in_test==1)]
length(imputed_Appliances)
ts_imputed_Appliances=ts(imputed_Appliances)
plot(ts_predicted,col="red")
lines(predicted[1:4654],col="black")
lines(ts_imputed_Appliances,col="dark green")
rmse(imputed_Appliances,predicted[1:4654])
pure_prediction=predicted[4655:5771]
ts_pure_prediction=ts(pure_prediction)

rm(train)
rm(test)
library(xts)
library(dygraphs)
rm(date_test)
date_test=Date = as.POSIXct(strptime(test$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
date_train=Date = as.POSIXct(strptime(train$date,"%Y-%m-%d  %H:%M:%S" ,tz="GMT"))
length(date_test)
predicted_xts<-xts(ts_predicted,order.by=date_test)
date_imputed=date_test[1:4654]
imputed_Appliances_xts=xts(ts_imputed_Appliances,order.by=date_imputed)
pure_prediction=xts(ts_pure_prediction,ore)
date_pure_prediction=date_test[4655:5771]
xts_pure_prediction=xts(ts_pure_prediction,order.by=date_pure_prediction)
graphes=cbind(imputed_Appliances_xts,predicted_xts,xts_pure_prediction)
dygraph(graphes)



dygraph(xts_pure_prediction,main="prediction des valeurs futures de Appliances")
          