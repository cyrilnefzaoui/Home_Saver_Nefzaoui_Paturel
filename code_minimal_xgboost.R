rm(list=objects())


##importation des différentes bibliothèques
library(tidyverse)
library(lubridate)
library(xts)
library(dygraphs)
library(ranger)
library(dplyr)

library(mltools)
library(data.table)
library(xgboost)
library(caret)

##Importation des données
setwd("/Users/Nefzaoui/Documents/SIM202/tp1")
train <- read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')
appliances_imputed=read.csv(file="submission_composite_xgboost.csv", sep=",", dec='.')$Appliances[1:4654]



p2=read.csv(file="submission_73.2.csv", sep=",", dec='.')$Appliances[4655:5771]
p3=read.csv(file="submission_73.3.csv", sep=",", dec='.')$Appliances[4655:5771]
p4=read.csv(file="submission_73.9.csv", sep=",", dec='.')$Appliances[4655:5771]

p_moy=(p2+p3+p4)/3
p_moy_original=p_moy

p_moy=list(p_moy)
###Méthode de complétion partielle pour conserver les valeurs d'Appliances déjà prédites et vraisemblables
p_moy_df=setDT(p_moy, keep.rownames=FALSE, key=NULL, check.names=FALSE)
p_moy$to_use=rep(0, times = 1117)
p_moy$to_use[which(p_moy$V1<80)]=1
p_moy$V1[which(p_moy$to_use==0)]=NA
par(mfrow=c(2,2))

  plot(ts(p_moy_original))
lines(ts(p_moy$V1),
      col="red")


###Ajout d'un attribut is_in_test
test=(test %>% select(-Id))
yes_is_in_test=rep(1, times = length(test$date))
test$is_in_test=yes_is_in_test
not_in_test=rep(0, times = length(train$date))
train$is_in_test=not_in_test
test$Appliances=rep(NA, times = length(test$date))

#############
################Fusion du train et du test à imputer

train_dt=setDT(train, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_sp=test_dt[1:4654]
test_total=test_dt[1:5771]
test_sp$Appliances=appliances_imputed
appliances_test_total=c(appliances_imputed,p_moy$V1)
test_total$Appliances=appliances_test_total
total_sorted=merge(train_dt,test_total,all=TRUE)
##tri par ordre chronologique croissant
total_sorted<-arrange(total_sorted_test ,as.Date(total_sorted_test$date))


###On cree une copie de total_sorted et on l'appelle heavy pour insister sur le fait qu'il contient toutes les variables explicatives.
total_heavy=total_sorted
total_heavy_matrix=data.matrix(total_heavy, rownames.force = NA)
total_heavy_numeric= (lapply(total_heavy, function(x) as.numeric(x)))
total_heavy_numeric_df=setDT(total_heavy_numeric, keep.rownames=FALSE, key=NULL, check.names=FALSE)



library(doParallel)
library(missForest)
registerDoParallel(cores=4)

###on creee une variable total_light qui contient uniquement les variables pertinentes.
total_light=(total_heavy_numeric_df %>% select(-c(rv1,rv2,lights,Tdewpoint,RH_6,Visibility,Instant,DayType,BE_load_actual_entsoe_transparency,
                                                                      BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,Month,Windspeed,Press_mm_hg,InstantF,T2,T3,RH_2,RH_8,Day_of_week,Posan)))
permutation=sample(1:19735)
total_light_permuted=total_light[permutation]

total_with_imputation=missForest(total_light, maxiter = 10 ,ntree = 10, variablewise = TRUE,
                                decreasing = FALSE, verbose = TRUE,
                                
                                mtry =sqrt(ncol(total_light)),
                                replace = TRUE,
                                classwt = NULL, parallelize = 'forests')

##On recupere les donnees avec les Appliances imputees
data_complete=total_with_imputation$ximp
#######Approche Xgboost

library(xgboost) 






#####Définition de la structure de controle
xgb_trcontrol = trainControl(method = "cv", number = 1000,allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)

#####definitiion du grid
xgbGrid <- expand.grid(nrounds =1,  max_depth =50,colsample_bytree = 0.9, eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.5)

total_light$is_in_test[which(is.na(total_light$Appliances))]=2
to_train=total_light[which(total_light$is_in_test<2)]
y_train=total_light$Appliances[which(total_light$is_in_test<2)]
total_light_wt_App=(to_train %>% select(-Appliances))
X_train=(xgb.DMatrix(as.matrix(total_light_wt_App ),label=y_train))


xgb_model=xgb.train(params=list(booster="gbtree"),data=X_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid,  method = "xgbTree",nrounds=100000)
rm(X_test)
X_test=total_light[which(total_light$is_in_test==2)]
X_test=(X_test %>% select (-Appliances))
X_test=xgb.DMatrix(as.matrix(X_test))
predictions=predict(xgb_model,newdata=X_test)
plot(ts(predictions))



reconstitution_test=test_total$Appliances
length(which(is.na(reconstitution_test)))
tokaggle=c(appliances_imputed,predictions)
plot(tokaggle)

total_light$Appliances[which(is.na(total_light$Appliances))]=predictions
length(which(is.na(test_total$Appliances)))
test_total$Appliances[which(is.na(test_total$Appliances))]=predictions
which(is.na(test_total$Appliances))
plot(test_total$Appliances[4655:5771])
which(is.na(total_light$Appliances))

tokaggle=test_total$Appliances[4655:5771]
plot(to_g)

submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- tokaggle

write.table(submit, file="xgb_simple_sans_tuning", quote=F, sep=",", dec='.',row.names = F)
