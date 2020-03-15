

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


################################################################################################







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






head(train$Day_of_week)
train_lite=(train_dt %>% select(-Appliances))
train_medium=(train_dt %>% select(-c(date,Appliances)))
#wtf=xgb.DMatrix(as.matrix(train_lite))
head(train_lite)

X_train_ohe=one_hot(train_dt)
wtf=one_hot(train$WeekStatus)
wtf
X_train_m=xgb.DMatrix(as.matrix(train_medium))

X_train = xgb.DMatrix(as.matrix(train_lite))
class(train_medium$WeekStatus)

class(train_medium$DayType)
class(train_medium$Day_of_week)



y_train = train_dt$Appliances
as.numeric(levels(train$WeekStatus))[train$WeekStatus]


xgb_trcontrol = trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE, returnData = FALSE)

train$WeekStatus$lev


xgbGrid <- expand.grid(nrounds = 100,  max_depth = 5,colsample_bytree = seq(0.5, 0.6, length.out = 5), eta = 0.1,gamma=0,min_child_weight = 1,subsample = 1)


set.seed(0)
#xgb_model = train(train_lite, y_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid,  method = "xgbTree")

xgbGrid_heavy <- expand.grid(nrounds = c(100,200),  
                             max_depth = c(3, 5, 10, 15, 20),
                             colsample_bytree = seq(0.5, 0.9, length.out = 5),
                             ## valeurs par défaut : 
                             eta = 0.1,
                             gamma=0,
                             
                             min_child_weight = 1,
                             subsample = 1
)

xgb_model_heavy = train(X_train, y_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid_heavy,  method = "xgbTree")


X_test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)




test$WeekStatus=as.numeric(test$WeekStatus)
test$date=as.numeric(test$date)
test$DayType=as.numeric(test$DayType)
test$InstantF=as.numeric(test$InstantF)
test$Day_of_week=as.numeric(test$Day_of_week)






X_test_dt=setDT(test, keep.rownames=FALSE, key=NULL, check.names=FALSE)
test_lite=(X_test_dt %>% select(-Id))


#X_test_ohe=one_hot(X_test_dt, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE,dropCols = TRUE, dropUnusedLevels = FALSE)


X_test = xgb.DMatrix(as.matrix(test_lite))


head(X_train)
head(X_test)
head(X_test)
predicted = predict(xgb_model_heavy, X_test)



X_test2= xgb.DMatrix(as.matrix(test_lite2))






submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit$Appliances <- predicted

write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)
