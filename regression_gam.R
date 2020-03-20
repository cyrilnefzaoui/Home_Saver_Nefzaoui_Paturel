rm(list=objects())
library(tidyverse)
library(lubridate)
library(ranger)
library(forecast)
setwd("/Users/Nefzaoui/Documents/SIM202/tp1")


  rmse = function(ychap, y){
  sqrt(mean((ychap - y)^2))
}


##mettre 10 en seuil pour RH6

train <- read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')
rame

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
paste("data.frame(train,",cos,",",sin,")",sep="")

####le but du code suivant est d'ex?cuter l'instruction:
####train<-data.frame(train,cos1,cos2,cos3,cos4,cos5,cos6,cos7,cos8,cos9,cos10,sin1,sin2,sin3,
#sin4,sin5,sin6,sin7,sin8,sin9,sin10)

#train<-data.frame(train,cos1,cos2,cos3,cos4,cos5,cos6,cos7,cos8,cos9,cos10,sin1,sin2,sin3,sin4,sin5,sin6,sin7,sin8,sin9,sin10)

train<-eval(parse(text=paste("data.frame(season,",cos,",",sin,")",sep="")))
names(season)
###ne marche pas:
###eval(paste("data.frame(train,",cos,",",sin,")",sep=""))

names(season)

lm.fourier<-list()
eq<-list()
for(i in c(1:Nfourier))
{
  cos<-paste(c('cos'),c(1:i),sep="")
  sin<-paste(c('sin'),c(1:i),sep="")
  fourier<-paste(c(cos,sin),collapse="+")
  eq[[i]]<-as.formula(paste("season~",fourier,sep=""))
  lm.fourier[[i]]<-lm(eq[[i]],data=season)
}

length(lm.fourier)

lapply(lm.fourier, summary)



par(mfrow=c(1,1))

plot(Appliances)
lines(fit.arima$fitted,col='red')
which(is.na(Appliances))
which(is.na(fit.arima$fit

####importation des donnees e
train<-read.csv(file="train.csv", sep=",", dec='.')
test <- read.csv(file="test.csv", sep=",", dec='.')
length(train)
length(new$trend)
#
###nettoyage des donnees
Data <-rbind(train[,-2], test[,-ncol(test)])
dim(Data)

Data_WTHNA <- Data[-which(is.na(Data$RH_6)), ]
which(is.na(Data$RH_6))
RF_NA <- ranger(RH_6 ~ RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_7 + RH_8, data=Data_WTHNA)
#Data$RH_6[which(is.na(Data$RH_6))] <- predict(RF_NA,data=Data[which(is.na(Data$RH_6)),])$predictions
Data$RH_6[which(is.na(Data$RH_6))]=10
which(is.na(Data$RH_6))
Data_WTHNA <- Data[-which(is.na(Data$Visibility)), ]
RF_NA <- ranger(Visibility ~ Tdewpoint + Windspeed + T_out + Instant, data=Data_WTHNA)




Data$Visibility[which(is.na(Data$Visibility))] <- predict(RF_NA, data=Data[which(is.na(Data$Visibility)),])$predictions
train[,-2] <- Data[1:nrow(train),]
test[,-ncol(test)] <- Data[(nrow(train)+1):nrow(Data),]
names(train)
n <- nrow(train)
set.seed(100)
s <- sample(c(1:n), size=floor(n*0.1))
length(s)
rmse = function(ychap, y){
  sqrt(mean((ychap - y)^2))
}
######un modele de regression simple
Appliances=new$trend
which(is.na(Appliances))

m=mean(Appliances,na.rm=TRUE)
m
Appliances[which(is.na(Appliances))]=m
which(is.na(Appliances))





#####regression(2,3,train)

reg0 <- lm(Appliances ~ T_out + InstantF + Day_of_week, data=train[-s,])
length(reg0$coefficients)
summary(reg0)
####erreur échantillon apprentissage
rmse(y=train$Appliances[-s], ychap=reg0$fitted.values)

####erreur échantillon test
reg0.forecast <- predict(reg0, newdata=train[s,])
rmse(y=train$Appliances[s], ychap=reg0.forecast[s])
mean(reg0.forecast[s])
length(which(is.na(reg0.forecast[s])))
length(reg0.forecast[s])

plot(reg0)


a <- 1
b <- 144
plot(train$Appliances[a:b], type='l')
lines(reg0$fitted.values[a:b], col='red')

###stepwise selection

cov <- head(names(train)[-c(1,2)], 32) ####un peu long (instantF)
cov <- head(names(train)[-c(1,2)], 30) 
eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
eq
full.model <- lm(eq, data = train[-s,])
summary(full.model)

#######################################################################################
#############################Stepwise regression model
#######################################################################################

library(MASS)
step.model <- stepAIC(full.model, direction = "both", trace = TRUE, 
                      data=train,steps=50,k=2)

summary(step.model)
names(step.model)
step.model$anova

step.model.forecast <- predict(step.model, newdata=train[s,])
rmse(y=train$Appliances[s], ychap=step.model.forecast)

a <- 1
b <- 144
plot(train$Appliances[a:b], type='l')
lines(step.model$fitted.values[a:b], col='red')

#######################################################################################
#############################forward selection, test set
#######################################################################################

cov <- head(names(train)[-c(1,2)], 30)
eq_list <- list()
eq_list[[1]] <-  paste0("Appliances ~", paste0(cov[1], collapse='+'))
for(i in c(2:length(cov)))
{
  eq_list[[i]] <-  paste0("Appliances ~", paste0(cov[1:i], collapse='+'))
}

fitMod <- function(eq, subset)
{
  reg <- lm(eq, data=train[-subset,])
  return(reg)
}

###test.cor
reg_list <- lapply(eq_list, fitMod, subset=s)
length(reg_list)
reg_list_forecast <- lapply(reg_list, predict, newdata=train[s,])
rmse_list <- lapply(reg_list_forecast, rmse, y=train[s,]$Appliances)

plot(unlist(rmse_list))
which.min(rmse_list)




#######################Partie GAM
library(gam)
#g0<-gam(Appliances~lights,data=train)
g0=gam(Appliances ~ lights + T1 + RH_1 + T2 + RH_2 + T3 + 
  RH_3 + T4 + T5 + T6 + RH_6 + RH_7 + T8 + RH_8 + T9 + T_out + 
  RH_out + Windspeed + Visibility + Tdewpoint + NSM + Day_of_week, 
data = train)
#g0=step.model$call
#g0
summary(g0)


plot(g0$fitted.values)

g40$fitted.values

g0.forecast<-predict(g0,newData=Data)




step.model <- stepAIC(full.model, direction = "both", trace = TRUE, 
                      data=train,steps=50,k=2)


Gam.object <- gam(step.model, data=train)
g=gam.scope(train, response=1, smoother = "s", arg = TRUE, form = TRUE)


require(doMC)
registerDoMC(cores=2)
step_gam <- step.Gam(g0, scope=g,parallel = FALSE,steps=25)
step_gam$formula
gam(Appliances~lights,data=train)
nv=gam(step_gam$formula,data=train)
nv_predict=predict(nv, newdata=test)



