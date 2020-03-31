

train_gbm=(train %>% select(-c(date,Day_of_week,DayType,lights,Visibility,InstantF,Posan,Month,Heure,BE_load_actual_entsoe_transparency,
                               BE_load_forecast_entsoe_transparency,BE_wind_onshore_capacity,BE_wind_onshore_capacity,BE_wind_onshore_generation_actual,BE_wind_onshore_profile,WeekStatus)))

library(gbm)
object_gbm=gbm(formula =my_formula ,
               
               data =train_gbm, distribution = "gaussian",
               , var.monotone = NULL, n.trees = 100,
               interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.05,
               bag.fraction = 0.5, train.fraction = 0.5, cv.folds = 20,
               keep.data = TRUE, verbose = TRUE,   n.cores = 4)
