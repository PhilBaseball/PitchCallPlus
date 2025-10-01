#### SCRAPE MLB DATA MULTIPLE YEARS ###
{
setwd("C:/RStudioProjects/AthleteLab")

library(sabRmetrics)
library(randomForest)
library(caret)
library(tidyr)
library(baseballr)
library(tidyverse)
library(hexbin)
library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(GeomMLBStadiums)
library(RColorBrewer)
library(ggpubr)
library(caret)
library(randomForest)
library(broom)
library(shiny)
library(ggforce)
library("duckdb")
}

### If you want to try it for a single day ##  
#data_baseballsavant <- sabRmetrics::download_baseballsavant(
#  start_date = "2025-07-01",
#  end_date = "2025-07-01"
#)  

# To Download more games (Whole Season) (Will take close to 15 minutes to load) ## since 2021 season
cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant23 <- sabRmetrics::download_baseballsavant(
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)

# To Download more games (Whole Season) (Will take close to 15 minutes to load) ## since 2021 season
cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant24 <- sabRmetrics::download_baseballsavant(
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)

# To Download more games (Whole Season) (Will take close to 15 minutes to load) ## since 2021 season
cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant25 <- sabRmetrics::download_baseballsavant(
  start_date = "2025-01-01",
  end_date = "2025-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)


Savant2325<- rbind(data_baseballsavant23,data_baseballsavant24,data_baseballsavant25)

write.csv(Savant2325, "Savant2325.csv")

modeldata1<- fread("Savant2325.csv")

modeldata1<- modeldata1 %>%
  arrange(year, game_id, event_index, pitch_number)

#modeldata1<- filter(modeldata1 %>% filter(year >= 2024)) %>% select(-year)

## Pitch Before ###
modeldata1$pitchm1 = ifelse(modeldata1$pitch_number == 1 , "-" , lag(modeldata1$pitch_type))
modeldata1$zonem1 = ifelse(modeldata1$pitch_number == 1 , "-" , lag(modeldata1$zone))

## 2 Pitches Before ##
modeldata1$pitchm2 = ifelse(modeldata1$pitch_number %in% c(1,2) , "-" , lag(modeldata1$pitch_type, n = 2))
modeldata1$zonem2 = ifelse(modeldata1$pitch_number %in% c(1,2) , "-" , lag(modeldata1$zone, n = 2))

## 3 Pitches Before ##
modeldata1$pitchm3 = ifelse(modeldata1$pitch_number %in% c(1,2,3) , "-" , lag(modeldata1$pitch_type, n = 3))
modeldata1$zonem3 = ifelse(modeldata1$pitch_number %in% c(1,2,3) , "-" , lag(modeldata1$zone, n = 3))

## 4 Pitches Before ##
modeldata1$pitchm4 = ifelse(modeldata1$pitch_number %in% c(1:4) , "-" , lag(modeldata1$pitch_type, n = 4))
modeldata1$zonem4 = ifelse(modeldata1$pitch_number %in% c(1:4) , "-" , lag(modeldata1$zone, n = 4))

### Cumu only by hand ###
Game_StatsHand <- modeldata1  %>% 
  dplyr::group_by(year,game_id, batter_id, pitch_hand)%>%
  dplyr::summarise('PA' = sum(woba_denom, na.rm = T),
                   "Woba" = sum(woba_value, na.rm = T)
  ) %>%  mutate("CumuPA"= cumsum(PA),
                "CumuWoba" = cumsum(Woba)
  ) %>%
  mutate(
    "CumulativeWobahand" = round(CumuWoba/CumuPA,3 )
  ) %>% arrange(game_id)



### Cumu by zone ###
Game_StatsZone <- modeldata1  %>% 
  dplyr::group_by(year,game_id, batter_id, pitch_hand, zone)%>%
  dplyr::summarise('PA' = sum(woba_denom, na.rm = T),
                   "Woba" = sum(woba_value, na.rm = T)
  ) %>%  mutate("CumuPA"= cumsum(PA),
                "CumuWoba" = cumsum(Woba)
  ) %>%
  mutate(
         "CumulativeWobaZone" = round(CumuWoba/CumuPA,3 )
  ) %>% arrange(game_id)

### Cumu by pitch_type ###

Game_StatsPC <- modeldata1  %>% 
  dplyr::group_by(year,game_id, batter_id, pitch_hand ,pitch_type)%>%
  dplyr::summarise('PA' = sum(woba_denom, na.rm = T),
                   "Woba" = sum(woba_value, na.rm = T)
  ) %>%  mutate("CumuPA"= cumsum(PA),
                "CumuWoba" = cumsum(Woba)
  ) %>%
  mutate(
    "CumulativeWobaPitchType" = round(CumuWoba/CumuPA,3 )
  ) %>% arrange(game_id)




Total_Data<- merge(modeldata1,Game_StatsHand, by = c("year","game_id", "batter_id","pitch_hand"))
Total_Data<- merge(Total_Data, Game_StatsZone, by = c("year","game_id", "batter_id","pitch_hand", "zone"))
Total_Data<- merge(Total_Data, Game_StatsPC, by = c("year","game_id", "batter_id","pitch_hand", "pitch_type"))


colnames(Total_Data)

Total_Data2<- Total_Data %>% select(year,fielder_2_id,pitch_number,bat_side,pitch_hand, outs, balls, strikes,
                    pitch_type,zone,delta_pitcher_run_exp,n_thruorder_pitcher,pitchm1,pitchm2,pitchm3,pitchm4,zonem1,zonem2,zonem3,zonem4, CumulativeWobahand, CumulativeWobaZone, CumulativeWobaPitchType)


write.csv(Total_Data2, "ModelSecond23-25.csv")

modeldata1<- filter(modeldata1 %>% filter(year >= 2024)) %>% select(-year)

## Pitch Before ###
modeldata1$pitchm1 = ifelse(modeldata1$pitch_number == 1 , "-" , lag(modeldata1$pitch_type))
modeldata1$zonem1 = ifelse(modeldata1$pitch_number == 1 , "-" , lag(modeldata1$zone))

## 2 Pitches Before ##
modeldata1$pitchm2 = ifelse(modeldata1$pitch_number %in% c(1,2) , "-" , lag(modeldata1$pitch_type, n = 2))
modeldata1$zonem2 = ifelse(modeldata1$pitch_number %in% c(1,2) , "-" , lag(modeldata1$zone, n = 2))

## 3 Pitches Before ##
modeldata1$pitchm3 = ifelse(modeldata1$pitch_number %in% c(1,2,3) , "-" , lag(modeldata1$pitch_type, n = 3))
modeldata1$zonem3 = ifelse(modeldata1$pitch_number %in% c(1,2,3) , "-" , lag(modeldata1$zone, n = 3))

## 4 Pitches Before ##
modeldata1$pitchm4 = ifelse(modeldata1$pitch_number %in% c(1:4) , "-" , lag(modeldata1$pitch_type, n = 4))
modeldata1$zonem4 = ifelse(modeldata1$pitch_number %in% c(1:4) , "-" , lag(modeldata1$zone, n = 4))



  ### First Model Data with 24 and 25 season ###

FullModelData<- modeldata1 %>% select(pitch_number,bat_side,pitch_hand, outs, balls, strikes,
                                      pitch_type,zone,delta_pitcher_run_exp,n_thruorder_pitcher,pitchm1,pitchm2, pitchm3, pitchm4,zonem1,zonem2,zonem3,zonem4)






# to start an in-memory database
#con <- dbConnect(duckdb())
#dbWriteTable(con, "Savant2325", Savant2325)
#modeldata1<-  dbGetQuery(con, "SELECT delta_pitcher_run_exp,year,bat_side,pitch_hand,fielder_2_id,inning,outs,balls,strikes,pitch_type, n_thruorder_pitcher, zone FROM Savant2325")


write.csv(FullModelData, "ModelFullData1.csv")



#### MODEL Data ####

modeldata1<- fread("ModelFullData1.csv")

## Catcher RV model ##
modeldata1<- na.omit(modeldata1)

modeldata1<- modeldata1 %>%  select(-V1)

set.seed(123456789) 
training_data_rows <- floor(0.80 * nrow(modeldata1))          

training_indices <- sample(c(1:nrow(modeldata1)), training_data_rows)

training<- modeldata1[training_indices,] 
testing <- modeldata1[-training_indices,]



#### Random Forest Model ####


train_ctrl <- trainControl(method="cv", # type of resampling in this case Cross-Validated
                           number=2, # number of folds
                           search = "random", # we are performing a "random
)

#RV_Mod <- train(delta_pitcher_run_exp ~ .,
#                  data = training,
#                  method = "rf", # this will use the randomForest::randomForest function
#                  #metric = "Accuracy", # which metric should be optimized for 
#                  trControl = train_ctrl,
#                  # options to be passed to randomForest
#                  ntree = 5,
#                  keep.forest=TRUE,
#                  importance=TRUE) 


RV_MOD <- randomForest(delta_pitcher_run_exp ~ ., data = train_data, ntree = 15, mtry = 2, importance = TRUE)

RV_MOD
randomForest::varImpPlot(RV_MOD$finalModel)
pred <- predict(RV_MOD, test_data)

test_data$run_exp_pred<- predict(RV_MOD, test_data)

#measure prediction accuracy
mean((test_data$delta_pitcher_run_exp - test_data$run_exp_pred)^2) #mse
caret::MAE(test_data$delta_pitcher_run_exp, test_data$run_exp_pred) #mae
caret::RMSE(test_data$delta_pitcher_run_exp, test_data$run_exp_pred) #rmse

saveRDS(RV_Mod, file = "RV_Mod.rds")


modeldata1$pred_delta<- predict(RV_MOD, modeldata1)

Full<- fread("Savant2325.csv")

modeldata2<- Full %>%
  arrange(game_id, event_index, pitch_number)


## Pitch Before ###
modeldata2$pitchm1 = ifelse(modeldata2$pitch_number == 1 , "-" , lag(modeldata2$pitch_type))
modeldata2$zonem1 = ifelse(modeldata2$pitch_number == 1 , "-" , lag(modeldata2$zone))

## 2 Pitches Before ##
modeldata2$pitchm2 = ifelse(modeldata2$pitch_number %in% c(1,2) , "-" , lag(modeldata2$pitch_type, n = 2))
modeldata2$zonem2 = ifelse(modeldata2$pitch_number %in% c(1,2) , "-" , lag(modeldata2$zone, n = 2))

## 3 Pitches Before ##
modeldata2$pitchm3 = ifelse(modeldata2$pitch_number %in% c(1,2,3) , "-" , lag(modeldata2$pitch_type, n = 3))
modeldata2$zonem3 = ifelse(modeldata2$pitch_number %in% c(1,2,3) , "-" , lag(modeldata2$zone, n = 3))

## 4 Pitches Before ##
modeldata2$pitchm4 = ifelse(modeldata2$pitch_number %in% c(1:4) , "-" , lag(modeldata2$pitch_type, n = 4))
modeldata2$zonem4 = ifelse(modeldata2$pitch_number %in% c(1:4) , "-" , lag(modeldata2$zone, n = 4))


modeldata2$pred_delta<- predict(RV_MOD, modeldata2)



leader<- modeldata2 %>% dplyr::group_by(fielder_2_id, year)%>% dplyr::summarize(
  pitchcallrv = sum(delta_pitcher_run_exp,na.rm = T),
  predpitchcallrv = sum(pred_delta, na.rm =T),
  diff = sum(pred_delta, na.rm =T)- sum(delta_pitcher_run_exp,na.rm = T)
) 


leader$name<- playername_lookup(leader$fielder_2_id)

name<- c(rep(NA, 311))

for (i in 1:311) {

  name[i]<- playername_lookup(leader$fielder_2_id[i])
  
    
}


#### XGBOOST MODEL ####

library(xgboost) #for fitting the xgboost model
library(caret) #for general data preparation and model fitting


#view the structure of the data

modeldata1<- na.omit(modeldata1)

#make this example reproducible
set.seed(123456)

#split into training (80%) and testing set (20%)
parts = createDataPartition(modeldata1$delta_pitcher_run_exp, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(training[, -9])
train_y = training[,9]

#define predictor and response variables in testing set
test_x = data.matrix(testing[, -9])
test_y = testing[, 9]

#fit XGBoost model to training set
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

#define final model change rounds for what the data tell you
final = xgboost(data = xgb_train, max.depth = 3, nrounds = , verbose = 0)

#use model to make predictions on test data
pred_y = predict(final, xgb_test)

#measure prediction accuracy
mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse





set.seed(123456)
train_indices <- sample(1:nrow(modeldata1), 0.8 * nrow(modeldata1))
train_data <- modeldata1[train_indices, ]
test_data <- modeldata1[-train_indices, ]

# Separate features and target variable
train_matrix <- as.matrix(train_data[, -9])  
train_label <- train_data$delta_pitcher_run_exp

test_matrix <- as.matrix(test_data[, -9])
test_label <- test_data$delta_pitcher_run_exp

# Create DMatrix objects for training and testing data
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)






