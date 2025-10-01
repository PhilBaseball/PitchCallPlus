##### MODEL ######

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



#### MODEL Data ####

modeldata1<- fread("ModelSecond23-25.csv")

modeldata1$pitch_type<- as.factor(modeldata1$pitch_type)
modeldata1$pitch_hand<- as.factor(modeldata1$pitch_hand)
modeldata1$zone<- as.factor(modeldata1$zone)
modeldata1$zonem1<- as.factor(modeldata1$zonem1)
modeldata1$zonem2<- as.factor(modeldata1$zonem2)
modeldata1$zonem3<- as.factor(modeldata1$zonem3)
modeldata1$zonem4<- as.factor(modeldata1$zonem4)
modeldata1$pitchm1<- as.factor(modeldata1$pitchm1)
modeldata1$pitchm2<- as.factor(modeldata1$pitchm2)
modeldata1$pitchm3<- as.factor(modeldata1$pitchm3)
modeldata1$pitchm4<- as.factor(modeldata1$pitchm4)
modeldata1$bat_side<- as.factor(modeldata1$bat_side)
modeldata1$pitch_hand<- as.factor(modeldata1$pitch_hand)







## Catcher RV model ##
modeldata1<- na.omit(modeldata1)
modeldata1<- modeldata1 %>% filter(CumulativeWobahand < 100 & CumulativeWobaZone <100 & CumulativeWobaPitchType <100)

#### XGBOOST MODEL ####

library(xgboost) #for fitting the xgboost model
library(caret) #for general data preparation and model fitting


#make this example reproducible
set.seed(123456)

modeldata3<- modeldata1 %>% select(-c("V1", "fielder_2_id", "year"))

#split into training (80%) and testing set (20%)
parts = createDataPartition(modeldata3$delta_pitcher_run_exp, p = .8, list = F)
train = modeldata3[parts, ]
test = modeldata3[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[, -9])
train_y = data.matrix(train[,9])

#define predictor and response variables in testing set
test_x = data.matrix(test[, -9])
test_y = data.matrix(test[, 9])

#fit XGBoost model to training set
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 1000)

#define final model change rounds for what the data tell you
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 506 , verbose = 0)

#use model to make predictions on test data
pred_y = predict(final, xgb_test)

#measure prediction accuracy
mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse

### Predictions ###

modeldata1$pred<- predict(final, data.matrix(modeldata1[,-c(1,2,3,12)]))

leader<- modeldata1 %>% dplyr::group_by(year,fielder_2_id)%>% dplyr::summarize(
  pitchcallrv = sum(delta_pitcher_run_exp,na.rm = T),
  predpitchcallrv = sum(pred, na.rm =T),
  diff = sum(pred, na.rm =T)- sum(delta_pitcher_run_exp,na.rm = T)
) 



write.csv(leader, "PitchCallLeaders.csv")




# Initialize an empty data frame
combined_results <- data.frame(t(1:11))
colnames(combined_results)<- colnames(t)
combined_results<- combined_results[-1,]

catchers<- unique(leader$fielder_2_id)

for (i in 1:length(catchers)) {
  
  name<- playername_lookup(catchers[i])
  combined_results <- rbind(combined_results, name)
  
  
}

combined_results$Name<- paste0(combined_results$name_first, " ", combined_results$name_last)
combined_results<- combined_results %>% select(Name, key_mlbam ) #### See which one ###
colnames(combined_results)<- c("Catcher" , "fielder_2_id") 




LeaderBoard<- merge(leader, combined_results, by = "fielder_2_id" )


lg_pitchcallvalue2023<- mean(LeaderBoard$predpitchcallrv[LeaderBoard$year == 2023], na.rm = T)
lg_pitchcallvalue2024<- mean(LeaderBoard$predpitchcallrv[LeaderBoard$year == 2024], na.rm = T)
lg_pitchcallvalue2025<- mean(LeaderBoard$predpitchcallrv[LeaderBoard$year == 2025], na.rm = T)


sdlg_pitchcallvalue2023<- sd(LeaderBoard$predpitchcallrv[LeaderBoard$year == 2023], na.rm = T)
sdlg_pitchcallvalue2024<- sd(LeaderBoard$predpitchcallrv[LeaderBoard$year == 2024], na.rm = T)
sdlg_pitchcallvalue2025<- sd(LeaderBoard$predpitchcallrv[LeaderBoard$year == 2025], na.rm = T)


LeaderBoard$PitchCall_Plus = ifelse(LeaderBoard$year == 2023, 100 + 10 *((LeaderBoard$predpitchcallrv - lg_pitchcallvalue2023)/sdlg_pitchcallvalue2023),
                             ifelse(LeaderBoard$year == 2024, 100 + 10 *((LeaderBoard$predpitchcallrv - lg_pitchcallvalue2024)/sdlg_pitchcallvalue2024),       
                             ifelse(LeaderBoard$year == 2025, 100 + 10 *((LeaderBoard$predpitchcallrv - lg_pitchcallvalue2025)/sdlg_pitchcallvalue2025), NA)))



LeaderBoard2<- LeaderBoard[,c(2,6,7)]

LeaderBoard2$PitchCall_Plus<- round(LeaderBoard2$PitchCall_Plus, 0)


write.csv(LeaderBoard2, "PitchCallingPlusLeaderBoard.csv")


#LeaderBoard$PitchCall_Plus = ifelse(LeaderBoard$year == 2023, LeaderBoard$predpitchcallrv / lg_pitchcallvalue2023 *100,
#                        ifelse(LeaderBoard$year == 2024, LeaderBoard$predpitchcallrv / lg_pitchcallvalue2024 *100,       
#                        ifelse(LeaderBoard$year == 2025, LeaderBoard$predpitchcallrv / lg_pitchcallvalue2025 *100, NA)))




################ RF #########################


