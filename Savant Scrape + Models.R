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

