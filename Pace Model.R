library(cfbfastR)
library(xgboost)

week <- CFBWeek()
weekly_pace <- data.frame()
for(i in 1:week){
  df <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{i}Ratings.csv")) %>% select(-pp) %>%
    mutate(week=i)
  weekly_pace <- bind_rows(weekly_pace, df)
}
actuals <- cfbd_drives(2021) %>% filter(!is.na(offense_conference),!is.na(defense_conference)) %>% 
  group_by(game_id) %>% summarise(home_team = first(offense[is_home_offense==T]),
                                  away_team = first(offense[is_home_offense==F]),
                                  drives = n()) %>% select(drives, game_id)

base <- train %>% select(game_id, home_team, away_team, home_pace, away_pace, home_pp, away_pp, home_conf, away_conf, spread) %>% 
  left_join(actuals, by="game_id")  %>% filter(!is.na(drives), !is.na(spread)) %>% 
  mutate(conf_game = if_else(home_conf==away_conf, 1, 0))


model_data <- base %>% select(home_pace, away_pace, home_pp, away_pp, drives,spread,conf_game) %>% filter(!is.na(drives))

tr_data <- as.matrix(model_data %>% select(-drives))
tr_label <- as.matrix(model_data %>% select(drives))

model_data <- xgb.DMatrix(data = tr_data, label = tr_label)

pace_model <- xgboost(data = model_data, nrounds=2000)

pred <- predict(pace_model, te_data)
err <- mean(abs(te_label - pred))

data <- as.matrix(model_data %>% select(-drives))
label <- as.matrix(model_data %>% select(drives))
model_data <- xgb.DMatrix(data = data, label = label)
pace_model <- xgboost(data = model_data, nrounds=2000)

importance_matrix <- xgb.importance(model = pace_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
