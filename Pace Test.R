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
projections <- bets %>% select(game_id, home_team, away_team, poss)
base <- bets %>% select(game_id, home_team, away_team, home_pace, away_pace, home_pp, away_pp, home_conf, away_conf) %>% 
  left_join(actuals, by="game_id") 

eval <- full_join(actuals, projections, by=c("game_id","home_team", "away_team")) %>% 
  mutate(proj = (poss*2)-1.931622,
         error = abs(drives-proj),
         bias = drives-proj) %>% filter(!is.na(poss),!is.na(drives))
mean(eval$error)

model_data <- base %>% select(home_pace, away_pace, home_pp, away_pp, drives) %>% filter(!is.na(drives))
ind <- sample(c(TRUE, FALSE), nrow(model_data), replace=TRUE, prob=c(0.5, 0.5))
test <- model_data[ind, ]
train <- model_data[!ind, ]

tr_data <- as.matrix(train %>% select(-drives))
tr_label <- as.matrix(train %>% select(drives))

te_data <- as.matrix(test %>% select(-drives))
te_label <- as.matrix(test %>% select(drives))

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
