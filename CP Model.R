library(xgboost)

data <- train %>% select(home_pace, away_pace, home_pp, away_pp, result, home_conf, away_conf,spread) %>% 
  mutate(conf_game = if_else(home_conf==away_conf, 1, 0),
         sresult = if_else(result>spread, 0,1)) %>% select(-home_conf, -away_conf, -result) %>% 
  filter(!is.na(spread))

tr_data <- as.matrix(data %>% select(-sresult))
tr_label <- as.matrix(data %>% select(sresult))

model_data <- xgb.DMatrix(data = tr_data, label = tr_label)

cp_model <- xgboost(data = model_data, nrounds=2000, objective="binary:logistic")
