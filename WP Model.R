library(cfbfastR)
library(xgboost)

week <- CFBWeek()
data <- bets %>% select(home_pace, away_pace, home_pp, away_pp, result, home_wp, home_conf, away_conf) %>% 
  mutate(result = if_else(result>0, 0,1),
         conf_game = if_else(home_conf==away_conf, 1, 0)) %>% select(-home_conf, -away_conf) 

eval <- data %>% filter(!is.na(home_wp)) %>% mutate(error = abs(home_wp-result))
mean(eval$error)

ind <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.5, 0.5))
test <- data[ind, ]
train <- data[!ind, ]

tr_data <- as.matrix(train %>% select(-result))
tr_label <- as.matrix(train %>% select(result))

te_data <- as.matrix(test %>% select(-result))
te_label <- as.matrix(test %>% select(result))

model_data <- xgb.DMatrix(data = tr_data, label = tr_label)

wp_model <- xgboost(data = model_data, nrounds=2000, objective="binary:logistic")

pred <- predict(wp_model, te_data)
err <- mean(abs(te_label - pred))

model_data <- data %>% select(-home_wp)
data <- as.matrix(model_data %>% select(-result))
label <- as.matrix(model_data %>% select(result))
model_data <- xgb.DMatrix(data = data, label = label)
wp_model <- xgboost(data = model_data, nrounds=2000, objective="binary:logistic")
importance_matrix <- xgb.importance(model = wp_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
