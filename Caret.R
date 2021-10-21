library(caret)
library(tidyverse)

data <- bets %>% select(spread, result, neutral_site, conference_game, home_pp, home_pace, away_pp, away_pace) %>% 
  filter(!is.na(spread),!is.na(result),neutral_site==F) %>% select(-neutral_site) %>% mutate(result = as.factor(if_else(result>0, 0, 1)))

summary(data)

featurePlot(x=data$spread,
            y= data$result,
            plot = "box")

nearZeroVar(data, freqCut = 80/20, uniqueCut = 10)
 
set.seed(16)
trainIndex <- createDataPartition(data$result, p=.7, list=F)
trainingSet <- data[trainIndex,]
testSet <- data[-trainIndex,]
