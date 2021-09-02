library(cfbfastR)

lines <- cfbd_betting_lines(year = 2021, week = 1)

teams <- cfbd_team_info(only_fbs=T) %>% select(school) 
teams$school <- recode(teams$school, "San José State" = "San Jose State")
