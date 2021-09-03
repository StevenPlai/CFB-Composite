library(cfbfastR)

lines <- cfbd_betting_lines(year = 2021, week = 1)

teams <- cfbd_team_info(only_fbs=T) %>% select(school) 
teams$school <- recode(teams$school, "San José State" = "San Jose State")

d<- cfbd_drives(2020)
g <- d %>% group_by(game_id) %>% summarise(drives=n())
drive_constant <- mean(g$drives)/2

g2 <- d %>% group_by(offense) %>% summarise(minutes = mean(time_minutes_elapsed),
                                            seconds = mean(time_seconds_elapsed)) %>%
  mutate(time = (seconds/60)+minutes)
g2$offense <- recode(g2$offense, "San José State" = "San Jose State")

lines2 <- lines %>% filter(provider=="Bovada") %>% select(home_team,away_team,spread,
                                                         formatted_spread)
completed <- lines %>% filter(!is.na(home_score)) %>% group_by(game_id) %>%
  mutate(spread = as.integer(spread)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            spread = mean(spread),
            result = first(away_score-home_score)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf))

predictions <- left_join(lines2,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_time"=time) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_time"=time) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(spread = as.integer(spread),
         pp_margin = home_pp-away_pp,
         poss = 60/(home_time+away_time),
         pt_margin = (-pp_margin*poss*2)-2.5,
         abs_diff = abs(as.integer(spread)-pt_margin)) 

results <- left_join(completed,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_time"=time) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_time"=time) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(spread = as.integer(spread),
         pp_margin = home_pp-away_pp,
         poss = 60/(home_time+away_time),
         pt_margin = (-pp_margin*poss*2)-2.5,
         abs_diff = abs(as.integer(spread)-pt_margin),
         pick = if_else(pt_margin<spread,"home_cover","away_cover"),
         result_type = if_else(result<spread,"home_cover","away_cover"),
         win = if_else(pick==result_type,1,0)) %>% 
         select(home_team,away_team,"avg_spread"=spread,"projected_margin"=pt_margin,
                pick,result,result_type,win)
