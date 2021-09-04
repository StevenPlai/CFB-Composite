library(cfbfastR)

lines <- cfbd_betting_lines(year = 2021, week = 1)
neutrals <- cfbd_game_info(2021,week=1) %>% select(game_id,neutral_site)

teams <- cfbd_team_info(only_fbs=T) %>% select(school) 
teams$school <- recode(teams$school, "San José State" = "San Jose State")

d<- cfbd_drives(2020)
g <- d %>% group_by(game_id) %>% summarise(drives=n())
drive_constant <- mean(g$drives)/2

g2 <- d %>% mutate(time_elapsed = (time_seconds_elapsed/60)+time_minutes_elapsed) %>%
  group_by(game_id) %>% summarise(team=first(offense),
                                  time=mean(time_elapsed)) %>%
  group_by(team) %>% summarise(team=first(team),
                                  time=mean(time)) 

g2$team <- recode(g2$team, "San José State" = "San Jose State")

tbp <- lines %>% filter(is.na(home_score)) %>% group_by(game_id) %>%
  mutate(spread = as.numeric(spread)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            spread = round(mean(spread),digits=1),
            result = first(away_score-home_score)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf)) %>% 
  left_join(neutrals,by="game_id")

completed <- lines %>% filter(!is.na(home_score)&provider!="consensus") %>% group_by(game_id) %>%
  mutate(spread = as.numeric(spread)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            spread = round(mean(spread),digits=1),
            result = first(away_score-home_score)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf)) %>% 
  left_join(neutrals,by="game_id")

predictions <- left_join(tbp,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_time"=time) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_time"=time) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_time+away_time),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-2.5,digits=1)),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick = if_else(pt_margin<spread,"home_cover","away_cover")) %>%
  select(home_team,away_team,"avg_spread"=spread,"projected_margin"=pt_margin,
         pick)

predictions_detailed <- left_join(tbp,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_time"=time) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_time"=time) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_time+away_time),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-2.5,digits=1)),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick = if_else(pt_margin<spread,"home_cover","away_cover")) %>%
  select(home_team,away_team,home_time,away_time,poss,"avg_spread"=spread,"projected_margin"=pt_margin,
         pick,abs_diff)

results <- left_join(completed,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_time"=time) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_time"=time) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_time+away_time),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-2.5,digits=1)),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick = if_else(pt_margin<spread,"home_cover","away_cover"),
         result_type = if_else(result<spread,"home_cover","away_cover"),
         win = if_else(pick==result_type,1,0)) %>% 
         select(home_team,away_team,"avg_spread"=spread,"projected_margin"=pt_margin,
                pick,result,result_type,win)

results_detailed <- results %>% mutate(v_error = abs(avg_spread-result),
                                       m_error = abs(projected_margin-result))

metrics <- data.frame(v_avg = mean(results_detailed$v_error),
                      m_avg = mean(results_detailed$m_error),
                      v_med = median(results_detailed$v_error),
                      m_med = median(results_detailed$m_error),
                      v_sd = sd(results_detailed$v_error),
                      m_sd = sd(results_detailed$m_error))

moneylines <- lines %>% filter(is.na(home_score)&!is.na(home_moneyline)) %>%
  group_by(game_id) %>% mutate(spread = as.numeric(spread)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            spread = mean(spread),
            result = first(away_score-home_score)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf))

