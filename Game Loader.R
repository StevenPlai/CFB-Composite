library(cfbfastR)

lines <- cfbd_betting_lines(year = 2021, week = week)
neutrals <- cfbd_game_info(2021,week=week) %>% select(game_id,neutral_site)
lines$home_team <- recode(lines$home_team, "San José State" = "San Jose State")
lines$away_team <- recode(lines$away_team, "San José State" = "San Jose State")

teams <- cfbd_team_info(only_fbs=T) %>% select(school) 
teams$school <- recode(teams$school, "San José State" = "San Jose State")

d<- cfbd_drives(2021)

pace21 <- d %>% mutate(time_elapsed = (time_seconds_elapsed/60)+time_minutes_elapsed) %>%
  group_by(game_id) %>% summarise(team=first(offense),
                                  time=mean(time_elapsed)) %>%
  group_by(team) %>% summarise(team=first(team),
                               time=mean(time))

pace21$team <- recode(pace21$team, "San José State" = "San Jose State")

if(week<6) {
  
d<- cfbd_drives(2020)

pace20 <- d %>% mutate(time_elapsed = (time_seconds_elapsed/60)+time_minutes_elapsed) %>%
  group_by(game_id) %>% summarise(team=first(offense),
                                  time=mean(time_elapsed)) %>%
  group_by(team) %>% summarise(team=first(team),
                               time20=mean(time))

pace20$team <- recode(pace20$team, "San José State" = "San Jose State")

pace <- left_join(pace20,pace21,by="team") %>% 
  mutate(pace = if_else(is.na(time),
                        time20,
                        (time20+(time*(week-1)))/(week-1+1))) %>%
  select(team,pace)
} else{pace <- pace21 %>% select(team,"pace"=time)}

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
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-2.5,digits=1)),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team)) %>%
  select(home_team,away_team,"avg_spread"=spread,"projected_margin"=pt_margin,
         pick_team,abs_diff)

predictions_detailed <- left_join(tbp,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-2.5,digits=1)),
         home_win_prob = pnorm(.0001,mean=pt_margin, sd=14.5),
         away_win_prob = pnorm(.0001,mean=-pt_margin, sd=14.5),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team),
         win_prob = if_else(pick_team==home_team,home_win_prob,away_win_prob),
         pick_team_margin = if_else(pick_team==home_team,-pt_margin,pt_margin),
         pick_team_spread = if_else(home_team==pick_team,-spread,spread),
         cover_prob = 1-pnorm(pick_team_spread,mean=pick_team_margin, sd=14.5),
         expected_value = (100*cover_prob)-(110*(1-cover_prob)),
         kelly_stake =((1.9090909090909092*cover_prob)-1)/(1.9090909090909092-1)*15) %>%
  select(home_team,away_team,home_pace,away_pace,poss,"avg_spread"=spread,"projected_margin"=pt_margin,
         pick_team,abs_diff,win_prob,cover_prob,expected_value,kelly_stake)

bets <- predictions_detailed %>% filter(kelly_stake>1) %>% select(home_team,away_team,
                                                                  avg_spread,projected_margin,
                                                                  pick_team,kelly_stake)

results <- left_join(completed,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
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

