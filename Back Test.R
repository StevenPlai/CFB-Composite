library(cfbfastR)
source("Functions.R")

week <- CFBWeek()
lines <- data.frame()
for(i in 1:week){
  df <- cfbd_betting_lines(year = 2021, week = i) %>% mutate(week=i)
  lines <- bind_rows(lines,df)
}
ratings <- data.frame()
for(i in 1:week){
  df <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{i}Ratings.csv")) %>% mutate(week=i)
  ratings <- bind_rows(ratings,df)
}
neutrals <- data.frame()
for(i in 1:week){
  df <- cfbd_game_info(2021,week=i) %>% select(game_id,neutral_site) %>% mutate(week=i)
  neutrals <- bind_rows(neutrals,df)
}
lines$home_team <- recode(lines$home_team, "San José State" = "San Jose State")
lines$away_team <- recode(lines$away_team, "San José State" = "San Jose State")

spreads <- lines %>% filter(!is.na(home_score)) %>% group_by(game_id) %>%
  mutate(spread = as.numeric(spread)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            spread = round(mean(spread),digits=1),
            result = first(away_score-home_score),
            week = first(week)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf)) %>% 
  left_join(neutrals,by=c("game_id","week")) %>%
  left_join(ratings,by=c("home_team"="team","week")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(ratings,by=c("away_team"="team","week")) %>%
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
         sp_ev = (100*cover_prob)-(110*(1-cover_prob)),
         cover_team = if_else(result<spread,home_team,away_team),
         amount_won = if_else(pick_team==cover_team,.9090909090909092*sp_ev,-sp_ev),
         stake =((1.9090909090909092*cover_prob)-1)/(1.9090909090909092-1)*15) %>%
  select(home_team,away_team,home_pace,away_pace,poss,spread,"projected_margin"=pt_margin,
         pick_team,abs_diff,win_prob,cover_prob,cover_team,sp_ev,amount_won,week,result) %>%
  filter(sp_ev>0) %>%
  select(home_team,away_team,pick_team,spread,cover_team,sp_ev) 

results_detailed <- moneylines %>% mutate(v_error = abs(avg_spread-result),
                                       m_error = abs(projected_margin-result))

metrics <- data.frame(v_avg = mean(results_detailed$v_error),
                      m_avg = mean(results_detailed$m_error),
                      v_med = median(results_detailed$v_error),
                      m_med = median(results_detailed$m_error),
                      v_sd = sd(results_detailed$v_error),
                      m_sd = sd(results_detailed$m_error))

roi <- sum(moneylines$amount_won)/sum(moneylines$expected_value)
units <- sum(moneylines$amount_won)/mean(moneylines$expected_value)

moneylines <- lines %>% filter(!is.na(home_score)&!is.na(home_moneyline)) %>%
  group_by(game_id) %>% mutate(home_ml = as.numeric(home_moneyline),
                               away_ml = as.numeric(away_moneyline)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            home_ml=mean(home_ml),
            away_ml=mean(away_ml),
            result = first(away_score-home_score),
            week = first(week)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf)) %>%
  left_join(neutrals,by=c("game_id","week")) %>%
  left_join(ratings,by=c("home_team"="team","week")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(ratings,by=c("away_team"="team","week")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-2.5,digits=1)),
         home_wp = pnorm(.0001,mean=pt_margin, sd=14.5),
         away_wp = pnorm(.0001,mean=-pt_margin, sd=14.5),
         home_imp = if_else(home_ml<0,-home_ml/(-home_ml+100),
                            100/(home_ml+100)),
         away_imp = if_else(away_ml<0,-away_ml/(-away_ml+100),
                            100/(away_ml+100)),
         home_ev = if_else(home_ml>0,(home_ml*home_wp)-(100*(away_wp)),
                           (abs(home_ml)/100*home_wp)-(100*(away_wp))),
         away_ev = if_else(away_ml>0,(away_ml*away_wp)-(100*(home_wp)),
                           (abs(away_ml)/100*away_wp)-(100*(home_wp))),
         pick_team = if_else(home_ev>away_ev,home_team,away_team),
         pick_ml = if_else(home_ev>away_ev,home_ml,away_ml),
         ml_ev = if_else(home_ev>away_ev,home_ev,away_ev),
         winner = if_else(result>0,away_team,home_team),
         amount_won = if_else(pick_team==winner,if_else(pick_ml>0,pick_ml*ml_ev/100,ml_ev*100/-pick_ml),
                              -ml_ev)) %>%
  select(home_team,away_team,home_wp,away_wp,home_imp,away_imp,home_ml,away_ml,pick_team,ml_ev,pick_ml,
         winner, amount_won) %>% filter(ml_ev>0) %>%
  select(home_team,away_team,pick_team,winner,ml_ev,pick_ml,amount_won)

roi <- sum(moneylines$amount_won)/sum(moneylines$ml_ev)
units <- sum(moneylines$amount_won)/mean(moneylines$ml_ev)

bets <- left_join(spreads,moneylines,by=c("home_team","away_team","pick_team")) %>%
  mutate(ev = if_else(is.na(ml_ev),sp_ev,if_else(ml_ev>sp_ev,ml_ev,sp_ev)),
         pick_type = if_else(is.na(ml_ev),"spread",if_else(ml_ev>sp_ev,"moneyline","spread")),
         amt_won = if_else(pick_type=="moneyline",
                           if_else(winner==pick_team,
                                   if_else(pick_ml>0,pick_ml*ml_ev/100,ml_ev*100/-pick_ml),-ml_ev),
                           if_else(cover_team==pick_team,.9090909090909092*sp_ev,-sp_ev)))

roi <- sum(bets$amt_won)/sum(bets$ev)
units <- sum(bets$amt_won)/mean(bets$ev)
