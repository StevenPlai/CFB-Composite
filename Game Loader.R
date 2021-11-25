library(cfbfastR)
library(tidyverse)
library(stringi)
library(glue)
source("Functions.R")

week <- CFBWeek()
lines <- cfbd_betting_lines(year = 2021, week = week)
neutrals <- cfbd_game_info(2021,week=week) %>% select(game_id,neutral_site)
lines$home_team <- recode(lines$home_team, "San José State" = "San Jose State")
lines$away_team <- recode(lines$away_team, "San José State" = "San Jose State")
summary <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{week}Ratings.csv"))
hfa <- 2.15
sd <- 16.15
broll <- 897.49
`%notin%` <- Negate(`%in%`)

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

spreads <- left_join(tbp,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-hfa,digits=1)),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team)) %>%
  select(home_team,away_team,"avg_spread"=spread,"projected_margin"=pt_margin,
         pick_team,abs_diff)
 
spreads2 <- left_join(tbp,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-hfa,digits=1)),
         home_win_prob = pnorm(.0001,mean=pt_margin, sd=sd),
         away_win_prob = pnorm(.0001,mean=-pt_margin, sd=sd),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team),
         win_prob = if_else(pick_team==home_team,home_win_prob,away_win_prob),
         pick_team_margin = if_else(pick_team==home_team,-pt_margin,pt_margin),
         pick_team_spread = if_else(home_team==pick_team,-spread,spread),
         cover_prob = 1-pnorm(pick_team_spread,mean=pick_team_margin, sd=14.5),
         ev = (90.909091*cover_prob)-(100*(1-cover_prob)),
         stake =((1.9090909090909092*cover_prob)-1)/(1.9090909090909092-1)*15)

spreads3 <- spreads2 %>%
  select(home_team,away_team,
         pick_team,"pick"=spread,ev) %>%
  mutate(type = "spread")

results <- left_join(completed,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-hfa,digits=1)),
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
  group_by(game_id) %>% mutate(home_ml = as.numeric(home_moneyline),
                               away_ml = as.numeric(away_moneyline)) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            home_ml=mean(home_ml),
            away_ml=mean(away_ml),
            result = first(away_score-home_score)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf)) %>%
  select(-result) %>%
  left_join(neutrals,by="game_id")

ml_predictions <- left_join(moneylines,summary,by=c("home_team"="team")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(summary,by=c("away_team"="team")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp,
         poss = 60/(home_pace+away_pace),
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss*2),digits=1),
                             round((-pp_margin*poss*2)-hfa,digits=1)),
         home_wp = pnorm(.0001,mean=pt_margin, sd=sd),
         away_wp = pnorm(.0001,mean=-pt_margin, sd=sd),
         home_imp = if_else(home_ml<0,-home_ml/(-home_ml+100),
                            100/(home_ml+100)),
         away_imp = if_else(away_ml<0,-away_ml/(-away_ml+100),
                            100/(away_ml+100)),
         home_ev = if_else(home_ml>0,(home_ml*home_wp)-(100*(away_wp)),
                           ((10000/abs(home_ml))*home_wp)-(100*(away_wp))),
         away_ev = if_else(away_ml>0,(away_ml*away_wp)-(100*(home_wp)),
                           ((10000/abs(away_ml))*away_wp)-(100*(home_wp))),
         pick_team = if_else(home_ev>away_ev,home_team,away_team),
         ev = if_else(home_ev>away_ev,home_ev,away_ev),
         pick_ml = if_else(home_ev>away_ev,home_ml,away_ml)) %>%
  select(home_team,away_team,pick_team,"pick"=pick_ml,ev) %>%
  mutate(type = "moneyline")

bets <- bind_rows(spreads3,ml_predictions) %>% filter(ev>0) %>%
  mutate(share = ev/sum(ev),
         stake = round(share*broll,2)) %>% filter(stake>0.49) %>%
  mutate(pick=if_else(type=="spread",if_else(pick_team==home_team,if_else(pick<0,
                                             glue("{pick_team} {pick}"),
                                             glue("{pick_team} +{pick}")),
                                             if_else(pick<0,
                                                     glue("{pick_team} +{-pick}"),
                                                     glue("{pick_team} -{pick}"))),
                      if_else(pick>0,
                              glue("{pick_team} +{pick}"),
                              glue("{pick_team} {pick}")))) %>%
  select(home_team,away_team,pick,type,stake)
  
sum(bets$stake)
