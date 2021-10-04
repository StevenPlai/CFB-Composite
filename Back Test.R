library(cfbfastR)
library(tidyverse)
library(stringi)
library(glue)
source("Functions.R")

start <- 1
week <- CFBWeek()
lines <- data.frame()
for(i in start:week){
  df <- cfbd_betting_lines(year = 2021, week = i) %>% mutate(week=i)
  lines <- bind_rows(lines,df)
}
ratings <- data.frame()
for(i in start:week){
  df <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{i}Ratings.csv")) %>% mutate(week=i)
  ratings <- bind_rows(ratings,df)
}
neutrals <- data.frame()
for(i in start:week){
  df <- cfbd_game_info(2021,week=i) %>% select(game_id,neutral_site,conference_game) %>% mutate(week=i)
  neutrals <- bind_rows(neutrals,df)
}
lines$home_team <- recode(lines$home_team, "San José State" = "San Jose State")
lines$away_team <- recode(lines$away_team, "San José State" = "San Jose State")
hfa <- 2.2
sdev <- 16.67

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
                             round((-pp_margin*poss*2)-hfa,digits=1)),
         home_win_prob = pnorm(.0001,mean=pt_margin, sd=sdev),
         away_win_prob = pnorm(.0001,mean=-pt_margin, sd=sdev),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team),
         win_prob = if_else(pick_team==home_team,home_win_prob,away_win_prob),
         pick_team_margin = if_else(pick_team==home_team,-pt_margin,pt_margin),
         pick_team_spread = if_else(home_team==pick_team,-spread,spread),
         cover_prob = 1-pnorm(pick_team_spread,mean=pick_team_margin, sd=14.5),
         ev = (90.91*cover_prob)-(100*(1-cover_prob)),
         cover_team = if_else(result<spread,home_team,away_team),
         amount_won = if_else(pick_team==cover_team,.9090909090909092*ev,-ev),
         stake =((.9091*cover_prob)-(1-cover_prob))/(.9091)) %>%
  filter(ev>0)

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
                             round((-pp_margin*poss*2)-hfa,digits=1)),
         home_wp = pnorm(.0001,mean=pt_margin, sd=sdev),
         away_wp = pnorm(.0001,mean=-pt_margin, sd=sdev),
         home_imp = if_else(home_ml<0,-home_ml/(-home_ml+100),
                            100/(home_ml+100)),
         away_imp = if_else(away_ml<0,-away_ml/(-away_ml+100),
                            100/(away_ml+100)),
         home_ev = if_else(home_ml>0,(home_ml*home_wp)-(100*(away_wp)),
                           (100/abs(home_ml)*home_wp)-(100*(away_wp))),
         away_ev = if_else(away_ml>0,(away_ml*away_wp)-(100*(home_wp)),
                           (100/abs(away_ml)*away_wp)-(100*(home_wp))),
         pick_team = if_else(home_ev>away_ev,home_team,away_team),
         pick_ml = if_else(home_ev>away_ev,home_ml,away_ml), 
         ev = if_else(home_ev>away_ev,home_ev,away_ev),
         winner = if_else(result>0,away_team,home_team),
         amount_won = if_else(pick_team==winner,if_else(pick_ml>0,pick_ml*ev/100,ev*100/-pick_ml),
                              -ev)) %>% filter(ev>0) 

bets <- bind_rows(spreads,moneylines) %>%
  mutate(stake = ev/1.52) %>% filter(stake>0)

roi <- sum(bets$amount_won)/sum(bets$ev)
units <- sum(bets$amount_won)/mean(bets$ev)
record <- bets %>% filter(is.na(home_ml)) %>% mutate(str=if_else(amount_won>0,1,0))
record <- sum(record$str)/nrow(record)

results_detailed <- spreads %>% mutate(error = abs(pt_margin-result),
                                        sd = abs(pt_margin-result)^2,
                                    bias = pt_margin-result,
                                    diff = abs(pt_margin))

metrics <- data.frame(avg = mean(results_detailed$error),
                      med = median(results_detailed$error),
                      sd = sqrt(sum(results_detailed$sd)/nrow(results_detailed)),
                      bias = mean(results_detailed$bias))

