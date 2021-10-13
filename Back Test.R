library(cfbfastR)
library(tidyverse)
library(stringi)
library(glue)
source("Functions.R")

start <- 1
cweek <- CFBWeek()
lines <- data.frame()
for(i in start:week){
  df <- cfbd_betting_lines(year = 2021, week = i) %>% mutate(week=i)
}
ratings <- data.frame()
for(i in start:cweek){
  df <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{i}Ratings.csv")) %>% mutate(week=i)
  ratings <- bind_rows(ratings,df)
}
neutrals <- data.frame()
for(i in start:cweek){
  df <- cfbd_game_info(2021,week=i) %>% select(game_id,neutral_site,conference_game) %>% mutate(week=i)
  neutrals <- bind_rows(neutrals,df)
}
lines$home_team <- recode(lines$home_team, "San José State" = "San Jose State")
lines$away_team <- recode(lines$away_team, "San José State" = "San Jose State")
hfa <- 2.5
sdev <- 16.09

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
  mutate(pp_margin = home_pp-away_pp)

pace <- spreads %>% select(game_id, home_pace, away_pace, home_pp, away_pp) 
ids <- pace %>% select(game_id)
pace <- pace %>% select(-game_id) %>% as.matrix()
poss <- predict(pace_model, pace)
wp <- predict(wp_model, pace)
extra <- data.frame(game_id = ids, poss = poss, home_wp = wp)

spreads <- spreads %>% mutate(poss = extra$poss,
                              home_wp = extra$home_wp,
                              away_wp = 1-home_wp,
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss),digits=1),
                             round((-pp_margin*poss)-hfa,digits=1)),
         home_wp2 = pnorm(.0001,mean=pt_margin, sd=sdev),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team),
         win_prob = if_else(pick_team==home_team,home_win_prob,away_win_prob),
         pick_team_margin = if_else(pick_team==home_team,-pt_margin,pt_margin),
         pick_team_spread = if_else(home_team==pick_team,-spread,spread),
         cover_prob = 1-pnorm(pick_team_spread,mean=pick_team_margin, sd=14.5),
         ev = (90.91*cover_prob)-(100*(1-cover_prob)),
         cover_team = if_else(result<spread,home_team,away_team),
         amount_won = if_else(pick_team==cover_team,.9090909090909092*ev,-ev),
         stake =((.9091*cover_prob)-(1-cover_prob))/(.9091))

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
  left_join(extra,by="game_id") %>% 
  mutate(pp_margin = home_pp-away_pp,
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss),digits=1),
                             round((-pp_margin*poss)-hfa,digits=1)),
         home_wp2 = pnorm(.0001,mean=pt_margin, sd=sdev),
         away_wp = 1-home_wp,
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
                              -ev))

bets <- bind_rows(spreads,moneylines) %>%
  mutate(stake = ev/1.52) %>% filter(stake>0)

current <- bets %>% filter(week==cweek)

roi <- sum(bets$amount_won)/sum(bets$ev)
units <- sum(bets$amount_won)/mean(bets$ev)
record <- bets %>% filter(is.na(home_ml)) %>% mutate(str=if_else(amount_won>0,1,0))
record <- sum(record$str)/nrow(record)

weekly <- bets %>% group_by(week) %>% summarise(roi = sum(amount_won)/sum(ev))

results_detailed <- spreads %>% mutate(error = abs(pt_margin-result),
                                        sd = (result-pt_margin)^2,
                                    bias = pt_margin-result,
                                    diff = abs(pt_margin))

metrics <- data.frame(avg = mean(results_detailed$error),
                      med = median(results_detailed$error),
                      sd = sqrt(sum(results_detailed$sd)/nrow(results_detailed)),
                      bias = mean(results_detailed$bias))


ggplot(data = bets, aes(x=pt_margin, y=home_wp)) + geom_point() + geom_smooth()


