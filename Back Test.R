library(cfbfastR)
library(tidyverse)
library(stringi)
library(glue)
library(caret)
source("Functions.R")

start <- 1
cweek <- CFBWeek()
lines <- data.frame()
for(i in start:week){
  df <- cfbd_betting_lines(year = 2021, week = i)
  lines <- bind_rows(lines, df)
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
broll <- 695.64

spreads <- lines %>% 
  filter(!is.na(spread)) %>% 
  mutate(spread = as.numeric(spread)) %>%
  group_by(game_id) %>%
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            spread = round(mean(spread),digits=1),
            result = first(away_score-home_score),
            week = first(week)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf))

moneylines <- lines %>% filter(!is.na(home_moneyline)) %>%
  mutate(home_ml = as.numeric(home_moneyline),
                               away_ml = as.numeric(away_moneyline)) %>%
  group_by(game_id) %>% 
  summarise(home_team=first(home_team),
            away_team=first(away_team),
            home_conf=first(home_conference),
            away_conf=first(away_conference),
            home_ml=mean(home_ml),
            away_ml=mean(away_ml),
            result = first(away_score-home_score),
            week = first(week)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf))

games <- bind_rows(spreads, moneylines) %>% left_join(neutrals,by=c("game_id","week")) %>%
  left_join(ratings,by=c("home_team"="team","week")) %>%
  rename("home_pp"=pp,"home_pace"=pace) %>% left_join(ratings,by=c("away_team"="team","week")) %>%
  rename("away_pp"=pp,"away_pace"=pace) %>% filter(!is.na(home_pp) & !is.na(away_pp)) %>%
  mutate(pp_margin = home_pp-away_pp)

ind <- sample(c(TRUE, FALSE), nrow(games), replace=TRUE, prob=c(0.5, 0.5))
test <- games
train <- games[!ind, ]

models <- games %>% select(game_id, home_pace, away_pace, home_pp, away_pp, conference_game,spread,result) %>% 
  mutate(conf_game = as.numeric(conference_game)) %>% select(-conference_game,-result) %>% group_by(game_id) %>% 
  summarise(home_pace = first(home_pace),away_pace=first(away_pace),home_pp=first(home_pp),away_pp=first(away_pp),
            spread=first(spread),conf_game=first(conf_game))
ids <- models %>% select(game_id)
pace <- models %>% select(-game_id) %>% as.matrix()
poss <- predict(pace_model, pace)
wp <- models %>% select(-game_id) %>% as.matrix()
wp <- predict(wp_model, wp)
cp <- models %>% select(-game_id) %>% as.matrix()
cp <- predict(cp_model, cp)
extra <- data.frame(game_id = ids, poss = poss, home_wp = wp, home_cp = cp)

spreads <- test %>% filter(!is.na(spread)) %>% left_join(extra, by="game_id") %>% 
  mutate(away_wp = 1-home_wp,
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss),digits=1),
                             round((-pp_margin*poss)-hfa,digits=1)),
         abs_diff = abs(as.numeric(spread)-pt_margin),
         pick_team = if_else(pt_margin<spread,home_team,away_team),
         wp = if_else(pick_team==home_team,home_wp,away_wp),
         cp = if_else(pick_team==home_team,home_cp,1-home_cp),
         pick_team_margin = if_else(pick_team==home_team,-pt_margin,pt_margin),
         pick_team_spread = if_else(home_team==pick_team,-spread,spread),
         ev = (90.91*cp)-(100*(1-cp)),
         cover_team = if_else(result<spread,home_team,away_team),
         amount_won = if_else(pick_team==cover_team,.9090909090909092*ev,-ev),
         stake =((.9091*cp)-(1-cp))/(.9091))

moneylines <- test %>% filter(!is.na(home_ml)) %>%
  left_join(extra,by="game_id") %>% 
  mutate(pp_margin = home_pp-away_pp,
         pt_margin = if_else(neutral_site==TRUE,round((-pp_margin*poss),digits=1),
                             round((-pp_margin*poss)-hfa,digits=1)),
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

bets <- bind_rows(spreads,moneylines) %>% filter(ev>0)

current <- bets %>% filter(week==cweek) %>% mutate(stake = ev/sum(ev),
                                                   stake = stake*broll)

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


ggplot(data = bets, aes(x=week, y=amount_won)) + geom_point() + geom_smooth()


