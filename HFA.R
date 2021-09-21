library(cfbfastR)
library(tidyverse)
#PBP


pbp_2019 <- cfbd_pbp_data(2019)
neutral_2019 <- cfbd_game_info(2019) %>% select(game_id,neutral_site,season)
pbp_2019 <- left_join(pbp_2019,neutral_2019,by="game_id") 
pbp_2018 <- cfbd_pbp_data(2018)
neutral_2018 <- cfbd_game_info(2018) %>% select(game_id,neutral_site,season)
pbp_2018 <- left_join(pbp_2018,neutral_2018,by="game_id") 
pbp_2017 <- cfbd_pbp_data(2017)
neutral_2017 <- cfbd_game_info(2017) %>% select(game_id,neutral_site,season)
pbp_2017 <- left_join(pbp_2017,neutral_2017,by="game_id") 
pbp_2016 <- cfbd_pbp_data(2016)
neutral_2016 <- cfbd_game_info(2016) %>% select(game_id,neutral_site,season)
pbp_2016 <- left_join(pbp_2016,neutral_2016,by="game_id") 
pbp_2015 <- cfbd_pbp_data(2015)
neutral_2015 <- cfbd_game_info(2015) %>% select(game_id,neutral_site,season)
pbp_2015 <- left_join(pbp_2015,neutral_2015,by="game_id") 
pbp_2014 <- cfbd_pbp_data(2014)
neutral_2014 <- cfbd_game_info(2014) %>% select(game_id,neutral_site,season)
pbp_2014 <- left_join(pbp_2014,neutral_2014,by="game_id") 
pbp_2013 <- cfbd_pbp_data(2013)
neutral_2013 <- cfbd_game_info(2013) %>% select(game_id,neutral_site,season)
pbp_2013 <- left_join(pbp_2013,neutral_2013,by="game_id") 
pbp_2012 <- cfbd_pbp_data(2012)
neutral_2012 <- cfbd_game_info(2012) %>% select(game_id,neutral_site,season)
pbp_2012 <- left_join(pbp_2012,neutral_2012,by="game_id") 
pbp_2011 <- cfbd_pbp_data(2011)
neutral_2011 <- cfbd_game_info(2011) %>% select(game_id,neutral_site,season)
pbp_2011 <- left_join(pbp_2011,neutral_2011,by="game_id") 
pbp_2010 <- cfbd_pbp_data(2010)
neutral_2010 <- cfbd_game_info(2010) %>% select(game_id,neutral_site,season)
pbp_2010 <- left_join(pbp_2010,neutral_2010,by="game_id") 
pbp_2009 <- cfbd_pbp_data(2009)
neutral_2009 <- cfbd_game_info(2009) %>% select(game_id,neutral_site,season)
pbp_2009 <- left_join(pbp_2009,neutral_2009,by="game_id") 
pbp_2008 <- cfbd_pbp_data(2008)
neutral_2008 <- cfbd_game_info(2008) %>% select(game_id,neutral_site,season)
pbp_2008 <- left_join(pbp_2008,neutral_2008,by="game_id") 
pbp_2007 <- cfbd_pbp_data(2007)
neutral_2007 <- cfbd_game_info(2007) %>% select(game_id,neutral_site,season)
pbp_2007 <- left_join(pbp_2007,neutral_2007,by="game_id") 
pbp_2006 <- cfbd_pbp_data(2006)
neutral_2006 <- cfbd_game_info(2006) %>% select(game_id,neutral_site,season)
pbp_2006 <- left_join(pbp_2006,neutral_2006,by="game_id") 
pbp_2005 <- cfbd_pbp_data(2005)
neutral_2005 <- cfbd_game_info(2005) %>% select(game_id,neutral_site,season)
pbp_2005 <- left_join(pbp_2005,neutral_2005,by="game_id") 

Plays <- bind_rows(pbp_2019,pbp_2018,pbp_2017,pbp_2016,pbp_2015,pbp_2014,
                   pbp_2013,pbp_2012,pbp_2011,pbp_2010,pbp_2009,pbp_2008,
                   pbp_2007,pbp_2006,pbp_2005)

drives <- Plays %>%   filter(!is.na(ppa)) %>% group_by(drive_id) %>%
  mutate(ppa = as.numeric(ppa),
         conference_game=if_else(offense_conference==defense_conference,T,F)) %>%
  summarise(home = first(home),
            away = first(away),
            posteam = first(offense_play),
            epa = sum(ppa),
            neutral_site = first(neutral_site),
            conference_game = first (conference_game),
            week = first(week),
            season = first(season.x),
            game_id = first(game_id))

teams <- drives %>% mutate(posteam_home = if_else(posteam==home,1,0)) %>%
  filter(conference_game==F&neutral_site==F) %>%
  group_by(posteam,game_id) %>%
  summarise(epa = mean(epa),
            drives = n(),
            posteam_home = first(posteam_home))

#Filter

pbp <- Plays %>% filter(neutral_site == FALSE, conference_game == TRUE, !is.na(home_EPA))

#Group By Game

G <- pbp  %>%  group_by(game_id) %>%
  summarise(
    home = first(home),
    away = first(away),
    home_EPA = mean(home_EPA),
    away_EPA = mean(away_EPA),
    neutral_site = first(neutral_site),
    conference_game = first (conference_game),
    week = first(week.x),
    season = first(season))

#Group By Team

H <- G %>% group_by(week, season) %>%
  summarise(
    H.EPA = mean(home_EPA)
  )

R <- G %>% group_by(week, season) %>%
  summarise(
    R.EPA = mean(away_EPA)
  )

HFA <- left_join(H, R, by = c("season" = "season")) %>%
  mutate(HFA = H.EPA-R.EPA)

#Export

write.csv(HFA,"/Users/stevenplaisance/desktop/HFA.csv", row.names = FALSE)



