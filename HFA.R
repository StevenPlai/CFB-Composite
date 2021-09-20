#PBP

pbp_2020 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2020, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2020 <- bind_rows(pbp_2020, df)
}

pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
}

pbp_2018 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2018, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2018 <- bind_rows(pbp_2018, df)
}

pbp_2017 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2017, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2017 <- bind_rows(pbp_2017, df)
}

pbp_2016 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2016, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2016 <- bind_rows(pbp_2016, df)
}

pbp_2015 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2015, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2015 <- bind_rows(pbp_2015, df)
}

pbp_2014 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2014, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2014 <- bind_rows(pbp_2014, df)
}

pbp_2013 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2013, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2013 <- bind_rows(pbp_2013, df)
}

pbp_2012 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2012, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2012 <- bind_rows(pbp_2012, df)
}

pbp_2011 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2011, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2011 <- bind_rows(pbp_2011, df)
}

pbp_2010 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2010, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2010 <- bind_rows(pbp_2010, df)
}

pbp_2009 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2009, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2009 <- bind_rows(pbp_2009, df)
}

pbp_2008 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2008, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2008 <- bind_rows(pbp_2008, df)
}

pbp_2007 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2007, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2007 <- bind_rows(pbp_2007, df)
}

pbp_2006 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2006, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2006 <- bind_rows(pbp_2006, df)
}

pbp_2005 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2005, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2005 <- bind_rows(pbp_2005, df)
}

#Games

games_20 <- cfb_game_info(2020)

games_19 <- cfb_game_info(2019)

games_18 <- cfb_game_info(2018)

games_17 <- cfb_game_info(2017)

games_16 <- cfb_game_info(2016)

games_15 <- cfb_game_info(2015)

games_14 <- cfb_game_info(2014)

games_13 <- cfb_game_info(2013)

games_12 <- cfb_game_info(2012)

games_11 <- cfb_game_info(2011)

games_10 <- cfb_game_info(2010)

games_09 <- cfb_game_info(2009)

games_08 <- cfb_game_info(2008)

games_07 <- cfb_game_info(2007)

games_06 <- cfb_game_info(2006)

games_05 <- cfb_game_info(2005)

#Join 

plays20 <- left_join(pbp_2020, games_20, by = c("game_id" = "game_id"))

plays19 <- left_join(pbp_2019, games_19, by = c("game_id" = "game_id"))

plays18 <- left_join(pbp_2018, games_18, by = c("game_id" = "game_id"))

plays17 <- left_join(pbp_2017, games_17, by = c("game_id" = "game_id"))

plays16 <- left_join(pbp_2016, games_16, by = c("game_id" = "game_id"))

plays15 <- left_join(pbp_2015, games_15, by = c("game_id" = "game_id"))

plays14 <- left_join(pbp_2014, games_14, by = c("game_id" = "id"))

plays13 <- left_join(pbp_2013, games_13, by = c("game_id" = "id"))

plays12 <- left_join(pbp_2012, games_12, by = c("game_id" = "id"))

plays11 <- left_join(pbp_2011, games_11, by = c("game_id" = "id"))

plays10 <- left_join(pbp_2010, games_10, by = c("game_id" = "id"))

plays09 <- left_join(pbp_2009, games_09, by = c("game_id" = "id"))

plays08 <- left_join(pbp_2008, games_08, by = c("game_id" = "id"))

plays07 <- left_join(pbp_2007, games_07, by = c("game_id" = "id"))

plays06 <- left_join(pbp_2006, games_06, by = c("game_id" = "id"))

plays05 <- left_join(pbp_2005, games_05, by = c("game_id" = "id"))

#Bind

Plays <- bind_rows(plays20, plays19, plays18, plays17, plays16, plays15)

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



