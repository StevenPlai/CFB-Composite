pbp21 <- data.frame()
for(i in 1:16){
  df <- cfbd_pbp_data(
    2021, 
    week=i,
    team="Southern Mississippi",
    season_type = "regular",
    epa_wpa = T)
  pbp21 <- bind_rows(pbp21,df)}

qb <- pbp21 %>% filter(!is.na(EPA),pos_team=="Southern Mississippi") %>% group_by(passer_player_name) %>% summarise(EPA = mean(EPA),
                                                          SR = mean(epa_success), n = n())
