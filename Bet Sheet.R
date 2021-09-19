library(tidyverse)
library(rvest)
library(cfbfastR)
source("Functions.R")

week <- CFBWeek()
lines <- cfbd_betting_lines(year = 2021, week = week)
lines$home_team <- recode(lines$home_team, "San José State" = "San Jose State")
lines$away_team <- recode(lines$away_team, "San José State" = "San Jose State")
spreads <- lines %>% filter(is.na(home_score)&provider=="Bovada") %>%
  mutate(spread = as.numeric(spread)) %>%
  filter(!is.na(home_conf)&!is.na(away_conf)) %>% 
  left_join(neutrals,by="game_id")
data <- read_html("https://www.thepredictiontracker.com/predncaa.html") %>% html_nodes("pre")
data <- data[1] %>% html_text()
fileConn<-file("output.txt")
writeLines(data, fileConn)
close(fileConn)
table <- read.fwf("output.txt",widths=c(19,20,10,10,10,10,12,12,12,8,12,10))
table <- table %>% slice(15:nrow(table)-1) %>% select(V1,V2,V11,V12) %>%
  mutate(V11=as.numeric(V11),
         V12=as.numeric(V12))
colnames(table) <- c("home","visitor","home_wp","home_cp")

write.csv(table, "~/Desktop/Betting/Week3Ratings.csv",row.names = F)
