library(tidyverse)
library(rvest)
library(janitor)
library(robustHD)
library(heatmaply)
library(glue)
library(stringi)
library(cfbfastR)
library(lubridate)
source("Functions.R")

week <- CFBWeek()
updates <- read.csv("Updates.csv") %>% mutate(time = ymd_hms(time))
key <- read.csv("Team Name Key.csv")

####FPI####

if(file.exists(glue("Archived Ratings/FPI/FPIWeek{week}Ratings.csv"))==T){
  old_fpi<-read.csv(glue("Archived Ratings/FPI/FPIWeek{week}Ratings.csv"))
} else{old_fpi<-read.csv(glue("Archived Ratings/FPI/FPIWeek{week-1}Ratings.csv"))}

old_fpi$fpi <- old_fpi$fpi %>% round(4)

new_fpi <- read_html("https://www.espn.com/college-football/fpi") %>% html_table()
new_fpi <- bind_cols(new_fpi[[1]],new_fpi[[2]]) %>% row_to_names(row_number = 1)
new_fpi <- new_fpi %>% select("team" = Team,"rating" = FPI) %>%
  mutate(rating = round(standardize(as.numeric(rating)),4)) %>%
  left_join(key,by=c("team"="fpi")) %>% select("team"=cfbfastr,"fpi"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_fpi,old_fpi,by=c("team","fpi")))>0){
  updates[updates$system=="fpi",2] <- Sys.time()
  updates[updates$system=="fpi",3] <- week
  write.csv(new_fpi,glue("Archived Ratings/FPI/FPIWeek{week}Ratings.csv"),row.names=F)
}

####FEI####

if(file.exists(glue("Archived Ratings/FEI/FEIWeek{week}Ratings.csv"))==T){
  old_fei<-read.csv(glue("Archived Ratings/FEI/FEIWeek{week}Ratings.csv"))
} else{old_fei<-read.csv(glue("Archived Ratings/FEI/FEIWeek{week-1}Ratings.csv"))}

old_fei$fei <- old_fei$fei %>% round(4)

new_fei <- read_html("https://www.bcftoys.com/2021-fei/") %>% html_table()
new_fei <- new_fei[[1]] %>% row_to_names(row_number = 2, remove_rows_above = T) %>%
  clean_names() %>% 
  select(team,"rating" = fei,"o_rating" = ofei,"d_rating" = dfei) %>% 
  filter(!is.na(team) & team!= "Team") %>%
  mutate(rating = round(standardize(as.numeric(rating)),4),
         o_rating = standardize(as.numeric(o_rating)),
         d_rating = standardize(as.numeric(d_rating))) %>%
  left_join(key,by=c("team"="fei")) %>% select("team"=cfbfastr,"fei"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_fei,old_fei,by=c("team","fei")))>0){
  updates[updates$system=="fei",2] <- Sys.time()
  updates[updates$system=="fei",3] <- week
  write.csv(new_fei,glue("Archived Ratings/FEI/FEIWeek{week}Ratings.csv"),row.names=F)
}

####SP+####

sp <- read.csv(glue("Archived Ratings/SP+/SP+Week{week}Ratings.csv")) %>% 
  separate(col=Team, into = c("rk","team"), sep=" ", extra="merge") %>%
  separate(col=team, into = c("team"), sep="\\(", extra="drop") %>%
  select(team, "rating" = Rating, "o_rating" = Offense, "d_rating" = Defense) %>% 
  separate(col=o_rating, into="o_rating", sep=" ", extra="drop") %>% 
  separate(col=d_rating, into="d_rating", sep=" ", extra="drop") %>%
  mutate(team=trimws(team),
         rating=standardize(as.numeric(rating)),
         o_rating=standardize(as.numeric(o_rating)),
         d_rating=standardize(as.numeric(d_rating))) %>%
  left_join(key,by=c("team"="sp")) %>% select("team"=cfbfastr,"sp"=rating)

#updates[updates$system=="sp",2] <- Sys.time()
#updates[updates$system=="sp",3] <- week

####Laz####

if(file.exists(glue("Archived Ratings/Laz/LazWeek{week}Ratings.csv"))==T){
  old_laz<-read.csv(glue("Archived Ratings/Laz/LazWeek{week}Ratings.csv"))
} else{old_laz<-read.csv(glue("Archived Ratings/Laz/LazWeek{week-1}Ratings.csv"))}

old_laz$laz <- old_laz$laz %>% round(4)

new_laz <- read_html("https://www.lazindex.com/ncaa.php") %>% html_table()

new_laz <- new_laz[[1]] %>% filter(DIVISION == "FBS") %>% select("team" = SCHOOL,
                                                         "rating" = POWER) %>%
  mutate(rating = round(standardize(rating),4)) %>%
  left_join(key,by=c("team"="laz")) %>% select("team"=cfbfastr,"laz"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_laz,old_laz,by=c("team","laz")))>0){
  updates[updates$system=="laz",2] <- Sys.time()
  updates[updates$system=="laz",3] <- week
  write.csv(new_laz,glue("Archived Ratings/Laz/LazWeek{week}Ratings.csv"),row.names=F)
}

####TeamRankings####

if(file.exists(glue("Archived Ratings/TeamRankings/TeamRankingsWeek{week}Ratings.csv"))==T){
  old_tr<-read.csv(glue("Archived Ratings/TeamRankings/TeamRankingsWeek{week}Ratings.csv"))
} else{old_tr<-read.csv(glue("Archived Ratings/TeamRankings/TeamRankingsWeek{week-1}Ratings.csv"))}

old_tr$tr <- old_tr$tr %>% round(4)

new_tr <- read_html("https://www.teamrankings.com/college-football/ranking/predictive-by-other") %>% 
  html_table()

new_tr <- new_tr[[1]] %>% 
  separate(col=Team, into = c("team","x","y","z"), sep=" ", extra="merge") %>%
  mutate(h1=grepl("^[-A-Za-z\\'\\(\\)&]+$", x),
         h2=grepl("^[-A-Za-z\\'\\(\\)&]+$", y),
         h3=if_else(h1==F,0,if_else(h2==F,1,2)),
         team=if_else(h3==0,team,if_else(h3==1,paste0(team," ",x),paste0(team," ",x," ",y)))) %>% 
  select(team,"rating" = Rating) %>% mutate(rating = round(standardize(as.numeric(rating)),4)) %>%
  left_join(key,by=c("team"="tr")) %>% select("team"=cfbfastr,"tr"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_tr,old_tr,by=c("team","tr")))>0){
  updates[updates$system=="tr",2] <- Sys.time()
  updates[updates$system=="tr",3] <- week
  write.csv(new_tr,glue("Archived Ratings/TeamRankings/TeamRankingsWeek{week}Ratings.csv"),row.names=F)
}

####Versus####

if(file.exists(glue("Archived Ratings/VersusSports/VersusSportsWeek{week}Ratings.csv"))==T){
  old_vs<-read.csv(glue("Archived Ratings/VersusSports/VersusSportsWeek{week}Ratings.csv"))
} else{old_vs<-read.csv(glue("Archived Ratings/VersusSports/VersusSportsWeek{week-1}Ratings.csv"))}

old_vs$vs <- old_vs$vs %>% round(4)

new_vs <- read_html("https://www.versussportssimulator.com/FBS/rankings") %>% html_table()

new_vs <- new_vs[[1]] %>% 
  separate(col=School, into = c("team","x","y","z"), sep=" ", extra="merge") %>%
  mutate(h1=grepl("^[-A-Za-z\\'&]+$", x),
         h2=grepl("^[-A-Za-z\\'&]+$", y),
         h3=if_else(h1==F,0,if_else(h2==F,1,2)),
         team=if_else(h3==0,team,if_else(h3==1,paste0(team," ",x),paste0(team," ",x," ",y)))) %>% 
  mutate(rating = sqrt(Rating)) %>% 
  select(team,rating) %>% mutate(rating = round(standardize(as.numeric(rating)),4)) %>%
  left_join(key,by=c("team"="vs")) %>% select("team"=cfbfastr,"vs"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_vs,old_vs,by=c("team","vs")))>0){
  updates[updates$system=="vs",2] <- Sys.time()
  updates[updates$system=="vs",3] <- week
  write.csv(new_vs,glue("Archived Ratings/VersusSports/VersusSportsWeek{week}Ratings.csv"),row.names=F)
}

####Howell####

if(file.exists(glue("Archived Ratings/Howell/HowellWeek{week}Ratings.csv"))==T){
  old_how<-read.csv(glue("Archived Ratings/Howell/HowellWeek{week}Ratings.csv"))
} else{old_how<-read.csv(glue("Archived Ratings/Howell/HowellWeek{week-1}Ratings.csv"))}

old_how$how <- old_how$how %>% round(4)

new_how <- read_html("http://www.jhowell.net/cf/cf2021.htm") %>% html_table()

new_how <- new_how[[1]] %>% row_to_names(row_number = 1) %>%
  select("team"=Team,"rating"=PR) %>% mutate(rating = round(standardize(sqrt(as.numeric(rating))),4)) %>%
  left_join(key,by=c("team"="howell")) %>% filter(!is.na(cfbfastr)) %>%
  select("team"=cfbfastr,"howell"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_how,old_how,by=c("team","howell")))>0){
  updates[updates$system=="how",2] <- Sys.time()
  updates[updates$system=="how",3] <- week
  write.csv(new_how,glue("Archived Ratings/Howell/HowellWeek{week}Ratings.csv"),row.names=F)
}

####StatFox####

if(file.exists(glue("Archived Ratings/Statfox/StatfoxWeek{week}Ratings.csv"))==T){
  old_fox<-read.csv(glue("Archived Ratings/Statfox/StatfoxWeek{week}Ratings.csv"))
} else{old_fox<-read.csv(glue("Archived Ratings/Statfox/StatfoxWeek{week-1}Ratings.csv"))}

old_fox$fox <- old_fox$fox %>% round(4)

new_fox <- read_html("http://www.statfox.com/cfb/power/") %>% html_table()
new_fox <- new_fox[[4]] %>% row_to_names(row_number = 1) %>%
  select("team"=Team,"rating"=Rating) %>% 
  mutate(rating = round(standardize(as.numeric(rating)),4)) %>%
  left_join(key,by=c("team"="statfox")) %>% select("team"=cfbfastr,"statfox"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_fox,old_fox,by=c("team","statfox")))>0){
  updates[updates$system=="fox",2] <- Sys.time()
  updates[updates$system=="fox",3] <- week
  write.csv(new_fox,glue("Archived Ratings/Statfox/StatfoxWeek{week}Ratings.csv"),row.names=F)
}

####Donchess####

if(file.exists(glue("Archived Ratings/Donchess/DonchessWeek{week}Ratings.csv"))==T){
  old_dr<-read.csv(glue("Archived Ratings/Donchess/DonchessWeek{week}Ratings.csv"))
} else{old_dr<-read.csv(glue("Archived Ratings/Donchess/DonchessWeek{week-1}Ratings.csv"))}

old_dr$donchess <- old_dr$donchess %>% round(4)

new_dr <- read_html("https://www.dratings.com/sports/ncaa-fbs-football-ratings/") %>% html_table()

new_dr <- new_dr[[1]] %>% select(Rank,"rating"=Overall) %>%
  separate(col=Rank, into = c("rk","team"), sep="\\.", extra="merge") %>%
  separate(col=team, into = c("team"), sep="\\(", extra="drop") %>% select(team,rating) %>%
  mutate(team=trimws(team,"both"),
         rating = round(standardize(as.numeric(rating)),4))
new_dr$team <- stringi::stri_sub(new_dr$team, from=2,to=1000)
new_dr <- left_join(new_dr,key,by=c("team"="donchess")) %>% select("team"=cfbfastr,"donchess"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_dr,old_dr,by=c("team","donchess")))>0){
  updates[updates$system=="dr",2] <- Sys.time()
  updates[updates$system=="dr",3] <- week
  write.csv(new_dr,glue("Archived Ratings/Donchess/DonchessWeek{week}Ratings.csv"),row.names=F)
}

####ARGH####

if(file.exists(glue("Archived Ratings/ARGH/ARGHWeek{week}Ratings.csv"))==T){
  old_argh<-read.csv(glue("Archived Ratings/ARGH/ARGHWeek{week}Ratings.csv"))
} else{old_argh<-read.csv(glue("Archived Ratings/ARGH/ARGHWeek{week-1}Ratings.csv"))}

old_argh$argh <- old_argh$argh %>% round(4)

new_argh <- read_html("http://www.arghratings.com/currate.html") %>% html_table()

new_argh <- new_argh[[1]] %>% select("team"=Team,"rating"="Power Rating") %>%
  mutate(rating = round(standardize(as.numeric(rating)),4)) %>%
  left_join(key,by=c("team"="argh")) %>% filter(!is.na(cfbfastr)) %>%
  select("team"=cfbfastr,"argh"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_argh,old_argh,by=c("team","argh")))>0){
  updates[updates$system=="argh",2] <- Sys.time()
  updates[updates$system=="argh",3] <- week
  write.csv(new_argh,glue("Archived Ratings/ARGH/ARGHWeek{week}Ratings.csv"),row.names=F)
}

####PowerRank####

if(file.exists(glue("Archived Ratings/PowerRank/PowerRankWeek{week}Ratings.csv"))==T){
  old_tpr<-read.csv(glue("Archived Ratings/PowerRank/PowerRankWeek{week}Ratings.csv"))
} else{old_tpr<-read.csv(glue("Archived Ratings/PowerRank/PowerRankWeek{week-1}Ratings.csv"))}

old_tpr$tpr <- old_tpr$tpr %>% round(4)

new_tpr <- read_html("https://thepowerrank.com/college-football/bowl-subdivision-rankings/") %>% html_table()

new_tpr <- new_tpr[[1]] %>% select("team"=Team,"rating"=Rating) %>%
  mutate(rating = round(standardize(as.numeric(rating)),4)) %>%
  left_join(key,by=c("team"="tpr")) %>% select("team"=cfbfastr,"tpr"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_tpr,old_tpr,by=c("team","tpr")))>0){
  updates[updates$system=="tpr",2] <- Sys.time()
  updates[updates$system=="tpr",3] <- week
  write.csv(new_tpr,glue("Archived Ratings/PowerRank/PowerRankWeek{week}Ratings.csv"),row.names=F)
}

####PiRate####

if(file.exists(glue("Archived Ratings/PiRate/PiRateWeek{week}Ratings.csv"))==T){
  old_pr<-read.csv(glue("Archived Ratings/PiRate/PiRateWeek{week}Ratings.csv"))
} else{old_pr<-read.csv(glue("Archived Ratings/PiRate/PiRateWeek{week-1}Ratings.csv"))}

old_pr$pirate <- old_pr$pirate %>% round(4)
new_pr <- read_html("https://piratings.wordpress.com/") %>% html_table()
values <- data.frame(value = new_pr %>% sapply(nrow)) 
values$index <- 1:nrow(values)
value <- values %>% group_by(value) %>% summarise(index = first(index)) %>%
  filter(value==131) %>% select(index) %>% as.numeric()
new_pr <- new_pr[[value]] %>% 
  row_to_names(row_number = 1) %>% select("team"=Team,"rating"=Rating) %>%
  mutate(rating = standardize(as.numeric(rating))) %>% 
  left_join(key,by=c("team"="pirate")) %>% select("team"=cfbfastr,"pirate"=rating) %>%
  as.data.frame()

if(nrow(anti_join(new_pr,old_pr,by=c("team","pirate")))>0){
  updates[updates$system=="pr",2] <- Sys.time()
  updates[updates$system=="pr",3] <- week
  write.csv(new_pr,glue("Archived Ratings/PiRate/PiRateWeek{week}Ratings.csv"),row.names=F)
}

####Join####

master <- full_join(new_argh,dr,by="team") %>% full_join(new_fei,by="team") %>%
  full_join(new_fpi,by="team") %>% full_join(new_fox,by="team") %>% full_join(new_how,by="team") %>%
  full_join(new_laz,by="team") %>% full_join(new_pr,by="team") %>% full_join(sp,by="team") %>% 
  full_join(new_tr,by="team") %>% full_join(new_vs,by="team")

summary <- as.data.frame(master$team) %>% mutate(mean = rowMeans(master[,c(2:12)]),
                                                 median = rowMedians(as.matrix(master[,c(2:12)])),
                                                 sd = matrixStats::rowSds(as.matrix(master[,c(2:12)]))) %>%
  rename("team" = "master$team")

model_data <- read_html("https://www.bcftoys.com/2021-fei/") %>% html_table()
model_data <- model_data[[1]] %>% row_to_names(row_number = 2, remove_rows_above = T) %>%
  clean_names() %>% 
  select(team,"rating" = fei) %>% 
  filter(!is.na(team) & team!= "Team") %>%
  mutate(mean = standardize(as.numeric(rating))) %>% select(rating,mean)

lmod <- lm(rating ~ mean, data=model_data)
pdata <- predict(lmod, summary %>% select(mean))
summary$pp <- pdata

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

summary <- left_join(summary,pace,by="team")

a <- summary %>% filter(is.na(pace)) 
b <- summary %>% filter(!is.na(pace))
c <- mean(b$pace)
a$pace <- c
summary <- bind_rows(a,b) %>% select(team,pp,pace)

write.csv(summary, glue("Archived Ratings/Composite/CompositeWeek{week}Ratings.csv"),row.names=F)
write.csv(updates,"Updates.csv",row.names = F)
