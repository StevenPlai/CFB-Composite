library(tidyverse)
library(rvest)
library(janitor)
library(robustHD)
library(heatmaply)
library(glue)
library(stringi)
source("~/Desktop/Projects/CFB-Composite/Functions.R")

week <- CFBWeek()

key <- read.csv("~/desktop/Projects/CFB-Composite/Team Name Key.csv")

old_fpi<-read.csv(glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/FPI/FPIWeek{week}Ratings.csv"))

fpi <- read_html("https://www.espn.com/college-football/fpi") %>% html_table()

fpi <- bind_cols(fpi[[1]],fpi[[2]]) %>% row_to_names(row_number = 1)

if_else()

write.csv(fpi,glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/FPI/FPIWeek{week}Ratings.csv"),row.names=F)

fpi <- fpi %>% select("team" = Team,"rating" = FPI) %>%
  mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="fpi")) %>% select("team"=cfbfastr,"fpi"=rating) %>%
  as.data.frame()

write.csv(fpi,glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/FPI/FPIWeek{week}Ratings.csv"),row.names=F)

fei <- read_html("https://www.bcftoys.com/2021-fei/") %>% html_table()
fei <- fei[[1]] %>% row_to_names(row_number = 2, remove_rows_above = T) %>%
  clean_names() %>% 
  select(team,"rating" = fei,"o_rating" = ofei,"d_rating" = dfei) %>% 
  filter(!is.na(team) & team!= "Team") %>%
  mutate(rating = standardize(as.numeric(rating)),
         o_rating = standardize(as.numeric(o_rating)),
         d_rating = standardize(as.numeric(d_rating))) %>%
  left_join(key,by=c("team"="fei")) %>% select("team"=cfbfastr,"fei"=rating) %>%
  as.data.frame()

write.csv(fei,glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/FEI/FEIWeek{week}Ratings.csv"),row.names=F)

sp <- read.csv(glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/SP+/SP+Week{week}Ratings.csv")) %>% 
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

laz <- read_html("https://www.lazindex.com/ncaa.php") %>% html_table()

laz <- laz[[1]] %>% filter(DIVISION == "FBS") %>% select("team" = SCHOOL,
                                                         "rating" = POWER) %>%
  mutate(rating = standardize(rating)) %>%
  left_join(key,by=c("team"="laz")) %>% select("team"=cfbfastr,"laz"=rating) %>%
  as.data.frame()

write.csv(laz,glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/Laz/LazWeek{week}Ratings.csv"),row.names=F)

tr <- read_html("https://www.teamrankings.com/college-football/ranking/predictive-by-other") %>% 
  html_table()

tr <- tr[[1]] %>% 
  separate(col=Team, into = c("team","x","y","z"), sep=" ", extra="merge") %>%
  mutate(h1=grepl("^[-A-Za-z\\'\\(\\)&]+$", x),
         h2=grepl("^[-A-Za-z\\'\\(\\)&]+$", y),
         h3=if_else(h1==F,0,if_else(h2==F,1,2)),
         team=if_else(h3==0,team,if_else(h3==1,paste0(team," ",x),paste0(team," ",x," ",y)))) %>% 
  select(team,"rating" = Rating) %>% mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="tr")) %>% select("team"=cfbfastr,"tr"=rating) %>%
  as.data.frame()

write.csv(tr,glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/TeamRankings/TeamRankings{week}Ratings.csv"),row.names=F)

vs <- read_html("https://www.versussportssimulator.com/FBS/rankings") %>% html_table()

vs <- vs[[1]] %>% 
  separate(col=School, into = c("team","x","y","z"), sep=" ", extra="merge") %>%
  mutate(h1=grepl("^[-A-Za-z\\'&]+$", x),
         h2=grepl("^[-A-Za-z\\'&]+$", y),
         h3=if_else(h1==F,0,if_else(h2==F,1,2)),
         team=if_else(h3==0,team,if_else(h3==1,paste0(team," ",x),paste0(team," ",x," ",y)))) %>% 
  mutate(rating = log(Rating)) %>% 
  select(team,rating) %>% mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="vs")) %>% select("team"=cfbfastr,"vs"=rating) %>%
  as.data.frame()

how <- read_html("http://www.jhowell.net/cf/cf2021.htm") %>% html_table()

how <- how[[1]] %>% row_to_names(row_number = 1) %>%
  select("team"=Team,"rating"=PR) %>% mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="howell")) %>% filter(!is.na(cfbfastr)) %>%
  select("team"=cfbfastr,"howell"=rating) %>%
  as.data.frame()

fox <- read_html("http://www.statfox.com/cfb/power/") %>% html_table()

fox <- fox[[4]] %>% row_to_names(row_number = 1) %>%
  select("team"=Team,"rating"=Rating) %>% 
  mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="statfox")) %>% select("team"=cfbfastr,"statfox"=rating) %>%
  as.data.frame()

dr <- read_html("https://www.dratings.com/sports/ncaa-fbs-football-ratings/") %>% html_table()

dr <- dr[[1]] %>% select(Rank,"rating"=Overall) %>%
  separate(col=Rank, into = c("rk","team"), sep="\\.", extra="merge") %>%
  separate(col=team, into = c("team"), sep="\\(", extra="drop") %>% select(team,rating) %>%
  mutate(team=trimws(team,"both"),
         rating = standardize(as.numeric(rating)))
dr$team <- stringi::stri_sub(dr$team, from=2,to=1000)
dr <- left_join(dr,key,by=c("team"="donchess")) %>% select("team"=cfbfastr,"donchess"=rating) %>%
  as.data.frame()

argh <- read_html("http://www.arghratings.com/currate.html") %>% html_table()

argh <- argh[[1]] %>% select("team"=Team,"rating"="Power Rating") %>%
  mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="argh")) %>% filter(!is.na(cfbfastr)) %>%
  select("team"=cfbfastr,"argh"=rating) %>%
  as.data.frame()

tpr <- read_html("https://thepowerrank.com/college-football/bowl-subdivision-rankings/") %>% html_table()

tpr <- tpr[[1]] %>% select("team"=Team,"rating"=Rating) %>%
  mutate(rating = standardize(as.numeric(rating))) %>%
  left_join(key,by=c("team"="tpr")) %>% select("team"=cfbfastr,"tpr"=rating) %>%
  as.data.frame()

pr <- read_html("https://piratings.wordpress.com/") %>% html_table()

pr <- pr[[4]] %>% row_to_names(row_number = 1) %>% select("team"=Team,"rating"=Rating) %>%
  mutate(rating = standardize(as.numeric(rating))) %>% 
  left_join(key,by=c("team"="pirate")) %>% select("team"=cfbfastr,"pirate"=rating) %>%
  as.data.frame()

master <- full_join(argh,dr,by="team") %>% full_join(fei,by="team") %>%
  full_join(fpi,by="team") %>% full_join(fox,by="team") %>% full_join(how,by="team") %>%
  full_join(laz,by="team") %>% full_join(pr,by="team") %>% full_join(sp,by="team") %>% 
  full_join(tpr,by="team") %>% full_join(tr,by="team") %>% full_join(vs,by="team")

summary <- as.data.frame(master$team) %>% mutate(mean = rowMeans(master[,c(2:13)]),
                                                 median = rowMedians(as.matrix(master[,c(2:13)])),
                                                 sd = matrixStats::rowSds(as.matrix(master[,c(2:13)]))) %>%
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

summary <- left_join(summary,pace,by="team")

a <- summary %>% filter(is.na(pace))
b <- summary %>% filter(!is.na(pace))
c <- mean(b$pace)
a$pace <- c
summary <- bind_rows(a,b) %>% select(team,pp,pace)

write.csv(summary, glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/Composite/CompositeWeek{week}Ratings.csv"),row.names=F)
