library(tidyverse)
library(rvest)
library(janitor)
library(robustHD)
library(heatmaply)

fpi <- read_html("https://www.espn.com/college-football/fpi") %>% html_table()

fpi <- bind_cols(fpi[[1]],fpi[[2]]) %>% row_to_names(row_number = 1) %>%
  select("team" = Team,"rating" = FPI) %>% mutate(rating = standardize(as.numeric(rating)))

fei <- read_html("https://www.bcftoys.com/2021-pfei/") %>% html_table()

fei <- fei[[1]] %>% row_to_names(row_number = 2, remove_rows_above = T) %>%
  clean_names() %>% 
  select(team,"rating" = fei,"o_rating" = ofei,"d_rating" = dfei) %>% 
  filter(!is.na(team) & team!= "Team") %>%
  mutate(rating = standardize(as.numeric(rating)),
         o_rating = standardize(as.numeric(o_rating)),
         d_rating = standardize(as.numeric(d_rating)))

sp <- read.csv("~/Downloads/spplus.csv") %>% 
  separate(col=Team, into = c("rk","team"), sep=" ", extra="merge") %>%
  select(team, "rating" = Rating, "o_rating" = Offense, "d_rating" = Defense) %>% 
  separate(col=o_rating, into="o_rating", sep=" ", extra="drop") %>% 
  separate(col=d_rating, into="d_rating", sep=" ", extra="drop") %>%
  mutate(rating = standardize(as.numeric(rating)),
         o_rating = standardize(as.numeric(o_rating)),
         d_rating = standardize(as.numeric(d_rating)))

laz <- read_html("https://www.lazindex.com/ncaa.php") %>% html_table()

laz <- laz[[1]] %>% filter(DIVISION == "FBS") %>% select("school" = SCHOOL,
                                                         "rating" = POWER) %>%
  mutate(rating = standardize(rating))


tr <- read_html("https://www.teamrankings.com/college-football/ranking/predictive-by-other") %>% 
  html_table()

tr <- tr[[1]] %>% 
  separate(col=Team, into = c("team","x","y","z"), sep=" ", extra="merge") %>%
  mutate(h1=grepl("^[-A-Za-z\\'&]+$", x),
         h2=grepl("^[-A-Za-z\\'&]+$", y),
         h3=if_else(h1==F,0,if_else(h2==F,1,2)),
         team=if_else(h3==0,team,if_else(h3==1,paste0(team," ",x),paste0(team," ",x," ",y)))) %>% 
  select(team,"rating" = Rating) %>% mutate(rating = standardize(as.numeric(rating)))

vs <- read_html("https://www.versussportssimulator.com/FBS/rankings") %>% html_table()

vs <- vs[[1]] %>% 
  separate(col=School, into = c("team","x","y","z"), sep=" ", extra="merge") %>%
  mutate(h1=grepl("^[-A-Za-z\\'&]+$", x),
         h2=grepl("^[-A-Za-z\\'&]+$", y),
         h3=if_else(h1==F,0,if_else(h2==F,1,2)),
         team=if_else(h3==0,team,if_else(h3==1,paste0(team," ",x),paste0(team," ",x," ",y)))) %>% 
  mutate(rating = log(Rating)) %>% 
  select(team,rating) %>% mutate(rating = standardize(as.numeric(rating)))

how <- read_html("http://www.jhowell.net/cf/cf2021.htm") %>% html_table()

how <- how[[1]] %>% row_to_names(row_number = 1) %>%
  select("team"=Team,"rating"=PR) %>% mutate(rating = standardize(as.numeric(rating)))

fox <- read_html("http://www.statfox.com/cfb/power/") %>% html_table()

fox <- fox[[4]] %>% row_to_names(row_number = 1) %>%
  select("team"=Team,"rating"=Rating) %>% mutate(rating = standardize(as.numeric(rating)))

dr <- read_html("https://www.dratings.com/sports/ncaa-fbs-football-ratings/") %>% html_table()

dr <- dr[[1]] %>% select(Rank,"rating"=Overall) %>%
  separate(col=Rank, into = c("rk","team"), sep="\\.", extra="merge") %>%
  separate(col=team, into = c("team"), sep="\\(", extra="drop") %>% select(team,rating) %>%
  mutate(team=trimws(team),
         rating = standardize(as.numeric(rating)))

argh <- read_html("http://www.arghratings.com/currate.html") %>% html_table()

argh <- argh[[1]] %>% select("team"=Team,"rating"="Power Rating") %>%
  mutate(rating = standardize(as.numeric(rating)))

tpr <- read_html("https://thepowerrank.com/college-football/bowl-subdivision-rankings/") %>% html_table()

tpr <- tpr[[1]] %>% select("team"=Team,"rating"=Rating) %>%
  mutate(rating = standardize(as.numeric(rating)))

pr <- read_html("https://piratings.wordpress.com/") %>% html_table()

pr <- pr[[4]] %>% row_to_names(row_number = 1) %>% select("team"=Team,"rating"=Rating) %>%
  mutate(rating = standardize(as.numeric(rating)))

key <- read.csv("~/desktop/Projects/CFB Composite/Repo/CFB-Composite/Team Name Key.csv")
