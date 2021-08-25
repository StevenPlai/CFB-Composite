library(tidyverse)
library(rvest)
library(janitor)
library(robustHD)

fpi <- read_html("https://www.espn.com/college-football/fpi") %>% html_table()

fpi <- bind_cols(fpi[[1]],fpi[[2]]) %>% row_to_names(row_number = 1) %>%
  select("team" = Team,"rating" = FPI) %>% mutate(rating = standardize(as.numeric(rating)))

fei <- read_html("https://www.bcftoys.com/2021-pfei/") %>% html_table()

fei <- fei[[1]] %>% row_to_names(row_number = 2, remove_rows_above = T) %>%
  clean_names() %>% 
  select(team,"rating" = fei,"o_rating" = ofei,"d_rating" = dfei) %>% 
  filter(!is.na(team) & team!= "Team")

sp <- read.csv("~/Downloads/spplus.csv") %>% 
  separate(col=Team, into = c("rk","team"), sep=" ", extra="merge") %>%
  select(team, "rating" = Rating, "o_rating" = Offense, "d_rating" = Defense) %>% 
  separate(col=o_rating, into="o_rating", sep=" ", extra="drop") %>% 
  separate(col=d_rating, into="d_rating", sep=" ", extra="drop")
