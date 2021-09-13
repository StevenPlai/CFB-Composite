library(tidyverse)
library(gt)
library(purrr)
library(glue)
library(extrafont)
library(fontawesome)
remotes::install_version("Rttf2pt1", version = "1.3.8")
source("~/Desktop/Projects/CFB-Composite/Functions.R")
week <- CFBWeek()

change_pos <- function(x) glue("+{x}")
change_neg <- function(x) "-x"
no_change <- function(x) "x"

old <- read.csv(glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/Composite/CompositeWeek{week-1}Ratings.csv")) %>%
  arrange(team)
new <- read.csv(glue("~/Desktop/Projects/CFB-Composite/Archived Ratings/Composite/CompositeWeek{week}Ratings.csv")) %>%
  arrange(team)

logos <- cfbd_team_info(only_fbs=T,year=2021) %>% select(school,logos) %>%
  separate(col=logos,sep=",",into=c("A","B"))
logos$A <- logos$A %>% stri_sub(from=4,to=nchar(logos$A)-1)
logos$B <- logos$B %>% stri_sub(from=3,to=nchar(logos$B)-2)
logos <- logos %>% arrange(school)

data <- data.frame(team = new$team,
                   rating = new$pp,digits=3,
                   last_week = old$pp,
                   logo = logos$A) %>%
  mutate(rank = 131-rank(rating,ties.method ="max"),
         last_rank = 131-rank(last_week,ties.method ="max"),
         change = last_rank-rank,digits=0) %>%
  select(logo,rank,team,rating,change) %>% 
  arrange(desc(rating)) %>% slice(1:10)


table <- gt(data) %>%
  tab_header(title="CFB Composite Ratings",
             subtitle=glue("Week {week}")) %>%
  tab_source_note(
    source_note = "Source: @StevenPlai on Twitter") %>%
  text_transform(
    locations = cells_body(columns=logo),
    fn = function(x) {
      web_image(url=x)}) %>%
  tab_footnote(footnote = "Represents per possession scoring advantage vs. an average team",
               locations = cells_column_labels(columns = rating)) %>%
  fmt_number(rating, decimals=2) %>%
  tab_style(
    style = cell_text(size = px(30), font = "SFProDisplay-Regular", weight = "bold",
                      align = "left"),
    locations = cells_title(group = "title")) %>%
  tab_style(
    style = cell_text(size = px(25), font = "SFProDisplay-Regular", align = "left"),
    locations = cells_title(groups = "subtitle")) %>%
  tab_style(
    style = cell_text(size = px(18), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(15),
      color = "#606066",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_labels(everything())
    ) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_body(columns=rank)) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_column_labels(columns=rank)) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_body(columns=rating)) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_column_labels(columns=rating)) %>%
  tab_style(
    style = cell_fill(color = "grey", alpha = .025),
    locations = cells_body(rows = (rank %% 2) == 0)) %>%
  text_transform(
    locations = cells_body(columns =change),
    fn = function(x){
      
      rank_chg <- as.integer(x)
      
      choose_logo <-function(x){
        if (x == 0){
          gt::html(fontawesome::fa("equals", fill = "grey", height = "2em"))
        } else if (x > 0){
          gt::html(glue::glue("<span style='color:#35b03b;font-face:bold;font-size:18px;'>{x}</span>"), fontawesome::fa("arrow-up", fill = "#35b03b", height = "1em"))
        } else if (x < 0) {
          gt::html(glue::glue("<span style='color:#DA2A2A;font-face:bold;font-size:18px;'>{x}</span>"), fontawesome::fa("arrow-down", fill = "#DA2A2A", height ="1em"))
        }
      } 
      
      map(rank_chg, choose_logo)
      
    }) %>% 
  cols_label(logo = "") %>%
  cols_width(
    logo ~ px(34),
    rank ~ px(60),
    team ~ px(100),
    rating ~ px(80),
    change ~ px(85)) %>% 
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    data_row.padding = px(5),
    column_labels.border.bottom.width = 2,
    footnotes.border.bottom.style = "none",
    table.border.bottom.style = "none",
    heading.border.bottom.style = "none")

gtsave(table, "~/Desktop/Week2Top10.png")
