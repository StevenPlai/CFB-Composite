library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(gt)
library(stringi)
library(purrr)
library(cfbfastR)
library(glue)
library(extrafont)
library(fontawesome)
library(kableExtra)
library(sparkline)
source("Functions.R") 

actual_week <- CFBWeek()
records <- cfbd_game_records(2021) %>% mutate(record = glue("{total_wins}-{total_losses}")) %>% 
  select(team,record)
pace <- 
power_five <- c("SEC","ACC","Pac-12","Big Ten","Big 12")

CSS <- "
/* CSS for the checked checkboxes */
.pretty.p-default input:checked~.state label:after {
  background-color: #388fe0 !important;
}
.btn-radio {
  background-color: #388fe0;
}
"

ui <- fluidPage(
  
  tags$head(tags$style(HTML(CSS))),
  
  theme = shinytheme("sandstone"),
  
  titlePanel("CFB Composite"),
  
  mainPanel(
    tabsetPanel(tabPanel(title="Team Ratings",
                         br(),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("week", "Week",
                               choices = 1:actual_week,
                               selected = actual_week),
                           radioGroupButtons("type", h3("View"),
                                        choices = list("All" = 1,
                                                       "Power Five" = 2,
                                                       "Group of Five" = 3,
                                                       "Custom" = 4),
                                        selected=1),
                           conditionalPanel(condition = "input.type == 4",
                           prettyCheckboxGroup("conf", 
                                             h3("Conference(s)"), 
                                             choices = list("American" = "American Athletic",
                                                            "ACC",
                                                            "Big 12",
                                                            "Big Ten",
                                                            "C-USA",
                                                            "Independents" = "FBS Independents",
                                                            "MAC" = "Mid-American",
                                                            "Mountain West",
                                                            "Pac-12",
                                                            "SEC",
                                                            "Sun Belt")))),
                         mainPanel(tableOutput("table")))))))

server <- function(input,output) {
  
  output$table <- render_gt({
  
  vweek <- as.numeric(input$week)
  vtype <- as.numeric(input$type)
  if(vtype==4){vconf <- input$conf}
  
  if(vweek>1){
    last_week <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{vweek-1}Ratings.csv")) %>% arrange(team)
  }
  current <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{vweek}Ratings.csv")) %>% arrange(team)
  
  logos <- cfbd_team_info(only_fbs=T,year=2021) %>% select(school,logos,mascot,conference) %>%
    separate(col=logos,sep=",",into=c("A","B"))
  logos$A <- logos$A %>% stri_sub(from=4,to=nchar(logos$A)-1)
  logos$B <- logos$B %>% stri_sub(from=3,to=nchar(logos$B)-2)
  logos <- logos %>% arrange(school) %>% 
  mutate(type = if_else(conference %in% power_five, 2, 
                         if_else(school=="Notre Dame", 2, 3), 3))
  
  ratings <- read.csv("Archived Ratings/Composite/CompositeFullRatings.csv") %>% 
    filter(week<=vweek) %>% 
    group_by(team) %>% summarise(weekly = list(pp)) %>% arrange(team) %>% 
    left_join(records, by="team")
  
    if(vweek>1){
      data <- ratings %>% mutate(rating = round(current$pp,digits=3),
                                 last_week = last_week$pp,
                                 logo = logos$A,
                                 mascot=logos$mascot,
                                 conf = logos$conference,
                                 type = logos$type)
        if(vtype==2){
          data <- data %>% filter(type==2)
        } else{if(vtype==3){
          data <- data %>% filter(type==3)
        } else{if(vtype==4){
          data <- data %>% filter(conf %in% vconf)}}}
       data <- data %>% 
        mutate(rank = n()+1-as.numeric(rank(rating,ties.method ="max")),
               last_rank = n()+1-as.numeric(rank(last_week,ties.method ="max")),
               change = as.numeric(last_rank)-as.numeric(rank),digits=0) %>%
        select(rank,change,logo,team,mascot,record,rating,weekly) %>% 
        arrange(desc(rating))
    } else{
      data <- ratings %>% 
        mutate(rating = round(current$pp,digits=3),
               logo = logos$A,
               mascot=logos$mascot) %>%
        mutate(rank = n()+1-as.numeric(rank(rating,ties.method ="max"))) %>%
        select(rank,logo,team,mascot,rating) %>% 
        arrange(desc(rating))
      if(vtype==2){
        data <- data %>% filter(type==2)
      } else{if(vtype==3){
        data <- data %>% filter(type==3)
      } else{if(vtype==4){
        data <- data %>% filter(conf %in% vconf)}}}
      data <- data %>% 
      mutate(rating = round(current$pp,digits=3),
                                 logo = logos$A,
                                 mascot=logos$mascot) %>%
        mutate(rank = n()+1-as.numeric(rank(rating,ties.method ="max"))) %>%
        select(rank,logo,team,mascot,record,rating) %>% 
        arrange(desc(rating))}
  if(vweek>1){
    table <- gt(data) %>%
      cols_merge(
        columns = c(team, mascot),
        pattern = "{1}:{2}"
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = team),
        fn = function(x){
          team <- word(x, 1, sep = "\\:")
          mascot <- word(x, -1, sep = "\\:")
          glue::glue(
            "<div style='line-height:10px'><span style='font-weight:bold;font-size:14px'>{team}</div>
        <div style='line-height:14px'><span style ='font-weight:bold;color:grey;font-size:12px'>{mascot}</span></div>"
          )
        }
      ) 
    if(vtype==1){
      table <- table %>% tab_header(title="CFB Composite Ratings",
                                    subtitle=glue("Week {vweek}"))
    } else{if(vtype==2){
      table <- table %>% tab_header(title="CFB Composite Ratings",
                                    subtitle=glue("Power Five, Week {vweek}"))
    } else{if(vtype==3){
      table <- table %>% tab_header(title="CFB Composite Ratings",
                                    subtitle=glue("Group of Five, Week {vweek}"))
    } else{if(vtype==4){
      if(length(vconf<3)){
        table <- table %>% tab_header(title="CFB Composite Ratings",
                                      subtitle=paste0(paste(vconf, collapse=" & "),", ",glue("Week {vweek}")))
      }else{
        table <- table %>% tab_header(title="CFB Composite Ratings",
                                      subtitle=paste0(paste(vconf, collapse=", "),", ",glue("Week {vweek}")))
      }
    }}}}
      table <- table %>% 
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
          size = px(12),
          color = "#606066",
          font = "SFProDisplay-Regular",
          transform = "uppercase"),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style=cell_text(
          align = "center"),
        locations = cells_body(columns=c(rank,rating,change,logo,weekly))) %>%
      tab_style(
        style=cell_text(
          align = "center"),
        locations = cells_column_labels(columns=c(rank,rating,change,logo,weekly))) %>%
      tab_style(
        style = cell_fill(color = "grey", alpha = .025),
        locations = cells_body(rows = (rank %% 2) == 0)) %>%
      gt_sparkline(weekly, type = "sparkline", same_limit = F) %>% 
      fmt_markdown(columns = weekly) %>%
      text_transform(
        locations = cells_body(columns =change),
        fn = function(x){
          
          rank_chg <- as.integer(x)
          
          choose_logo <-function(x){
            if (x == 0){
              gt::html(fontawesome::fa("equals", fill = "grey", height = "1em"))
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
        rank ~ px(50),
        change ~ px(60),
        logo ~ px(45),
        team ~ px(140),
        rating ~ px(80)) %>% 
      tab_options(
        column_labels.border.top.style = "none",
        table.border.top.style = "none",
        table_body.border.top.style = "none",
        data_row.padding = px(5),
        column_labels.border.bottom.width = 2,
        footnotes.border.bottom.style = "none",
        table.border.bottom.style = "none",
        heading.border.bottom.style = "none")
  } else{
    table <- gt(data) %>%
      cols_merge(
        columns = c(team, mascot),
        pattern = "{1}:{2}"
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = c(team)
        ),
        fn = function(x){
          team <- word(x, 1, sep = "\\:")
          mascot <- word(x, -1, sep = "\\:")
          glue::glue(
            "<div style='line-height:10px'><span style='font-weight:bold;font-size:14px'>{team}</div>
        <div style='line-height:14px'><span style ='font-weight:bold;color:grey;font-size:12px'>{mascot}</span></div>"
          )
        }
      )
    if(vtype==1){
      table <- table %>% tab_header(title="CFB Composite Ratings",
                                    subtitle=glue("Week {vweek}"))
    } else{if(vtype==2){
      table <- table %>% tab_header(title="CFB Composite Ratings",
                                    subtitle=glue("Power Five, Week {vweek}"))
    } else{if(vtype==3){
      table <- table %>% tab_header(title="CFB Composite Ratings",
                                    subtitle=glue("Group of Five, Week {vweek}"))
    } else{if(vtype==4){
      if(length(vconf<3)){
        table <- table %>% tab_header(title="CFB Composite Ratings",
                                      subtitle=paste0(paste(vconf, collapse=" & "),", ",glue("Week {vweek}")))
      }else{
        table <- table %>% tab_header(title="CFB Composite Ratings",
                                      subtitle=paste0(paste(vconf, collapse=", "),", ",glue("Week {vweek}")))
      }
    }}}}
      table <- table %>% 
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
          size = px(12),
          color = "#606066",
          font = "SFProDisplay-Regular",
          transform = "uppercase"),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style=cell_text(
          align = "center"),
        locations = cells_body(columns=c(rank,rating,logo))) %>%
      tab_style(
        style=cell_text(
          align = "center"),
        locations = cells_column_labels(columns=c(rank,rating,logo))) %>%
      tab_style(
        style = cell_fill(color = "grey", alpha = .025),
        locations = cells_body(rows = (rank %% 2) == 0)) %>%
      cols_label(logo = "") %>%
      cols_width(
        rank ~ px(50),
        logo ~ px(45),
        team ~ px(140),
        rating ~ px(80)) %>% 
      tab_options(
        column_labels.border.top.style = "none",
        table.border.top.style = "none",
        table_body.border.top.style = "none",
        data_row.padding = px(5),
        column_labels.border.bottom.width = 2,
        footnotes.border.bottom.style = "none",
        table.border.bottom.style = "none",
        heading.border.bottom.style = "none")}
  })}

shinyApp(ui = ui, server = server)


