library(shiny)
library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("slate"),
  
  titlePanel("CFB Composite"),
  
  mainPanel(
    navbarPage(tabPanel("By Season",
                        fluidRow(
                          column(4, align = "center",
                                 
                                 tags$h3("Parameters"),
                                 
                                 selectInput(
                                   inputId =  "season",
                                   label = "Season:",
                                   choices = 1999:2020,
                                   selected = 2020
                                 ),
                                 
                                 sliderInput(
                                   inputId =  "min_rushes",
                                   label = "Minimum Rushes:",
                                   min = 1, max = 300,
                                   value = 80
                                 ),
                                 
                          )
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "rusher_graph",
                                     width = "100%",
                                     height = "50%"),
                          tableOutput("rusher_table")
                        )        
                        
               ),
               tabPanel("By Team",
                        fluidRow(
                          column(4, align = "center",
                                 
                                 tags$h3("Parameters"),
                                 selectInput("team",
                                             "Offense:",
                                             c(sort(unique(as.character(ids)))), selected = "DET"),
                                 sliderInput(
                                   inputId =  "team_min_rushes",
                                   label = "Minimum Rushes:",
                                   min = 1, max = 300,
                                   value = 50
                                 ),
                                 selectInput(
                                   inputId =  "team_season",
                                   label = "Season:",
                                   choices = 1999:2020,
                                   selected = 2020
                                 ),
                          )
                        ),
                        mainPanel(
                          plotOutput(outputId = "team_graph",
                                     width = "100%",
                                     height = "50%"),
                          tableOutput(outputId = "team_table_1"),
                          tableOutput(outputId = "team_table_2")
                        ),
               ),
               tabPanel('Rusher Comparison',
                        fluidRow(
                          column(4, align = "center",
                                 tags$h3('Parameters'),
                                 selectInput("player_1",
                                             "Player 1", 
                                             c(sort(unique(as.character(rushers)))), selected = "A.Kamara"),
                                 selectInput("player_2",
                                             "Player 2", 
                                             c(sort(unique(as.character(rushers)))), selected = "N.Chubb"),
                                 selectInput("player_3",
                                             "Player 3", 
                                             c(sort(unique(as.character(rushers)))), selected = "D.Henry"),
                          ),
                          sliderInput("year_range", "Year Range", value = c(2018, 2020), min = 1999, max = 2020, sep = ""),
                          sliderInput("week_range", "Weeks Range", value = c(1, 17), min = 1, max = 17),
                        ),
                        mainPanel(
                          plotOutput(outputId = "csum_graph",
                                     width = "750px", height = "500px"),
                          tableOutput(outputId = "rusher_comp_tab"), 
                          plotOutput(outputId = "perc_stacked",
                                     width = "750px", height = "500px")
                        ),
                        column(6, plotOutput(outputId = "rusher_graph_1", width = "750px", height = "500px")),
                        column(9, plotOutput(outputId = "rusher_graph_2", width = "750px", height = "500px")),
                        column(12, plotOutput(outputId = "rusher_graph_3", width = "750px", height = "500px")),
               )
    )
  )
)
