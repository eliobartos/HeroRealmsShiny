library(shiny)
library(shinydashboard)
library(rdrop2)
library(formattable)
library(dplyr)

source("compare.R")
source("formatTables.R")
source("helperFunctions.R")
#source("dropboxFunctions.R")

# Connect to database
source("mySQLConnect.R")

# Closing connection
onStop(function() {
  poolClose(pool)
})

# Define possible players
players_names = c('Stasa', 'Kata', 'Elio', 'Tata')
classes_names = c('Fighter', 'Wizard', 'Ranger', 'Cleric', 'Thief')

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Hero Realms"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall", tabName = "overall", icon = icon("globe")),
      menuItem("Player", tabName = "player", icon = icon("user-o")),
      menuItem("Compare", tabName = "compare", icon = icon("superpowers")),
      menuItem("Enter New Game", tabName = "input", icon = icon("pencil-square-o"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overall", # -------------------------------------------------------
        h2("Overall"),
        br(),
        br(),
        fluidRow(
          column(width = 5,
                 box(
                   title = "Players",
                   status = "primary",
                   width = NULL,
                   solidHeader = TRUE,
                   div(style = 'overflow-x: hidden', 
                       formattableOutput("overall_players")
                   )
                 )
          ),
          column(width = 5,
                 box(
                   title = "Classes",
                   status = "primary",
                   width = NULL,
                   solidHeader = TRUE,
                   div(style = 'overflow-x: hidden', 
                       formattableOutput("overall_classes")
                   )
                 )
          )
        )
      ),
      tabItem(tabName = "player",
        h2("Player")
      ),
      tabItem(tabName = "compare", # ---------------------------------------------------------------
        h2("Compare"),
        br(),
        fluidRow(
          column(
            width = 2,
            box(
              title = "Player 1",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              
              selectInput(
                'comp_p1_name', 'Name:', choices = players_names
              ),
              selectInput(
                'comp_p1_class', 'Class:', choices = c("All", classes_names)
              ),
              
              tags$div(
              checkboxGroupInput("players_filter", 
                                 h3("VS Players"), 
                                 choices = list("Stasa" = "Stasa", 
                                                "Kata" = "Kata", 
                                                "Elio" = "Elio",
                                                "Tata" = "Tata"),
                                 selected = players_names),
              style="padding-left: 1vw; padding-right: 1vw"
              ),
              tags$div(
              checkboxGroupInput("class_filter", 
                                 h3("VS Classes"), 
                                 choices = list("Fighter" = "Fighter", 
                                                "Wizard" = "Wizard", 
                                                "Ranger" = "Ranger",
                                                "Cleric" = "Cleric",
                                                "Thief" = "Thief"),
                                 selected = classes_names),
              style="padding-left: 1vw; padding-right: 1vw"
              )
            )
          ),
          column(width = 10,
            fluidRow(valueBoxOutput("matches_played", width = 3),
                     infoBoxOutput("wins", width = 3),
                     valueBoxOutput("top_pick", width = 3)),
            fluidRow(h2("Row2")),
            fluidRow(h2("Row3")),
            fluidRow(h2("Row4"))
          )
        )
      ),
      tabItem(tabName = "input", # ----------------------------------------------
        h2("Enter New Game"),
        br(),
        br(),
        fluidRow(
          column(width = 4,
                box(
                  title = "Player 1",
                  status = "primary",
                  width = NULL,
                  solidHeader = TRUE,
                  
                  selectizeInput(
                    'p1_name', 'Name:', choices = players_names,
                    options = list(
                      placeholder = 'Select first player',
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  ),
                  selectizeInput(
                    'p1_class', 'Class:', choices = classes_names,
                    options = list(
                      placeholder = 'Select first players class',
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                )
                 
          ),
          column(width = 4,
                 box(
                   title = "Player 2",
                   status = "primary",
                   width = NULL,
                   solidHeader = TRUE,
                   
                   selectizeInput(
                     'p2_name', 'Name:', choices = players_names,
                     options = list(
                       placeholder = 'Select second player',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   ),
                   selectizeInput(
                     'p2_class', 'Class:', choices = classes_names,
                     options = list(
                       placeholder = 'Select second players class',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   )
                 )
                 
          ),
          column(width = 4,
                 box(
                   title = "Winner",
                   status = "primary",
                   width = NULL,
                   solidHeader = TRUE,
                   
                   selectizeInput(
                     'winner', 'Winner:', choices = players_names,
                     options = list(
                       placeholder = 'Select winner',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   ),
                   passwordInput("password", "Enter Password:"),
                   actionButton("add_game", "Add Game")
                 )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data_all = pool %>% 
    tbl('hero_realms_data') %>% 
    as.data.frame()
  
  # Overall Tab -------------------------------------------------------------
  data_players = get_overall(data_all, "name")
  data_classes = get_overall(data_all, "class")
  
  output$overall_players = renderFormattable({
    format_overall_data(data_players)
  })
  
  output$overall_classes = renderFormattable({
    format_overall_data(data_classes)
  })
  
  # Compare Tab ------------------------------------------------------
  one_player_data = reactive({
    get_match_data_for_player(data_all, input$comp_p1_name, 'All') %>% 
      filter(name == input$comp_p1_name)
  })
  
  output$matches_played <- renderValueBox({
    valueBox(
      nrow(one_player_data()), "Games Played", icon = icon("play"),
      color = "orange",
      width = NULL
    )
  })
  
  output$wins <- renderValueBox({
    valueBox(
      sum(one_player_data()[['winner']]), "Wins", 
      icon = icon("trophy"),
      color = "aqua",
      width = NULL
    )
  })
  
  output$top_pick <- renderValueBox({
    valueBox(
      names(sort(table(one_player_data()[['class']]), decreasing = TRUE))[[1]],
      "Top Pick", icon = icon("lisfaet"),
      color = "black",
      width = NULL
    )
  })
  # Enter New Game Tab ------------------------------------------------------
  # When clicking on Add Game Button
  observeEvent(input$add_game, {
    validate_data(input$p1_name,
                  input$p1_class,
                  input$p2_name,
                  input$p2_class,
                  input$winner,
                  input$password)
    
  })
}






# Run the application 
shinyApp(ui = ui, server = server)

