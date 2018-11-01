library(shiny)
library(shinydashboard)
library(rdrop2)

source("helperFunctions.R")
source("dropboxFunctions.R")

# Define possible players
players_names = c('Stasa', 'Kata', 'Elio', 'Tata')
classes_names = c('Fighter', 'Wizard', 'Ranger', 'Cleric', 'Thief')

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Hero Realms"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Players", tabName = "players", icon = icon("user-o")),
      menuItem("Classes", tabName = "classes", icon = icon("superpowers")),
      menuItem("Enter New Game", tabName = "input", icon = icon("pencil-square-o"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "players",
        h2("Players")  
      ),
      tabItem(tabName = "classes",
        h2("Classes")
      ),
      tabItem(tabName = "input",
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

