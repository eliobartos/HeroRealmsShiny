library(shiny)
library(shinydashboard)

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
        fluidRow(
          textInput("player_1", "Player 1:"),
          textInput("class_1", "Player 1 class:")
        ),
        fluidRow(
          
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

