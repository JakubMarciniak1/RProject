library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Salaries"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("database", lib = "font-awesome"))
    )
  ),
  dashboardBody()
)

server <- function(input, output) {
  library(ggplot2)
  
  source(file.path(".","Data_cleaning.R"))
  
  output$linePlot <- 
  
}


shinyApp(ui, server)