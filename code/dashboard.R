####Shiny
library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)

data_clean <- "j:\\Desktop\\projekt-przetw\\RProject\\resources\\Data_cleaning.R"

##load data
source(data_clean)

###UI
ui <- fluidPage(title = "Data Salaries 2020-2024",
                theme = bs_theme(bootswatch = "lux"),
                fluidRow(
                  #dropdown menu
                  selectInput("field_of_work", NULL, choices = job_titles_groups),
                  
                  plotOutput("line_plot"),
                  
                  checkboxGroupInput("year", NULL, choices = 2020:2024, selected = 2024, inline = TRUE),
                  
                  plotOutput("box_plot")
                  
                
                  
                  
                  
                  )
                )







#####Server
server <- function(input, output, session) {
  

  
  #1st plot
  output$line_plot <- renderPlot({
    clean_data %>%
      group_by(job_group, year) %>%
      summarise(Avreage_Salary = mean(salary)) %>%
      filter(job_group == input$field_of_work) %>%
      ggplot(aes(x = year, y = Avreage_Salary)) +
      geom_line() +
      labs(x = "YEAR", y = "AVREAGE SALARY")
  })
  
  
  #2nd plot
  output$box_plot <- renderPlot({
    clean_data %>%
      group_by(company_size, year) %>%
      filter(year %in% input$year) %>%
      ggplot(aes(x = company_size, y = salary)) +
      geom_boxplot() +
      labs(x  = "NUMBER OF EMPLOEES IN COMPANY", y = "SALARY") +
      scale_y_log10() 
  })
  
  #3rd plot
  
  
  
}





shinyApp(ui = ui, server = server)