####Shiny
library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)

##TODO: custom_theme for plots


countries <- DescTools::d.countries

setwd("C:\\Users\\calko\\OneDrive\\Pulpit")

data_clean <- ".\\RProject\\resources\\Data_cleaning.R"

##load data
source(data_clean)

###UI
ui <- fluidPage(
  title = "Data Salaries 2020-2024",
  theme = bs_theme(bootswatch = "lux"),
  fluidRow(
    column(4, 
           selectInput("field_of_work", NULL, choices = job_titles_groups)
    ),
    column(4,
           checkboxGroupInput("year", NULL, choices = 2020:2024, selected = 2024, inline = TRUE)
    )
    
  ),       
  fluidRow(
    
    column(4,
           
           
           plotOutput("line_plot")
    ),
    
    
    column(4,
           
           plotOutput("box_plot")
    ),
    
    
    column(4,
           plotOutput("histogram")
    )
    
  ),              
                
               
  fluidRow(
    column(4,
      #map     
      leafletOutput("map")
    ),
    column(4,
      #plot corresponding to the map
      plotOutput("map_plot")
    )
  
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
      labs(x = "YEAR", y = "AVREAGE SALARY") +
      geom_area(fill = "#9933FF", alpha = 5/10) +
      theme_minimal()
    
    
  })
  
  
  #2nd plot
  output$box_plot <- renderPlot({
    
    purple_palette <- c("#C8A2C8", "#8A2BE2", "#9400D3")    
    clean_data %>%
      group_by(company_size, year) %>%
      filter(year %in% input$year) %>%
      ggplot(aes(y = company_size, x = salary, fill = company_size)) +  # Swapping x and y
      geom_boxplot() +
      labs(y  = "NUMBER OF EMPLOEES IN COMPANY", x = "SALARY") +  # Updating axis labels
      scale_x_log10() +
      theme_minimal() +
      scale_fill_manual(values = purple_palette) +
      theme(legend.position = "none") #remove legend
  })
  
  
  #3rd plot
  salary_ranges <- c(0, 50000, 200000, 350000, 500000, 750000)
  output$histogram <- renderPlot({
    clean_data %>%
    ggplot(aes(x = salary)) +
    geom_histogram(color = "darkslategrey", fill = "darkslategray3") +
    scale_y_sqrt() +
    scale_x_sqrt() +
    scale_x_continuous(trans = "sqrt", breaks = salary_ranges) 
  
  })
  
  
  #map
  output$map <- renderLeaflet({
    mapdata <-clean_data %>%
      group_by(employee_residence) %>%
      summarise(count = n()) %>%
      left_join(countries, by = c("employee_residence" = "a2"))
    
      leaflet(mapdata) %>%
      addTiles() %>%
      addCircles(lng = ~longitude,
                 lat = ~latitude,
                 radius = ~sqrt(count) * 50000)
     
  })
  
  
}





shinyApp(ui = ui, server = server)

