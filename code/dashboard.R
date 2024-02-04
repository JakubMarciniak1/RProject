#### Shiny
library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(leaflet)
library(shinythemes)
library(DT)
library(kableExtra)

## TODO: custom_theme for plots

options(shiny.launch.browser = TRUE) # open in browser instead of rstudio
countries <- DescTools::d.countries # database with coords for country code

setwd("C:\\projektUAM\\RProject")

data_clean <- ".\\resources\\Data_cleaning.R"

## load data
source(data_clean) # executing the data cleaning script

# TestingTableStyles
styled_data <- kbl(data)
str(data)
str(styled_data)

### UI
ui <- dashboardPage( skin = "blue",
  dashboardHeader(title = "Data Salaries 2020-2024", titleWidth = 270),
  dashboardSidebar(width = 270,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Source", tabName = "source", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      
      .dataTables_wrapper { padding: 10px }
      
      .datatables { background: white; overflow: scroll; }
      
      .content-wrapper, .right-side {
        min-height: 100%;
        background-color: #C4CCD5;
      z-index: 800;}
    
      .row{
        margin-bottom: 5px;
        margin-top: 10px;
      }
    
      #map{
      
      }
      #line_plot{
      
      }
      #box_plot{
      
      }
      #histogram{
      
      }
      #map_plot{
        height: 400px !important;
      }
      #bar_plot{
      }
      #year{
        margin-bottom: 28px;
      }
      #min_salary{
        padding: 0;
      }
      #max_salary{
        padding: 0;
      }
      .small-box{
        min-height: 190px !important;
      }
    
    #reset_button {
      appearance: none;
      background-color: #fff;
      border: 1px solid rgba(27, 31, 35, 0.15);
      border-radius: 6px;
      box-shadow: rgba(27, 31, 35, 0.04) 0 1px 0, rgba(255, 255, 255, 0.25) 0 1px 0 inset;
      box-sizing: border-box;
      color: #24292E;
      cursor: pointer;
      display: inline-block;
      font-family: -apple-system, system-ui, , Helvetica, Arial, sans-serif;
      font-size: 14px;
      font-weight: 500;
      line-height: 20px;
      list-style: none;
      padding: 6px 16px;
      position: relative;
      transition: background-color 0.2s cubic-bezier(0.3, 0, 0.5, 1);
      user-select: none;
      -webkit-user-select: none;
      touch-action: manipulation;
      vertical-align: middle;
      white-space: nowrap;
      word-wrap: break-word;
      margin-bottom: 15px;
    }

  #reset_button:hover {
    background-color: #F3F4F6;
    text-decoration: none;
    transition-duration: 0.1s;
  }

  #reset_button:disabled {
    background-color: #FAFBFC;
    border-color: rgba(27, 31, 35, 0.15);
    color: #959DA5;
    cursor: default;
  }

  #reset_button:active {
    background-color: #EDEFF2;
    box-shadow: rgba(225, 228, 232, 0.2) 0 1px 0 inset;
    transition: none 0s;
  }

  #reset_button:focus {
    outline: 1px transparent;
  }

  #reset_button:before {
    display: none;
  }

  #reset_button:-webkit-details-marker {
    display: none;
  }
"))
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            4,
            selectInput("field_of_work", NULL, choices = append(job_titles_groups, "All fields")),
            plotOutput("line_plot")
          ),
          column(
            4,
            # map
            actionButton("reset_button", "Reset to global data"),
            leafletOutput("map")
          ),
          column(
            4,
            checkboxGroupInput("year", NULL, choices = 2020:2024, selected = 2024, inline = TRUE),
            plotOutput("box_plot")
          )
        ),
        fluidRow(
          column(
            4,
            plotOutput("histogram")
          ),
          column(
            2,
            # plot corresponding to the map
            plotOutput("map_plot"),
          ),
          column(
            2,
            # plot corresponding to the map

              infoBoxOutput("min_salary", width = 12),
              infoBoxOutput("max_salary", width = 12)
          
          ),
          column(
            4,
            plotOutput("bar_plot"),
          )
        ),
      ),
      tabItem(
        tabName = "source",
        dataTableOutput("source_table")
      )
    )
  ),
)


##### Server
server <- function(input, output, session) {
  # source table organised
  output$source_table <- DT::renderDataTable(data)

  # line plot
  output$line_plot <- renderPlot({
    filtered_data <- if (input$field_of_work != "All fields") {
      clean_data %>%
        group_by(job_group, year) %>%
        filter(job_group == input$field_of_work)
    } else {
      clean_data %>%
        group_by(year)
    }




    filtered_data %>%
      summarise(Avreage_Salary = mean(salary, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = Avreage_Salary)) +
      geom_area(fill = "#9933FF", alpha = 5 / 10) +
      geom_line() +
      labs(x = "YEAR", y = "AVREAGE SALARY") +
      #theme_minimal() +
      theme(
        axis.title = element_text(family = "Roboto", size = 12),
        axis.text = element_text(family = "Roboto", size = 10),
        plot.title = element_text(family = "Roboto", size = 14, face = "bold")
      ) +
      geom_label(aes(label = paste(round(Avreage_Salary / 1000), "K")), label.padding = ) +
      scale_y_continuous(limits = c(0, 200000))
  })


  # boxplot
  output$box_plot <- renderPlot({
    purple_palette <- c("#C8A2C8", "#8A2BE2", "#9400D3")
    clean_data %>%
      group_by(company_size, year) %>%
      filter(year %in% input$year) %>%
      ggplot(aes(y = company_size, x = salary, fill = company_size)) + # Swapping x and y
      geom_boxplot() +
      labs(y = "NUMBER OF EMPLOEES IN COMPANY", x = "SALARY") + # Updating axis labels
      scale_x_log10() +
      #theme_minimal() +
      theme(
        axis.title = element_text(family = "Roboto", size = 12),
        axis.text = element_text(family = "Roboto", size = 10),
        plot.title = element_text(family = "Roboto", size = 14, face = "bold")
      ) +
      scale_fill_manual(values = purple_palette) +
      theme(legend.position = "none") # remove legend
  })


  # histogram

  output$histogram <- renderPlot({
    salary_ranges <- c(0, 50000, 200000, 350000, 500000, 750000)

    filtered_data <- if (input$field_of_work != "All fields") {
      clean_data %>%
        filter(job_group == input$field_of_work)
    } else {
      clean_data
    }


    filtered_data %>%
      ggplot(aes(x = salary)) +
      labs(
        x = "SALARY",
        y = "COUNT"
      )+
      geom_histogram(color = "darkslategrey", fill = "darkslategray3") +
      theme(
        axis.title = element_text(family = "Roboto", size = 12),
        axis.text = element_text(family = "Roboto", size = 10),
        plot.title = element_text(family = "Roboto", size = 14, face = "bold")
      ) +
      scale_y_sqrt() +
      scale_x_sqrt() +
      scale_x_continuous(trans = "sqrt", breaks = salary_ranges)
  })


  # map
  country_code <- reactiveVal("Global")

  output$map <- renderLeaflet({
    mapdata <- clean_data %>%
      group_by(employee_residence) %>%
      summarise(count = n()) %>%
      left_join(countries, by = c("employee_residence" = "a2"))



    leaflet(mapdata) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ sqrt(count),
        layerId = ~employee_residence
      ) %>%
      fitBounds(63.720766, 25.533866, -11.718330, 36.037643)
  })

  # click
  observeEvent(input$reset_button, {
    country_code("Global")
  })


  observeEvent(input$map_marker_click, {
    print(input$map_marker_click$id) # debug
    country_code(input$map_marker_click$id) # changing reactiveVal on click
    print(paste("Country code clicked:", country_code())) # debug
  })


  output$map_plot <- renderPlot({
    map_data <- if (country_code() == "Global") {
      clean_data %>%
        count(work_type)
    } else {
      clean_data %>%
        filter(employee_residence == country_code()) %>%
        count(work_type)
    }



    wykres <- ggplot(map_data, aes(x = "", y = n, fill = work_type)) +
      geom_bar(stat = "identity", width = 1) +
      theme_void(legend.position = "bottom") +
      coord_polar(theta = "y") # Convert to pie chart
    
    wykres <- wykres + theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
    
  })


  output$min_salary <- renderValueBox({
    if (country_code() == "Global") {
      infoBox_data <- clean_data
      country_name <- country_code()
    } else {
      infoBox_data <- clean_data %>% filter(employee_residence == country_code())
      country <- countries %>%
        filter(a2 == country_code())
      country_name <- country$name
    }


    min_list <- infoBox_data %>%
      filter(salary == min(salary)) %>%
      summarise(job_title = first(job_title), min_salary = first(salary)) %>%
      as.list()

    valueBox(paste(round(min_list$min_salary / 1000), "K $"),
      subtitle = HTML(paste("Minimum salary in", country_name, "<br><small style='font-size: 12px;'>Position:", min_list$job_title)),
      icon = icon("angle-down"),
      color = "blue"
    )
  })


  output$max_salary <- renderValueBox({
    if (country_code() == "Global") {
      infoBox_data <- clean_data
      country_name <- country_code()
    } else {
      infoBox_data <- clean_data %>% filter(employee_residence == country_code())
      country <- countries %>%
        filter(a2 == country_code())
      country_name <- country$name
    }

    max_list <- infoBox_data %>%
      filter(salary == max(salary)) %>%
      summarise(job_title = first(job_title), max_salary = first(salary)) %>%
      as.list()

    valueBox(paste(round(max_list$max_salary / 1000), "K $"),
      subtitle = HTML(paste("Maximum salary in", country_name, "<br><small style='font-size: 12px;'>Position:", max_list$job_title)),
      icon = icon("angle-up"),
      color = "green"
    )
  })


  output$bar_plot <- renderPlot({
    clean_data %>%
      group_by(experience_level, year) %>%
      filter(year %in% input$year) %>%
      summarise(mean_salary = mean(salary)) %>%
      ggplot(aes(x = experience_level, y = mean_salary, fill = experience_level)) +
      labs(
        x = "EXPERIENCE LEVEL",
        y = "MEAN SALARY"
      ) +
      theme(
        axis.title = element_text(family = "Roboto", size = 12),
        axis.text = element_text(family = "Roboto", size = 10),
        plot.title = element_text(family = "Roboto", size = 14, face = "bold")
      ) +
      geom_col() +
      theme(legend.position = "none")
  })
}





shinyApp(ui = ui, server = server)
