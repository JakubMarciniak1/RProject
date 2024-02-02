#### Shiny
library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(leaflet)
library(shinythemes)

## TODO: custom_theme for plots

options(shiny.launch.browser = TRUE) # open in browser instead of rstudio
countries <- DescTools::d.countries # database with coords for country code

getwd()
setwd("C:\\Moje rzeczy\\Uczelnia UAM\\matematyka zaoczna\\semestr 3\\Przetwarzanie i wizualizacja danych\\projekty R\\Pati i ja\\RProject")

data_clean <- ".\\resources\\Data_cleaning.R"

## load data
source(data_clean) # executing the data cleaning script

### UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Salaries 2020-2024"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Source", tabName = "source", icon = icon("table"))
    )
  ),
  dashboardBody(
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
            4,
            # plot corresponding to the map
            plotOutput("map_plot"),
            fluidRow(
              infoBoxOutput("min_salary", width = 6),
              infoBoxOutput("max_salary", width = 6)
            )
          ),
          column(
            4,
            plotOutput("bar_plot")
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
      theme_minimal() +
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
      theme_minimal() +
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
      geom_histogram(color = "darkslategrey", fill = "darkslategray3") +
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
      )
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



    ggplot(map_data, aes(x = "", y = n, fill = work_type)) +
      geom_bar(stat = "identity", width = 1) +
      theme_void() +
      coord_polar(theta = "y") # Convert to pie chart
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
      geom_col() +
      theme(legend.position = "none")
  })
}





shinyApp(ui = ui, server = server)
