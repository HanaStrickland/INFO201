library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)


### Read in and Clean Data

counties <- read.csv("data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)


# WA counties
counties <- counties %>%
  filter(parent.location == "Washington") %>% 
  select(year, name, population, poverty.rate, rent.burden, eviction.rate)


years <- unique(counties$year)

eviction_rate_range <- range(counties$eviction.rate, na.rm = TRUE)


ui <- fluidPage(
  titlePanel("Eviction Rates"),

  sidebarLayout(
    selectInput("year", label = "Select Year", choices = years),

    sliderInput("eviction_rate",
                label = "Eviction Rate",
                min = eviction_rate_range[1],
                max = eviction_rate_range[2],
                value = eviction_rate_range
    ),

  ),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotOutput("plot", click = "plot_click"),
                         p("This plot shows eviction data for", strong(textOutput("selected_year", inline = TRUE)))
                         ),
                p("Highlighting:", strong(textOutput("selected", inline = TRUE))
                ),
                tabPanel("Table", dataTableOutput("table")),
                tabPanel("Dataset Information",
                         p("This research uses data from The Eviction Lab at Princeton University,
                           a project directed by Matthew Desmond and designed by Ashley Gromis,
                           Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung,
                           and Adam Porton. The Eviction Lab is funded by the JPB, Gates,
                           and Ford Foundations as well as the Chan Zuckerberg Initiative.
                           More information is found at evictionlab.org."))
                         )

                         )

                )

server <- function(input, output) {

  data <- reactiveValues()
  data$selected_county <- ""

  output$plot <- renderPlot({

    

    results_data <- counties[counties$year == input$year, ] 
    
    results_data <- results_data %>%
      filter(eviction.rate >= input$eviction_rate[1] & eviction.rate <= input$eviction_rate[2])

    ggplot(data = results_data, mapping = aes(x = poverty.rate, y = eviction.rate)) +
      geom_point(aes(color = (name %in% data$selected_county), size = 2)) +
      guides(color = FALSE)

  })

  output$table <- renderDataTable({
    results_data <- counties[counties$year == input$year, ]
    
    results_data <- results_data %>%
      filter(eviction.rate >= input$eviction_rate[1] & eviction.rate <= input$eviction_rate[2])
    
    results_data


  })

  output$selected_year <- renderText({input$year})
  
  output$selected <- renderText({
    data$selected_county

  })

  observeEvent(input$plot_click, {
    selected <- nearPoints(counties, input$plot_click)
    data$selected_county <- 
      paste(unique(selected$name),
             "Population:", unique(selected$population), 
             "Poverty Rate: ", unique(selected$poverty.rate),
             "Rent Burden: ", unique(selected$rent.burden),
            "Eviction Rate: ", unique(selected$eviction.rate)
            )

  })
 
  

}

shinyApp(ui, server)



