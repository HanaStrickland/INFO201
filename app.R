library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

### Read in and Clean Data

counties <- read.csv("data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)
cities <- read.csv("data/cities.csv", stringsAsFactors = FALSE) # cities in WA


counties <- counties %>% 
  filter(parent.location == "Washington") # WA counties

cities <- cities %>% 
  filter(name == "Seattle")

year_range <- range(counties$year)
eviction_rate_range <- range(counties$eviction.rate, na.rm = TRUE)


ui <- fluidPage(
  titlePanel("Eviction Rates"),
  
  sidebarLayout(
    sliderInput("year",
                label = "Year from 2000 to 2016",
                min = year_range[1],
                max = year_range[2],
                value = year_range
    ),
    sliderInput("eviction_rate",
                label = "Eviction Rate",
                min = eviction_rate_range[1],
                max = eviction_rate_range[2],
                value = eviction_rate_range
    )
  ),
  
  mainPanel(
    plotOutput("plot", click = "plot_click"),
    textOutput("selected")
    #p("Highlighting:", strong(textOutput("selected", inline = TRUE)))
    
  )
  
  
  
  
)

server <- function(input, output) {
  
  data <- reactiveValues()
  data$selected_year <- ""
  
  output$plot <- renderPlot({
  
    results_data <- counties %>%
      filter(year > input$year[1] & year < input$year[2], 
             eviction.rate > input$eviction_rate[1] & eviction.rate < input$eviction_rate[2]) 
    
    ggplot(data = results_data, mapping = aes(x = poverty.rate, y = eviction.rate)) +
      geom_point(aes(color = (year %in% data$selected_year))) +
      guides(color = FALSE)
    
    
  })
  
  output$selected <- renderText({
    data$selected_year
    
    
  })
  
  observeEvent(input$plot_click, {
    selected <- nearPoints(counties, input$plot_click)
    data$selected_year <- unique(selected$year)
    
    
    
  })
  
}


shinyApp(ui, server)

