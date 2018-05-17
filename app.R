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

    plotOutput("plot")
  )
  

)

server <- function(input, output) {

  
  output$plot <- renderPlot({
    results_data <- counties %>%
      filter(year > input$year[1] & year < input$year[2])

    p <- ggplot(data = results_data) +
        geom_point((mapping = aes(x = year, y = eviction.rate, color = median.household.income))) +
        scale_color_gradientn(colors = c("blue", "orange"))
    p


  })

}


shinyApp(ui, server)

