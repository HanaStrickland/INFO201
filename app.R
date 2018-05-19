library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)


### Read in and Clean Data

counties <- read.csv("data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)


# WA counties
counties <- counties %>%
  filter(parent.location == "Washington")

counties_long <- gather(counties,
                        key = race,
                        value = pct.pop,
                        c(pct.white:pct.other )
                        )

years <- unique(counties_long$year)

eviction_rate_range <- range(counties_long$eviction.rate, na.rm = TRUE)


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
                         p("Highlighting:", strong(textOutput("selected", inline = TRUE)))),
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
  data$selected_racial <- ""

  output$plot <- renderPlot({

    results_data <- counties_long %>%
      filter(eviction.rate >= input$eviction_rate[1] & eviction.rate <= input$eviction_rate[2])

    results_data <- counties_long[counties_long$year == input$year, ]

    ggplot(data = results_data, mapping = aes(x = pct.pop, y = eviction.rate)) +
      geom_point(aes(color = (race %in% data$selected_racial), size = 2)) +
      guides(color = FALSE)

  })

  output$table <- renderDataTable({
    results_data <- counties_long %>%
      filter(eviction.rate >= input$eviction_rate[1] & eviction.rate <= input$eviction_rate[2])

    results_data <- counties_long[counties_long$year == input$year, ]
    results_data


  })

  output$selected <- renderText({
    data$selected_racial

  })

  observeEvent(input$plot_click, {
    selected <- nearPoints(counties_long, input$plot_click)
    data$selected_racial <- unique(selected$race)

  })

}

shinyApp(ui, server)



