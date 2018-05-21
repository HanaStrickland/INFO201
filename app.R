library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)



# Can't get year isolated in ObserveEvent
# Want "selected" to be on multiple lines


### Read in and Clean Data

counties <- read.csv("data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)


# WA counties
counties <- counties %>%
  filter(parent.location == "Washington") %>% 
  select(year, name, population, poverty.rate, rent.burden, eviction.rate)

years <- unique(counties$year)

eviction_rate_range <- range(counties$eviction.rate, na.rm = TRUE)


ui <- fluidPage(
  titlePanel("Eviction Rates in Washington by County"),

  sidebarLayout(
    selectInput("year", label = "Select Year", choices = years, multiple = F),

    sliderInput("eviction_rate",
                label = "Eviction Rate",
                min = eviction_rate_range[1],
                max = eviction_rate_range[2],
                value = eviction_rate_range
    )

  ),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", p("This plot shows the relationship between rent burden
                                   (the percentage of income spent on rent) and eviction rate for",
                           strong(textOutput("selected_year_plot", inline = TRUE)), ". There seems to be a
                           low to negligible correlation between rent burden and eviction rate."
                           ),
                         plotOutput("plot", brush = "plot_brush"),
                         p("Highlighting:", strong(textOutput("selected", inline = TRUE))
                           )
                         ),

                tabPanel("Table",
                         p("This table shows a county's population, poverty rate, rent burden, and eviction rate for",
                           strong(textOutput("selected_year_table", inline = TRUE)), ". You can select a different year
                           using the dropdown menu above, and you can filter for a different range of eviction rates
                           using the slider above."
                           ),
                         dataTableOutput("table")
                         ),

                tabPanel("Dataset Information",
                         p("This research uses data from The Eviction Lab at Princeton University,
                           a project directed by Matthew Desmond and designed by Ashley Gromis,
                           Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung,
                           and Adam Porton. The Eviction Lab is funded by the JPB, Gates,
                           and Ford Foundations as well as the Chan Zuckerberg Initiative.
                           More information is found at",
                           a("evictionlab.org", href="https://data-downloads.evictionlab.org"), "."))


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

    ggplot(data = results_data, mapping = aes(x = rent.burden, y = eviction.rate)) +
      geom_point(aes(color = (name %in% data$selected_county)), size = 3) +
      guides(color = FALSE) +
      labs(title = "Relationship Between Rent Burden and Eviction Rate") +

      xlab("Rent Burden") +
      ylab("Eviction Rate") +
      theme(plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))


  })

  output$table <- renderDataTable({
    results_data <- counties[counties$year == input$year, ]

    results_data <- results_data %>%
      filter(eviction.rate >= input$eviction_rate[1] & eviction.rate <= input$eviction_rate[2])

    results_data


  })

  output$selected_year_plot <- renderText({input$year})
  output$selected_year_table <- renderText({input$year})


  output$selected <- renderText({
    data$selected_county

  })

  observeEvent(input$plot_brush, {
    selected <- brushedPoints(counties, input$plot_brush)
    selected <- filter(selected, year == input$year)
    selected <- as.vector(selected)



    data$selected_county <-
     paste(selected[2],
                "Population: ", selected[3],
                "Poverty Rate: ", selected[4],
                "Rent Burden: ", selected[5],
                "Eviction Rate: ", selected[6])

  })



}

shinyApp(ui, server)





