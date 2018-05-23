counties <- read.csv("data/evictionlab-us-counties.csv", stringsAsFactors = FALSE)

counties <- counties %>%
  filter(parent.location == "Washington") %>%
  select(year, name, population, poverty.rate, rent.burden, eviction.rate)

years <- unique(counties$year)

eviction_rate_range <- range(counties$eviction.rate, na.rm = TRUE)

### UI
ui <- fluidPage( 
  titlePanel("Eviction Rates in Washington by County"),
  
  sidebarLayout(
    sidebarPanel(
      
      # widget selects year
      selectInput("year", label = "Select Year", choices = years), 
      
      # widget filters for eviction rate
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
                                     strong(textOutput("selected_year_plot", inline = TRUE)), 
                                     ". Each data point represents a county. You can select a different 
                                   year using the dropdown menu above, and you can filter for a different 
                                   range of eviction rates using the slider above. There seems to be a 
                                   low to negligible correlation between rent burden and eviction rate."
                                     
                  ), 
                  p("Click on a data point to learn more about it."),
                  
                  plotOutput("plot", click = "plot_click"),
                  
                  p(htmlOutput("selected", inline = TRUE)
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
                           h3("About the Data"),
                           p("This research uses data from The Eviction Lab at Princeton University,
                           a project directed by Matthew Desmond and designed by Ashley Gromis,
                           Lavar Edmonds, James Hendrickson, Katie Krywokulski, Lillian Leung,
                           and Adam Porton. The Eviction Lab is funded by the JPB, Gates,
                           and Ford Foundations as well as the Chan Zuckerberg Initiative.
                           More information is found at", 
                             a("evictionlab.org", href="https://data-downloads.evictionlab.org"), "."),
                           h3("Terms"),
                           p(strong("Poverty Rate:"),  "% of the population with income in the past 12 months 
                             below the poverty level"),
                           p(strong("Rent Burden:"), "Median gross rent as a percentage of household income"),
                           p(strong("Eviction Rate:"), "Ratio of the number of renter-occupied households in an 
                             area that received an eviction judgement in which renters were ordered to 
                             leave")
                           ) 
                  
                  
      )
    )
  )
)