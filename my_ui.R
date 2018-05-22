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
    
    selectInput("year", label = "Select Year", choices = years),
    
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
                #plotOutput("plot", brush = "plot_brush"),
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