### SERVER
server <- function(input, output) {
  
  data <- reactiveValues()
  data$selected_county <- ""
  data$selected_info <- ""
  
  results_data <- reactive({
    
    results <- counties[counties$year == input$year, ]
    
    results <- results %>%
      filter(eviction.rate >= input$eviction_rate[1] & eviction.rate <= input$eviction_rate[2])
    
    results
    
    
  })
  
  
  output$plot <- renderPlot({
    
    ggplot(data = results_data(), mapping = aes(x = rent.burden, y = eviction.rate)) +
      geom_point(aes(color = (name %in% data$selected_county)), size = 3) +
      guides(color = FALSE) +
      labs(title = "Relationship Between Rent Burden and Eviction Rate in WA Counties") +
      
      xlab("Rent Burden (%)") +
      ylab("Eviction Rate") +
      theme(plot.title = element_text(size = 20), 
            axis.title.x = element_text(size = 15),  
            axis.title.y = element_text(size = 15)) 
    
    
  })
  
  output$table <- renderDataTable({
    
    results_data <- results_data()
    
    colnames(results_data) <- c("Year", "County", "Population", "Poverty Rate", "Rent Burden", "Eviction Rate")
    
    results_data
    
    
  })
  
  # part of parapgraph above plot/table that lets users know what year they're looking at
  output$selected_year_plot <- renderText({input$year})
  output$selected_year_table <- renderText({input$year})
  
  # renders info that appears below the plot
  output$selected <- renderUI({
    data$selected_info
    
  })
  
  observeEvent(input$plot_click, {
    selected <- nearPoints(counties, input$plot_click)
    selected <- filter(selected, year == input$year) # filters for year picked in the dropdown menu
    
    selected <- as.vector(selected)  
    
    data$selected_county <- paste(selected$name) # used to highlight datapoint selected in a different color

    # info that appears below the plot
    data$selected_info <-
      HTML(
        paste(
       "Highlighting ", strong(unique(selected[2])), "<br>",
        em("Population: ", selected[3]), "<br>",
        em("Poverty Rate: ", selected[4]), "<br>",
        em("Rent Burden: ", selected[5]), "<br>",
        em("Eviction Rate: ", selected[6])
            )
       )
  })
  
  
  
}




