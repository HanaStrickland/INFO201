### SERVER
server <- function(input, output) {
  
  data <- reactiveValues()
  data$selected_county <- ""
  data$selected_info <- ""
  
  
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
  
  
  output$selected <- renderUI({
    data$selected_info
    
  })
  
  observeEvent(input$plot_click, {
    selected <- nearPoints(counties, input$plot_click)
    selected <- filter(selected, year == input$year)
    
    selected <- as.vector(selected)
    
    data$selected_county <- paste(selected$name)

    
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




