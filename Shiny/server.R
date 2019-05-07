library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

# Server ----
server <- function(input, output, session) {
  
  values <- reactiveValues(active = FALSE)
  
  # callModule(startingSettings, "input",values)
  
  observeEvent(input$nav,{
    if(input$nav == 0){
      callModule(refreshButton, "input",values)
    }
    else{
      callModule(refreshButton, "m3",values)
    }
    
  })
  
  
  radioActivation <- callModule(disableRadioOptions, "occ", values)
  
  toogleView <- callModule(activateDetailedView , "occ", values)
  
  radioActivation.m3 <- callModule(disableRadioOptions, "m3", values)
  
  toogleView.m3 <- callModule(activateDetailedView , "m3", values)
  
  # RadioButtons' Variable
  sel.month <- reactive({
    
    selection <- case_when( values$active == TRUE ~ 1,
                            values$active == FALSE ~ 0)
  })
  
  answer <- reactiveValues( plot = NULL)
  
  occ.selection <- callModule(plotSelected, "occ")
  
  observe({ 
    answer$plot <- occ.selection$sel
  })
  
  #Selecting the dataframes
  sel <- reactive({
    selection <- switch (answer$plot,
                         all = list(type.transport.monthly, type.transport),
                         vt = list(vt.monthly, vt.daily),
                         lt = list(lt.monthly, lt.daily),
                         pc = list(pc.monthly, pc.daily)
    )
  })
  
  
  sel.m3 <- reactive({
    selection <- switch (answer$plot,
                         all = list(Volume.type.transport.month, Volume.type.transport),
                         vt = list(Volume.vt.monthly, Volume.vt.daily),
                         lt = list(Volume.lt.monthly, Volume.lt.daily),
                         pc = list(Volume.pc.monthly, Volume.pc.daily)
    )
  })
  #Number of Occurences ----
  
  # First Render Plot
  output$time.plot <- renderPlot({
    
    if(sel.month() == 0){
      
      result<- callModule(sliderPlotProcessing, "occ", Daily.plot)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, title = "Smooth time series plot")
      
      p
    }
    
  })
  
  
  # Second Render Plot
  output$soft.plot <- renderPlot({
    
    if(sel.month() == 0){
      
      result<- callModule(sliderPlotProcessing, "occ", Monthly.plot)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, title = "Smooth time series plot", method = geom_smooth)
      
      p
    } else {
      result<- callModule(dateRangePlotProcessing, "occ", Daily.plot)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, title = "Smooth time series plot", colour = "orange")
      
      p
    }
  })
  
  
  output$monthly <- renderPlot({
    
    
    if(sel.month() == 0){
      result <- sel()[[1]]
      result<- callModule(sliderPlotProcessing, "occ", result)
      
      colour <- colourTest(result$Transport)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, colour = colour, title = "Smooth time series plot", method = geom_smooth)
      
      p
    } else {
      result <- sel()[[2]]
      result <- callModule(dateRangePlotProcessing, "occ", result)
      
      colour <- colourTest(result$Transport)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, colour = colour, title = "Smooth time series plot")
      
      p
    }
    
  })
  
  output$daily <- renderPlot({
    
    if(sel.month() == 0){
      result <- sel()[[2]]
      result <- callModule(sliderPlotProcessing, "occ", result)
      
      colour <- colourTest(result$Transport)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, colour = colour, title = "Daily time series plot")
      
      p
      
    }
  })
  
  # Volume ----
  
  
  output$m3.raw <- renderPlot({
    
    if(sel.month() == 0){
      
      result<- callModule(sliderPlotProcessing, "m3", Volume.level)
      
      p <- plot.time.series(result,
                            result$date.full, result$total.waste, title = "Smooth time series plot", y.name = "Amount of waste in m3")
      
      p
    }
    
  })
  
  
  # Second Render Plot
  output$m3.soft <- renderPlot({
    
    if(sel.month() == 0){
      
      result<- callModule(sliderPlotProcessing, "m3", Volume.level.month)
      
      p <- plot.time.series(result,
                            result$date.full, result$total.waste, y.name = "Amount of waste in m3", title = "Smooth time series plot", method = geom_smooth)
      
      p
    } else {
      result<- callModule(dateRangePlotProcessing, "m3", Volume.level)
      
      p <- plot.time.series(result,
                            result$date.full, result$total.waste, y.name = "Amount of waste in m3",title = "Smooth time series plot", colour = "orange")
      
      p
    }
  })
  
  
  output$m3.monthly <- renderPlot({
    
    
    if(sel.month() == 0){
      result <- sel.m3()[[1]]
      result<- callModule(sliderPlotProcessing, "m3", result)
      
      colour <- colourTest(result$Transport)
      
      p <- plot.time.series(result,
                            result$date.full, result$total.waste, colour = colour, title = "Smooth time series plot", y.name = "Amount of waste in m3", method = geom_smooth)
      
      p
    } else {
      result <- sel.m3()[[2]]
      result <- callModule(dateRangePlotProcessing, "m3", result)
      
      colour <- colourTest(result$Transport)
      
      p <- plot.time.series(result,
                            result$date.full, result$total.waste, colour = colour, title = "Smooth time series plot", y.name = "Amount of waste in m3")
      
      p
    }
    
  })
  
  output$m3.daily <- renderPlot({
    
    if(sel.month() == 0){
      result <- sel.m3()[[2]]
      result <- callModule(sliderPlotProcessing, "m3", result)
      
      colour <- colourTest(result$Transport)
      
      p <- plot.time.series(result,
                            result$date.full, result$total.waste, colour = colour, title = "Daily time series plot", y.name = "Amount of waste in m3")
      
      p
      
    }
  })
  
  # Map ----
  
  boundingBox <- reactive({
    
    if(!is.null(input$map_bounds)){
      Map.Kanyama <- in_bounding_box(Map.Kanyama, Map.Kanyama$X1.9..latitude., Map.Kanyama$X1.9..longitude., input$map_bounds)
    } else {
      Map.Kanyama
    }
    
  })
  
  output$map <- renderLeaflet({
    
    
    Map.Kanyama <- Map.Kanyama %>% filter( year(date.full) == input$map_year_sel)
    
    print(nrow(Map.Kanyama))
    leaflet(data = Map.Kanyama) %>% addProviderTiles( providers$Esri.WorldTopoMap) %>% 
      addCircleMarkers(lng = ~X1.9..longitude.,
                       lat = ~X1.9..latitude.,
                       color = ~pal_disposal(Map.Kanyama$Transport),
                       radius = 7, fillOpacity = 0.3, stroke = T) %>% 
      addLegend("bottomleft", 
                pal = pal_disposal, 
                values = ~Map.Kanyama$Transport,
                title = "Type of Transport",
                na.label = "Not Available")
    
    
  })
  
  output$quant.toilets <- renderPlot({
    
    Map.Kanyama <- boundingBox()
    plot <- ggplot(data = Map.Kanyama, aes(x = Transport)) +
      geom_bar(aes(fill = Transport),
               stat = 'count', show.legend = F) + 
      geom_text(stat = 'count' ,
                aes(label = ..count.., group = Transport), vjust = -.5) +
      theme_minimal() +
      labs(y = "Number of Occurrences", x = "") +
      transportScale
    
    # Expanding the plot's top
    plot + scale_y_continuous(limits = c(0, max(ggplot_build(plot)$data[[1]]$count) * 1.1))
    
    
  })
  
  output$type.toilets <- renderPlotly({
    
    Map.Kanyama <- boundingBox()
    
    colors <- c( 'rgb(128,133,133)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    plot <- plot_ly(Map.Kanyama, labels=~kind, type = "pie",
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = paste("\t Type of full toilets in", input$map_year_sel),
             font = list(family = "Helvetica", bold = F),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = FALSE,
             margin = list( l = 0, r = 10,b = 0, t = 30)) %>% config(displayModeBar = F)
    
    plot
    
  })
  
  output$altitude <- renderLeaflet({
    
    leaflet(data = Map.Kanyama) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      addCircleMarkers(lng = ~X1.9..longitude.,
                       lat = ~X1.9..latitude.,
                       color = ~altitude.pal(Map.Kanyama$factor.altitude),
                       radius = 2, fillOpacity = 0.3, stroke = T) %>% 
      addLegend("bottomleft", 
                pal = altitude.pal, 
                values = ~Map.Kanyama$factor.altitude,
                title = "Altitude")
    
    
    
    
  })
  
  output$stats.top <- renderPlot({
    
    plot <- ggplot(data = Map.Kanyama, aes(x = factor.altitude)) +
      geom_bar(aes(fill = factor.altitude),
               stat = 'count', show.legend = F) + 
      geom_text(stat = 'count',
                aes(label = ..count.., group = factor.altitude), vjust = -.5) +
      theme_minimal() +
      labs(y = "Number of toilets", x = "Altitude") +
      theme(axis.text.x = element_blank()) +
      altitudeScale
    
    # Expanding the plot's top
    plot + scale_y_continuous(limits = c(0, max(ggplot_build(plot)$data[[1]]$count) * 1.1))
    
  })
}
