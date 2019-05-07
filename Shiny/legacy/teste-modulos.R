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

library(rsconnect)


#shinyapps.io

setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/R")

source("functions.R")
source("ui_modules.R")
source("server_modules.R")
# Read the data
setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/Shiny")
Kanyama <- read.csv("Shiny_plot.csv", stringsAsFactors = F)

# Number of Ocurrences ----
# Creating the first group
Daily.plot <- Kanyama %>% 
  group_by(date.full) %>% 
  summarise(number.of.cases = n()) %>% 
  filter(!is.na(date.full)) %>% ungroup()

Daily.plot$date.full <- as.Date(Daily.plot$date.full)

# Creating the second group
Monthly.plot <- Daily.plot
Monthly.plot$date.full <- format(as.Date(Monthly.plot$date.full), "%Y-%m-01")
Monthly.plot$date.full <- as.Date(Monthly.plot$date.full)

# Lines for each kind of transport
type.transport <- Kanyama %>% 
  group_by(date.full, Transport) %>% 
  summarise(number.of.cases = n()) %>% 
  filter(!is.na(date.full)) %>% 
  ungroup()

type.transport$date.full <- as.Date(type.transport$date.full)

type.transport.monthly <- type.transport
type.transport.monthly$date.full <- format(as.Date(type.transport.monthly$date.full), "%Y-%m-01")
type.transport.monthly$date.full <- as.Date(type.transport.monthly$date.full)

# Vacuum Tanker only

vt.daily <- type.transport %>% filter(Transport == "Vacuum Tanker")
vt.monthly <- type.transport.monthly %>% filter(Transport == "Vacuum Tanker")

# Light Truck only 

lt.daily <- type.transport %>% filter(Transport == "Light Truck")
lt.monthly <- type.transport.monthly %>% filter(Transport == "Light Truck")

# Push Cart only 

pc.daily <- type.transport %>% filter(Transport == "Push Cart")
pc.monthly <- type.transport.monthly %>% filter(Transport == "Push Cart")


# Waste volume ----
Volume.level <- Kanyama %>%
  group_by(date.full) %>% 
  summarise(total.waste = sum(area.m3)) %>% 
  filter(!is.na(date.full)) %>% 
  ungroup()

Volume.level$date.full <- as.Date(Volume.level$date.full)

# Monthly view

Volume.level.month <- Volume.level
Volume.level.month$date.full <- format(as.Date(Volume.level.month$date.full), "%Y-%m-01")
Volume.level.month$date.full <- as.Date(Volume.level.month$date.full)

# Volume per transport

Volume.type.transport <- Kanyama %>% 
  group_by(date.full, Transport) %>% 
  summarise(total.waste = sum(area.m3)) %>% 
  filter(!is.na(date.full)) %>% 
  ungroup()

Volume.type.transport$date.full <- as.Date(Volume.type.transport$date.full)

Volume.type.transport.month <- Volume.type.transport
Volume.type.transport.month$date.full <- format(as.Date(Volume.type.transport.month$date.full), "%Y-%m-01")
Volume.type.transport.month$date.full <- as.Date(Volume.type.transport.month$date.full)


# Vacuum Tanker only

Volume.vt.daily <- Volume.type.transport %>% filter(Transport == "Vacuum Tanker")
Volume.vt.monthly <- Volume.type.transport.month %>% filter(Transport == "Vacuum Tanker")

# Light Truck only 

Volume.lt.daily <- Volume.type.transport %>% filter(Transport == "Light Truck")
Volume.lt.monthly <- Volume.type.transport.month %>% filter(Transport == "Light Truck")

# Push Cart only 

Volume.pc.daily <- Volume.type.transport %>% filter(Transport == "Push Cart")
Volume.pc.monthly <- Volume.type.transport.month %>% filter(Transport == "Push Cart")


# Map part ----

# Applied to the total, and any kind of transports needed
# General points

Map.Kanyama <- Kanyama %>% select(X1.9..latitude., X1.9..longitude.,X1.9..altitude., date.full, Transport, area.m3) %>% 
  filter(!is.na(X1.9..altitude.) & X1.9..latitude. > -16 & X1.9..longitude. > 28 & X1.9..altitude. > 0 ) %>% 
  mutate(kind = case_when(area.m3 == 4.5 ~ "Pit Latrine",
                          area.m3 == 12.5 ~ "Septic Tank"))

pal_disposal <- colorFactor(c("#e41a1c", "#377eb8", "#4daf4a"), domain = Map.Kanyama$Transport)
transportColors <- c("Light Truck" = "#e41a1c", "Push Cart" = "#377eb8", "Vacuum Tanker" = "#4daf4a")
transportScale <- scale_fill_manual(values = transportColors, limits = names(transportColors) )

Map.Kanyama$date.full <- as.Date(Map.Kanyama$date.full)

# Using the altitude to make a topographic plot ----

min.altitude <- min(Map.Kanyama$X1.9..altitude., na.rm = T)
max.altitude <- max(Map.Kanyama$X1.9..altitude., na.rm = T)


Map.Kanyama$factor.altitude <- cut(Map.Kanyama$X1.9..altitude., 
                                   breaks = c(min.altitude, 1250, 1260, 1270, 1280, 1290, 1300, max.altitude),
                                   labels = c("Under 1250m","1250 to 1260 m", "1260 to 1270 m","1270 to 1280 m","1280 to 1290 m","1290 to 1300 m","above 1300 m"),
                                   include.lowest = T)


altitude.pal <- colorFactor(c("#d73027", "#fc8d59","#fee08b", "#ffffbf", "#d9ef8b", "#91cf60", "#1a9850"), domain = Map.Kanyama$factor.altitude)
altitudeColors <- c("Under 1250m" = "#d73027",
                    "1250 to 1260 m" = "#fc8d59",
                    "1260 to 1270 m" = "#fee08b",
                    "1270 to 1280 m" = "#ffffbf",
                    "1280 to 1290 m" = "#d9ef8b",
                    "1290 to 1300 m" = "#91cf60",
                    "above 1300 m" = "#1a9850")
altitudeScale <- scale_fill_manual(values = altitudeColors, limits = names(altitudeColors) )


# Named Attributes ----
values <-c("All together" = "all",
           "Vacuum Tanker" = "vt",
           "Light Truck" = "lt",
           "Push Cart" = "pc")

names.occ <- c("soft.plot", "time.plot", "monthly", "daily")

names.m3 <- c("m3.soft", "m3.raw", "m3.monthly", "m3.daily")
# User interface ----
ui <- tagList(
  tags$head(tags$script(type="text/javascript", src = "code.js")),
  navbarPage(title = "Predicted dates for Waste Collection in Kanyama", id = "nav", theme = "style.css",
                 
                 tabPanel("Number of Occurences", value = 0,
                          
                          sidebarLayout(
                            
                          
                            defaultSidebarPanel("occ", date.select = Kanyama$date.full, radio.options = values),
                            
                            defaultMainPanel("occ", names.occ)
                            
                          )
                 ),
                 tabPanel("Amount of waste (m3)", value = 1,
                          
                          sidebarLayout(
                            defaultSidebarPanel("m3", date.select = Kanyama$date.full, radio.options = values),
                            
                            defaultMainPanel("m3", names.m3)
                            
                          )
                          
                          
                 ),
                 tabPanel("Maps", value = 2,
                          
                          
                            tabsetPanel(type = "tabs", id = "map.tabset",
                                        tabPanel("Preditcion of waste collection",
                                                 leafletOutput(outputId = "map", height = 700),
                                                 absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                                               draggable = F, top =140, left = "auto", right = 20, bottom = "auto",
                                                               width = 330, height = "auto",
                                                               
                                                               sliderInput("map_year_sel", "Select a year:", 
                                                                           min(year(Kanyama$date.full), na.rm = T),
                                                                           2025,
                                                                           value = 2019,
                                                                           sep = ""),
                                                               plotOutput("quant.toilets", height = "250px"),
                                                               plotlyOutput("type.toilets", height = "250px"))),
                                        tabPanel("Topology",
                                                 leafletOutput(outputId = "altitude", height = 700),
                                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                               draggable = F, top = 140, left = "auto", right = 20, bottom = "auto",
                                                               width = 330, height = "auto",
                                                               
                                                               h1("Stats"),
                          
                                                               plotOutput("stats.top", width = 300, height = 300))
                                                 )
                                        
                                        
                            )
                            ),
              
                 tabPanel("Misc", value = 3, useShinyjs())
                          

))




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

# Running ----
shinyApp(ui  = ui, server = server, options = list(height = 1080))
rsconnect::deployApp(paste0(getwd(), '/teste-modulos.R'))
