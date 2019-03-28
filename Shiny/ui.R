library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

#shinyapps.io

setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/R")

source("functions.R")
# Read the data
setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/Shiny")
Kanyama <- read.csv("Shiny_plot.csv", stringsAsFactors = F)

# All in one ----
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

# Lines for each kind of transport ----
type.transport <- Kanyama %>% 
  group_by(date.full, Transport) %>% 
  summarise(number.of.cases = n()) %>% 
  filter(!is.na(date.full)) %>% 
  ungroup()

type.transport$date.full <- as.Date(type.transport$date.full)

type.transport.monthly <- type.transport
type.transport.monthly$date.full <- format(as.Date(type.transport.monthly$date.full), "%Y-%m-01")
type.transport.monthly$date.full <- as.Date(type.transport.monthly$date.full)

# Vacuum Tanker only ----

vt.daily <- type.transport %>% filter(Transport == "Vacuum Tanker")
vt.monthly <- type.transport.monthly %>% filter(Transport == "Vacuum Tanker")

# Light Truck only ----

lt.daily <- type.transport %>% filter(Transport == "Light Truck")
lt.monthly <- type.transport.monthly %>% filter(Transport == "Light Truck")

# Push Cart only ----

pc.daily <- type.transport %>% filter(Transport == "Push Cart")
pc.monthly <- type.transport.monthly %>% filter(Transport == "Push Cart")


# Map part ----

# Applied to the total, and any kind of transports needed
# General points

Map.Kanyama <- Kanyama %>% select(X1.9..latitude., X1.9..longitude., date.full, Transport)


# User interface ----
ui <- navbarPage(title = "Predicted dates for Waste Collection in Kanyama", theme = shinytheme("united"),
  tabPanel("Time series",
    sidebarLayout(
      
      sidebarPanel(
        
        radioButtons(inputId = "plot.selection", label = "Type of graph: ",
                     c("All together" = "all",
                       "Vacuum Tanker" = "vt",
                       "Light Truck" = "lt",
                       "Push Cart" = "pc")),
        br(),
        
        sliderInput(inputId = "year.sel",
                    label = "Select a Year",
                    value = 2019,
                    min = min(year(Kanyama$date.full), na.rm = T),
                    max = 2025, 
                    sep = ""),
        br(),
        
        actionBttn("detail", "Enable monthly analysis",
                   size = "sm", 
                   color = "primary",
                   style = "jelly"
                   ),
        
        br(),
        br(),
        
        dateRangeInput("month.selection", "Month Selection",
                       min = min(Kanyama$date.full, na.rm = T),
                       max = "2025-12",
                       end = "2019-12",
                       format = "M-yyyy",
                       startview = "year"
                       )
        ),
        
      mainPanel(
        
        tabsetPanel(type = "tabs", id = "tabset",
                    tabPanel("General",
                             plotOutput(outputId = "soft.plot", height = "400px"),
                             plotOutput(outputId = "time.plot")),
                    tabPanel("Transports",
                             plotOutput(outputId = "monthly"),
                             plotOutput(outputId = "daily")
                             )
        
        ),
        br()
      )
      
    )
  ),
  
  tabPanel("Maps",
           
           tags$head(tags$style(
             HTML('
                  #ap {align: center; background-color: rgba(217,217,214,0.9);;}
                  #map.year.sel { background-color: rgba(217,217,214,1);}'))),

    leafletOutput(outputId = "map", height = 700),
    
    absolutePanel(top =80, right = 30, id = "ap",
                  sliderInput("map.year.sel", "Select a year", 
                              min(year(Kanyama$date.full), na.rm = T),
                              2025,
                              value = 2019,
                              sep = "")
                  )
               
  ),
               
               
               
               
           
  tabPanel("Misc"),
  
  useShinyjs()
)

# Server ----
server <- function(input, output, session) {

  values <- reactiveValues(active = FALSE)
  
  observe({
    disable("month.selection")
    updateActionButton(session, inputId = "detail", label = "Enable month selection" )
    values$active = FALSE
    disable("change")
    if(input$tabset == 'General') {
      disable("plot.selection")
    } else {
      enable("plot.selection")
    }
  })
  
  
  observeEvent(input$detail,{
    values$active = !values$active
    toggleState(id = "month.selection")
    toggleState(id = "change")
    if(values$active == T){
    updateActionButton(session, inputId = "detail", label = "Disable month selection" )
    
    } else {
      updateActionButton(session, inputId = "detail", label = "Enable month selection" )
    }
    
  })
  
  # RadioButtons' Variable
  sel.month <- reactive({
    
    selection <- case_when( values$active == TRUE ~ 1,
                            values$active == FALSE ~ 0)
  })
  
  
  sel <- reactive({
    selection <- switch (input$plot.selection,
                   all = list(type.transport.monthly, type.transport),
                   vt = list(vt.monthly, vt.daily),
                   lt = list(lt.monthly, lt.daily),
                   pc = list(pc.monthly, pc.daily)
    )
  })
  
  
  # First Render Plot
  output$time.plot <- renderPlot({

    if (sel.month() == 0) {
      result<- filter(Daily.plot, year(Daily.plot$date.full) == input$year.sel)
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, title = "Daily time series plot")
      
      p
    }
    

    
  })
                              

  # Second Render Plot
  output$soft.plot <- renderPlot({

    if (sel.month() == 0) {
      result<- filter(Monthly.plot, year(Monthly.plot$date.full) == input$year.sel)
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, title = "Smooth time series plot", method = geom_smooth)
      
      p
    } else {
      result<- filter(Daily.plot, 
                      Daily.plot$date.full > input$month.selection[1] & Daily.plot$date.full < input$month.selection[2] )
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, title = "Smooth time series plot", colour = "orange")
      
      p
    }
    
  })
  
  output$monthly <- renderPlot({
    
    
    if(sel.month() == 0){
      result <- sel()[[1]]
      result$date.full <- as.Date(result$date.full)
      result<- filter(result, year(result$date.full) == input$year.sel)
      
      if(length(levels(factor(result$Transport))) == 3){
        colour <- result$Transport
      } else{
        colour <- "Orange"
      }
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, colour = colour, title = "Smooth time series plot", method = geom_smooth)
      
      p
    } else {
      result <- sel()[[2]]
      result$date.full <- as.Date(result$date.full )
      result<- filter(result, result$date.full > input$month.selection[1] & result$date.full < input$month.selection[2])
      
      if(length(levels(factor(result$Transport))) == 3){
        colour <- result$Transport
      } else{
        colour <- "Orange"
      }
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, colour = colour, title = "Smooth time series plot")
      
      p
    }
    
  })
  
  output$daily <- renderPlot({
    
    if(sel.month() == 0){
      result <- sel()[[2]]
      result$date.full <- as.Date(result$date.full)
      result<- filter(result, year(result$date.full) == input$year.sel)
      
      if(length(levels(factor(result$Transport))) == 3){
        colour <- result$Transport
      } else{
        colour <- "Orange"
      }
      
      
      p <- plot.time.series(result,
                            result$date.full, result$number.of.cases, colour = colour, title = "Daily time series plot")
      
      p
      
    }
  })
  
  output$map <- renderLeaflet({
    
    pal_disposal <- colorFactor("Set1", domain = Map.Kanyama$Transport)
    
    Map.Kanyama$date.full <- as.Date(Map.Kanyama$date.full)
    
    Map.Kanyama <- Map.Kanyama %>% filter( year(date.full) == input$map.year.sel) 
    
    leaflet(data = Map.Kanyama) %>% addProviderTiles( providers$Esri.WorldTopoMap) %>% 
      addCircleMarkers(lng = ~X1.9..longitude.,
                       lat = ~X1.9..latitude.,
                       color = ~pal_disposal(Map.Kanyama$Transport),
                       radius = 2, fillOpacity = 0.3, stroke = T) %>% 
      addLegend("bottomright", 
                pal = pal_disposal, 
                values = ~Map.Kanyama$Transport,
                title = "Type of Transport",
                na.label = "Not Available")
    
    
  })
  


}

# Running ----
shinyApp(ui  = ui, server = server, options = list(height = 1080))
 