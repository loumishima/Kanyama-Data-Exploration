library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(plotly)

#shinyapps.io

plot.time.series <- function(df, x.axis, y.axis, colour = "blue", colour.legend = "Legend: ", title = "Plot", method = geom_line){
  
  if(length(colour) == 1){
    
    plot <- ggplot(data = df, aes(x = x.axis)) + 
      method(aes(y = y.axis ), colour = colour, se = F, method = "loess") +
      labs(title= title, 
           subtitle="Need of waste management team", 
           y="Number of Occurrences",
           colour = colour.legend) + 
      theme_bw()+
      scale_x_date(labels = date_format(format = "%b %Y"),breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom")
    
  } else {
    
    plot <- ggplot(data = df, aes(x = x.axis)) + 
      method(aes(y = y.axis , colour = colour), se = F, method = "loess") +
      labs(title="2019 Time Series", 
           subtitle="Need of waste management team", 
           y="Number of Occurrences",
           colour = colour.legend) + 
      theme_bw()+
      scale_x_date(labels = date_format(format = "%b %Y"),breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom") 
  }
  # instead of returning the plot, save images in a folder
  # ggsave
  return(plot)
  
}

# Read the data
setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Shiny")
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
                                          sep = "")
                            ),
                            
                            mainPanel(
                              
                              tabsetPanel(type = "tabs",
                                          tabPanel("General",   plotlyOutput(outputId = "soft.plot", height = "400px"),
                                                   plotlyOutput(outputId = "time.plot") ),
                                          tabPanel("Transports", 
                                                   plotlyOutput(outputId = "monthly"),
                                                   plotlyOutput(outputId = "daily")
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
                 
                 
                 
                 
                 
                 tabPanel("Misc")
                 
                 
                          )

# Server ----
server <- function(input, output) {
  
  
  # RadioButtons' Variable
  
  sel <- reactive({
    selection <- switch (input$plot.selection,
                         all = list(type.transport.monthly, type.transport),
                         vt = list(vt.monthly, vt.daily),
                         lt = list(lt.monthly, lt.daily),
                         pc = list(pc.monthly, pc.daily)
    )
  })
  
  # First Render Plot
  output$time.plot <- renderPlotly({
    
    
    result<- filter(Daily.plot, year(Daily.plot$date.full) == input$year.sel)
    
    p <- plot.time.series(result,
                          result$date.full, result$number.of.cases, title = "Daily time series plot")
    
    ggplotly(p)
    
  })
  
  
  # Second Render Plot
  output$soft.plot <- renderPlotly({
    
    result<- filter(Monthly.plot, year(Monthly.plot$date.full) == input$year.sel)
    
    p <- plot.time.series(result,
                          result$date.full, result$number.of.cases, title = "Smooth time series plot", method = geom_smooth)
    
    ggplotly(p)
    
  })
  
  output$monthly <- renderPlotly({
    
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
    
    ggplotly(p)
    
  })
  
  output$daily <- renderPlotly({
    
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
    
    ggplotly(p)
    
    
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
