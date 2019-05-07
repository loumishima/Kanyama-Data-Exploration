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


source("data_processing.R")

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

