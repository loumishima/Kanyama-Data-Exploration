library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

server <- function(input, output) {
  
  output$result.plot <- renderPlot({
    
    
    plot.time.series <- function(df, x.axis, y.axis, colour = "blue", colour.legend = "Legend: "){
      
      # Add a for loop to use over the years
      # for(i in 1:length(unique(year(Kanyama$date.full)))){ print(unique(year(Kanyama$date.full))[i])}
      
      if(length(colour) == 1){
        
        plot <- ggplot(data = df, aes(x = x.axis)) + 
          geom_line(aes(y = y.axis ), colour = colour) +
          labs(title="Yearly Time Series", 
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
          geom_line(aes(y = y.axis , colour = colour)) +
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
    
    setwd("/Users/gather3/Documents/Kanyama - Data Exploration")
    Kanyama <- read.csv("Kanyama_organized.csv", stringsAsFactors = F)
    
  })
  
}