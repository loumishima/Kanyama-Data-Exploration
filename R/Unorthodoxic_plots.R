library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)

# Loading the organized Dataset ---
setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Data")
Kanyama <- read.csv("Kanyama_organized.csv", stringsAsFactors = F)


# This file is responsible for making the new plots (Unorthodoxic ones)

# Creating a generic function ----

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
# Objective of this plot: Time for waste collection  ----

# Vacuum tanker: Whenever avaialble
# Light truck: When Vaccum tanker is unavailable
# Push cart: When none of the above is available

P.Availability <- Kanyama %>% select(DATE.OF.INTERVIEW,
                                     X1.9..latitude.,
                                     X1.9..longitude.,
                                     area.m3, 
                                     Perception.of.the.fill.level, 
                                     people.using.toilet, 
                                     days.to.fill, 
                                     date.full,
                                     Is.the.toilet.easily.accessible.to.the.following...Vacuum.Tanker,
                                     Is.the.toilet.easily.accessible.to.the.following...Light.Truck,
                                     Is.the.toilet.easily.accessible.to.the.following...Push.Cart
                                     )

P.Availability <- P.Availability %>% 
  filter(!is.na(Is.the.toilet.easily.accessible.to.the.following...Vacuum.Tanker) &
           !is.na(Is.the.toilet.easily.accessible.to.the.following...Light.Truck) &
           !is.na(Is.the.toilet.easily.accessible.to.the.following...Push.Cart)) %>% 
  mutate( Transport = case_when( Is.the.toilet.easily.accessible.to.the.following...Vacuum.Tanker == T ~ "Vacuum Tanker",
                                 Is.the.toilet.easily.accessible.to.the.following...Light.Truck == T ~ "Light Truck",
                                 TRUE ~ "Push Cart") ) %>%
  select(-contains("Is.the.toilet"))

write.csv(P.Availability, file = "Shiny/Shiny_plot.csv")

# Plot ( All cases combined ) ----

grouped.P.Availability <- P.Availability %>% 
  group_by(date.full) %>% 
  summarise(number.of.cases = n()) %>% 
  filter(!is.na(date.full)) %>% ungroup()

grouped.P.Availability$date.full <- as.Date(grouped.P.Availability$date.full)

grouped.P.Availability.2019<- filter(grouped.P.Availability, year(grouped.P.Availability$date.full) == 2019)

grouped.P.Availability.2020<- filter(grouped.P.Availability, year(grouped.P.Availability$date.full) == 2020)

p.1.2019 <- plot.time.series(grouped.P.Availability.2019,
                        grouped.P.Availability.2019$date.full, grouped.P.Availability.2019$number.of.cases)

p.1.2020 <- plot.time.series(grouped.P.Availability.2020,
                             grouped.P.Availability.2020$date.full, grouped.P.Availability.2020$number.of.cases)

p.1.2019
p.1.2020

# Now creating multiple lines for each kind of Transport ----

type.transport <- P.Availability %>% 
  group_by(date.full, Transport) %>% 
  summarise(number.of.cases = n()) %>% 
  filter(!is.na(date.full)) %>% 
  ungroup()

type.transport$date.full <- as.Date(type.transport$date.full)

type.transport.2019<- filter(type.transport, year(type.transport$date.full) == 2019)

type.transport.2020<- filter(type.transport, year(type.transport$date.full) == 2020)

p.2.2019 <- plot.time.series(type.transport.2019, 
                        type.transport.2019$date.full, type.transport.2019$number.of.cases,
                        type.transport.2019$Transport, colour.legend = "Transport: ")

p.2.2020 <- plot.time.series(type.transport.2020, 
                             type.transport.2020$date.full, type.transport.2020$number.of.cases,
                             type.transport.2020$Transport, colour.legend = "Transport: ")

p.2.2019
p.2.2020

# Now making each transport type separately ----


Vaccum.Tanker.2019 <- type.transport.2019 %>% filter(Transport == "Vacuum Tanker")
Light.Truck.2019 <- type.transport.2019 %>% filter(Transport == "Light Truck")
Push.Cart.2019 <- type.transport.2019 %>% filter(Transport == "Push Cart")

Vaccum.Tanker.2020 <- type.transport.2020 %>% filter(Transport == "Vacuum Tanker")
Light.Truck.2020 <- type.transport.2020 %>% filter(Transport == "Light Truck")
Push.Cart.2020 <- type.transport.2020 %>% filter(Transport == "Push Cart")

# Plot for each case 

# 2019

# Vacuum Tanker

p.vaccum.2019 <- plot.time.series(Vaccum.Tanker.2019,
                             Vaccum.Tanker.2019$date.full, Vaccum.Tanker.2019$number.of.cases,
                             "red4")
p.vaccum.2019

# Light Truck

p.light.2019 <- plot.time.series(Light.Truck.2019,
                            Light.Truck.2019$date.full, Light.Truck.2019$number.of.cases,
                            "purple1")
p.light.2019

# Push Cart

p.cart.2019 <- plot.time.series(Push.Cart.2019,
                           Push.Cart.2019$date.full, Push.Cart.2019$number.of.cases,
                           "limegreen")
p.cart.2019


# 2020

# Vacuum Tanker

p.vaccum.2020 <- plot.time.series(Vaccum.Tanker.2020,
                                  Vaccum.Tanker.2020$date.full, Vaccum.Tanker.2020$number.of.cases,
                                  "red4")
p.vaccum.2020

# Light Truck

p.light.2020 <- plot.time.series(Light.Truck.2020,
                                 Light.Truck.2020$date.full, Light.Truck.2020$number.of.cases,
                                 "purple1")
p.light.2020

# Push Cart

p.cart.2020 <- plot.time.series(Push.Cart.2020,
                                Push.Cart.2020$date.full, Push.Cart.2020$number.of.cases,
                                "limegreen")
p.cart.2020


# smoothing the curve line ----

# Changing the date format
P.Availability$date.full <- format(as.Date(P.Availability$date.full), "%Y-%m-01")

G.Kanyama <- P.Availability %>% 
  group_by(date.full) %>% 
  summarise(count.of.occurrences = n()) %>% 
  ungroup()

G.Kanyama$date.full <- as.Date(G.Kanyama$date.full)

G.Kanyama <- filter(G.Kanyama, year(date.full) == 2019)

plot.smooth <- plot.time.series(G.Kanyama, 
                                G.Kanyama$date.full, G.Kanyama$count.of.occurrences,
                                colour = "red4")
plot.smooth

type.transport <- P.Availability %>% 
  group_by(date.full, Transport) %>% 
  summarise(number.of.cases = n()) %>% 
  filter(!is.na(date.full)) %>% 
  ungroup()

type.transport$date.full <- as.Date(type.transport$date.full)

type.transport.2019<- filter(type.transport, year(type.transport$date.full) == 2019)

type.transport.2020<- filter(type.transport, year(type.transport$date.full) == 2020)

p.2.2019 <- plot.time.series(type.transport.2019, 
                             type.transport.2019$date.full, type.transport.2019$number.of.cases,
                             type.transport.2019$Transport, colour.legend = "Transport: ")

p.2.2020 <- plot.time.series(type.transport.2020, 
                             type.transport.2020$date.full, type.transport.2020$number.of.cases,
                             type.transport.2020$Transport, colour.legend = "Transport: ")

p.2.2020
p.2.2019


Vaccum.Tanker.2019 <- type.transport.2019 %>% filter(Transport == "Vacuum Tanker")
Light.Truck.2019 <- type.transport.2019 %>% filter(Transport == "Light Truck")
Push.Cart.2019 <- type.transport.2019 %>% filter(Transport == "Push Cart")

Vaccum.Tanker.2020 <- type.transport.2020 %>% filter(Transport == "Vacuum Tanker")
Light.Truck.2020 <- type.transport.2020 %>% filter(Transport == "Light Truck")
Push.Cart.2020 <- type.transport.2020 %>% filter(Transport == "Push Cart")

# Plot for each case 

# 2019

# Vacuum Tanker

p.vaccum.2019 <- plot.time.series(Vaccum.Tanker.2019,
                                  Vaccum.Tanker.2019$date.full, Vaccum.Tanker.2019$number.of.cases,
                                  "red4")
p.vaccum.2019

# Light Truck

p.light.2019 <- plot.time.series(Light.Truck.2019,
                                 Light.Truck.2019$date.full, Light.Truck.2019$number.of.cases,
                                 "purple1")
p.light.2019

# Push Cart

p.cart.2019 <- plot.time.series(Push.Cart.2019,
                                Push.Cart.2019$date.full, Push.Cart.2019$number.of.cases,
                                "limegreen")
p.cart.2019


# 2020

# Vacuum Tanker

p.vaccum.2020 <- plot.time.series(Vaccum.Tanker.2020,
                                  Vaccum.Tanker.2020$date.full, Vaccum.Tanker.2020$number.of.cases,
                                  "red4")
p.vaccum.2020

# Light Truck

p.light.2020 <- plot.time.series(Light.Truck.2020,
                                 Light.Truck.2020$date.full, Light.Truck.2020$number.of.cases,
                                 "purple1")
p.light.2020

# Push Cart

p.cart.2020 <- plot.time.series(Push.Cart.2020,
                                Push.Cart.2020$date.full, Push.Cart.2020$number.of.cases,
                                "limegreen")
p.cart.2020

# In the future maybe we can add the lines of real waste management team
# comparing the predict with the real data


# Another plots ----

#ok

leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap)
