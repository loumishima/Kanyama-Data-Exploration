library(shiny)
library(dplyr)
library(lubridate)
library(scales)

#shinyapps.io

source("functions.R")
source("ui_modules.R")
source("server_modules.R")
# Read the data
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

Map.Kanyama <- Kanyama %>% dplyr::select(X1.9..latitude., X1.9..longitude.,X1.9..altitude., date.full, Transport, area.m3) %>% 
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

