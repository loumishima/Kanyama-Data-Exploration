#Loading the libraries and dataset ----
library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)
library(plotly)
library(viridis)

#Loading the reduced Dataset ----
setwd("/Users/gather3/Documents/Github")
Kanyama.plot <- read.csv("Kanyama_to_plot.csv")

Kanyama.valid <- Kanyama.plot %>% filter(!is.na(X1.9..latitude.) & !is.na(X1.9..longitude.)) %>% filter(X1.9..latitude. > -20)

#Map visualization (Initial step) ----
pal <- colorFactor("Accent", domain = Kanyama.valid$Interface.Layout, na.color = "#000000"  )


plot <- leaflet(data = Kanyama.valid) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal(Kanyama.valid$Interface.Layout), label = ~Water.source..fetch.,
                   radius = 10, fillOpacity = 0.5, stroke = F) %>% addLegend("bottomright", 
                                              pal = pal, 
                                              values = ~Interface.Layout,
                                              title = "Type of toilet", na.label = "Not Available")
plot


#ggplot Visualization ----

type.of.toilet <- Kanyama.plot %>% group_by(Interface.Layout) %>% summarise(percentage = round(n()/nrow(Kanyama.plot) * 100, 1), count = n())

#Percentage of toilets ----
plot_percentage <- ggplot(data = type.of.toilet, aes(x = Interface.Layout, y = percentage, fill = Interface.Layout)) + 
  geom_bar(stat = 'identity') +
  theme_light() +
  geom_text(aes(label=percentage), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  labs(title = "Percentage of toilets' type", subtitle = "Kanyama", fill = "Toilet type", y = "Percentage")

#Count of toilets
plot_count <- ggplot(data = Kanyama.plot, aes(x = Interface.Layout, fill = Interface.Layout))  + 
  geom_histogram(stat = 'count') +
  theme_light() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  labs(title = "Quantity of toilets in Kanyama", subtitle = paste("Based on", nrow(Kanyama.plot), "plots"), fill = "Toilet type", y = "Number of toilets")

plot_count

#Type of water used ----
type.of.water <- Kanyama.plot %>% group_by(Water.source..fetch.) %>% summarise(count = n())

plot_water <- plot_ly(type.of.water, labels=~Water.source..fetch., values=~count, type = "pie") %>% 
  layout(title = "Water Source in Kanyama",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

plot_water


#Collect zones ----


#Vacuum Tanker
pal_bool <- colorFactor(c("red", "green"), domain = Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following...Vacuum.Tanker, na.color = "#000000")

plot_Vacuum <- leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal_bool(Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following...Vacuum.Tanker), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                             pal = pal_bool, 
                                                                             values = ~Is.the.toilet.easily.accessible.to.the.following...Vacuum.Tanker,
                                                                             title = "Avaiable for the Vacuum Tanker", na.label = "Not Available")
plot_Vacuum  

#Light Truck

plot_Truck <-leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal_bool(Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following...Light.Truck), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                            pal = pal_bool, 
                                                                            values = ~Is.the.toilet.easily.accessible.to.the.following...Light.Truck,
                                                                            title = "Avaiable for the Light Truck", na.label = "Not Available")
plot_Truck


#Push Cart

plot_Cart <-leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal_bool(Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following...Push.Cart), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                            pal = pal_bool, 
                                                                            values = ~Is.the.toilet.easily.accessible.to.the.following...Push.Cart,
                                                                            title = "Avaiable for the Push Cart", na.label = "Not Available")
plot_Cart




#Safety for people ----

#Children
plot_Children <-leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal_bool(Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following.people...Children), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                            pal = pal_bool, 
                                                                            values = ~Is.the.toilet.easily.accessible.to.the.following.people...Children,
                                                                            title = "Avaiable for Children", na.label = "Not Available")
plot_Children

#Women at Night

plot_Women <-leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal_bool(Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following.people...Women.at.night), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                            pal = pal_bool, 
                                                                            values = ~Is.the.toilet.easily.accessible.to.the.following.people...Women.at.night,
                                                                            title = "Avaiable for women at night", na.label = "Not Available")
plot_Women

#People with dissability

plot_dis <-leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~pal_bool(Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following.people...Persons.with.dissability), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                            pal = pal_bool, 
                                                                            values = ~Kanyama.valid$Is.the.toilet.easily.accessible.to.the.following.people...Persons.with.dissability,
                                                                            title = "Avaiable for peopley with dissability", na.label = "Not Available")
plot_dis



#People per plot (Average) ----
filter_outside <- Kanyama.valid %>% filter(X1.9..longitude. < 28.275)

filter_outside$fact_people_per_plot <- cut(filter_outside$People_on_the_plot, breaks = c(0,5,10, 20, 50, 50000),
                                      labels = c("Up to 5 people","Up to 10 people", "Up to 20 people", "Up to 50 people", "More than 50 people" ) , include.lowest = T)

people_per_plot <- filter_outside %>% filter(!is.na(People_on_the_plot))

people_pal <- colorFactor("Spectral", domain = people_per_plot$fact_people_per_plot, na.color = "#000000")

plot_avg_people <- leaflet(data = people_per_plot) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~people_pal(people_per_plot$fact_people_per_plot), label = ~Water.source..fetch.,
                   radius = 5, opacity = 0.5, stroke = T) %>% addLegend("bottomright", 
                                                                        pal = people_pal, 
                                                                        values = ~fact_people_per_plot,
                                                                        title = "Toilet per people", na.label = "Not Available")


plot_avg_people



#People per toilet / Toilet per people ----
filter_outside <- filter_outside %>% mutate(people_per_toilet= case_when(Total.number.of.toilets.0. != 0 ~ people.using.toilet/Total.number.of.toilets.0.,
                                                                         TRUE ~ 0))
                                            
filter_outside <- filter_outside %>% mutate(toilet_per_people= case_when(people.using.toilet != 0 ~ Total.number.of.toilets.0./people.using.toilet,
                                                                         TRUE ~ 0))                                            

filter_outside$fact_ppl_toilet <- cut(filter_outside$people_per_toilet, breaks = c(0,1,5, 10, 20, 50, 100,5000),
                                      labels = c("1 person","Up to 5 people", "Up to 10 people", "Up to 20 people", "Up to 50 people","Up to 100 people", "More than 100 people" ) ,include.lowest = T)
filter_outside$fact_toilet_ppl <- cut(filter_outside$toilet_per_people, breaks = c(0,1, 2, 3, 50),
                                      labels = c("Up to 1 toilets","Up to 2 toilets", "Up to 3 toilets", "More than 3 toilets" ) , include.lowest = T)


cont_pal <- colorFactor("Set1", domain = filter_outside$fact_toilet_ppl, na.color = "#000000" )
cont_pal2 <- colorFactor("Set1", domain = filter_outside$fact_ppl_toilet, na.color = "#000000" )

plot_toilet_ppl <- leaflet(data = filter_outside) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~cont_pal(filter_outside$fact_toilet_ppl), label = ~Water.source..fetch.,
                   radius = 5, opacity = 0.5, stroke = T) %>% addLegend("bottomright", 
                                                                             pal = cont_pal, 
                                                                             values = ~fact_toilet_ppl,
                                                                             title = "Toilet per people", na.label = "Not Available")


plot_ppl_toilet <- leaflet(data = filter_outside) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude., 
                   color = ~cont_pal2(filter_outside$fact_ppl_toilet), label = ~Water.source..fetch.,
                   radius = 5, opacity = 0.5, stroke = T) %>% addLegend("bottomright", 
                                                                        pal = cont_pal2, 
                                                                        values = ~fact_ppl_toilet,
                                                                        title = "People per toilet", na.label = "Not Available")
plot_ppl_toilet
plot_toilet_ppl

#Bin_hex people per toilet
ggplot(data = filter_outside ,aes(x=X1.9..longitude., y =X1.9..latitude., fill = fact_ppl_toilet  )) + geom_hex(bins = 30) + theme_void()



#Solid Waste Disposal ----

Waste.type <- Kanyama.plot %>% group_by(Where.do.you.dispose.your.solid.wastes.) %>% summarise(count = n())

plot_waste <- plot_ly(Waste.type, labels = ~Where.do.you.dispose.your.solid.wastes., values =~count, type = "pie",
                      insidetextfont = list(color = "#FFFFFF")) %>% 
  layout(title = "Solid Waste Disposal in Kanyama",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

plot_waste

#Landlord live on the plot? ----
Landlord <- Kanyama.plot %>% filter(!is.na(Landlord.live.in.the.plot.)) %>%  group_by(Landlord.live.in.the.plot.) %>% summarise(count = n())

ggplot(data = Landlord, aes(x = Landlord.live.in.the.plot., y = count, fill = Landlord.live.in.the.plot.)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = count), vjust = -0.3) +
  theme_bw() +
  labs(title = "Landlord live in the plot?", subtitle = " Plots in Kanyama", x = element_blank(), y = element_blank(), fill = "Landlord live in the plot?") +
  theme(panel.grid = element_blank()) +
  coord_cartesian( ylim = c(0,10000))


#Landlord living x Quality of the toilet ----

plot_qualityxlandlord <- ggplot(data = Kanyama.plot,aes(x = Perception.of.the.fill.level, fill = Landlord.live.in.the.plot.) )+
  geom_histogram() + theme_light() +
  labs(x = "Perception of the fill level", y = "Number of toilets", fill = "Does the landlord live in the plot?")

plot_qualityxlandlord

plot_disposalxlandlord <- ggplot(data = filter(Kanyama.plot,!is.na(What.happens.when.the.toilet.gets.full.) & !is.na(Landlord.live.in.the.plot.) ), aes(x = What.happens.when.the.toilet.gets.full., fill = Landlord.live.in.the.plot.)) +
  geom_histogram(stat = "count") + theme_light()+
  labs(x = element_blank(), y = "Number of toilets", fill = "Does the landlord live in the plot?", title = "Full toilet Vs Landlord lives in the place") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

plot_disposalxlandlord

plot_typexlandlord <- ggplot(data = Kanyama.plot, aes(x = Interface.Layout, fill = Landlord.live.in.the.plot.)) +
  geom_histogram(stat = "count") + coord_flip()

plot_typexlandlord


time.emptyxlandlord <- Kanyama.plot %>% filter (!is.na(Landlord.live.in.the.plot.) & !is.na(Toilet.emptying.time)) %>% 
  group_by(Landlord.live.in.the.plot.) %>% summarise(median.time = mean(as.numeric(as.character(Toilet.emptying.time)), na.rm = T))

plot_time.emptyxlandlord <- ggplot(data = time.emptyxlandlord, aes(x = Landlord.live.in.the.plot., y = median.time, fill = Landlord.live.in.the.plot.)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = median.time), vjust = -.5) +
  scale_y_continuous(limits = c(0,5)) +
  theme_light()

plot_time.emptyxlandlord


#Toilet reuse ----


pal_disposal <- colorFactor("Set1", domain = Kanyama.valid$What.happens.when.the.toilet.gets.full.)

plot_disposal <- leaflet(data = Kanyama.valid) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = ~X1.9..longitude.,
                   lat = ~X1.9..latitude.,
                   color = ~pal_disposal(Kanyama.valid$What.happens.when.the.toilet.gets.full.), label = ~Water.source..fetch.,
                   radius = 1, fillOpacity = 0.3, stroke = T) %>% addLegend("bottomright", 
                                                                            pal = pal_disposal, 
                                                                            values = ~Kanyama.valid$What.happens.when.the.toilet.gets.full.,
                                                                            title = "What happens when the toilet gets full?", na.label = "Not Available")
plot_disposal
