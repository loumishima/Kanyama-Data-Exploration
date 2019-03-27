library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)

# Loading the organized Dataset ---
setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Data")
Kanyama <- read.csv("Kanyama_organized.csv", stringsAsFactors = F)

# This document has the objective to create some datasets focused on differents areas
# such as: geolocalization, people, toilets, etc.
# If necessary, the datasets can be merged based on their infos

# Function responsible for creating ID's for each toilet ----

create.ID <- function( df) {
  
  df <- df %>% mutate(ID = as.character(row.names(df))) %>% select(ID, everything())
  return(df)
  
}

# After the ID field creation, it is easy to combine multiple datasets just joining by the ID

Kanyama <- create.ID(Kanyama)

# Selecting the columns related to geolocalization to create de dataset 'Zone.Info.Kanyama' ----

Zone.Info.Kanyama <- Kanyama %>% select(ID,
                                        DATE.OF.INTERVIEW,
                                        RECORD.TYPE.OF.PROPERTY,
                                        SELECT.ZONE,
                                        SELECT.ZONE.SECTION,
                                        "Latitude" = X1.9..latitude.,
                                        "Longitude" = X1.9..longitude.,
                                        "Altitude" = X1.9..altitude.,
                                        "Administrative.Region" = X1.9..administrative.region.,
                                        "Accuracy" = X1.9..accuracy.,
                                        "Time.Answered" = X1.9..Time.Answered.
                                        )

# Selecting the columns related to public / people info ----

People.Info.Kanyama <- Kanyama %>% select(ID,
                                          DATE.OF.INTERVIEW,
                                          Families_on_the_plot,
                                          adults.using.toilet,
                                          children.using.toilet,
                                          people.using.toilet,
                                          People_on_the_plot,
                                          Landlord.live.in.the.plot.,
                                          starts_with("How.many.people"),
                                          )

# Selecting the columns related to Toilet physical characteristics ----


Toilet.Info.Kanyama <- Kanyama %>% select(ID,
                                          DATE.OF.INTERVIEW,
                                          VIP.toilets,
                                          ECOSAN.toilets,
                                          Inside.waterflush.toilets,
                                          Outside.waterflush.toilets,
                                          Poor.flush.Inside,
                                          Poor.flush.Outside,
                                          Pit.Latrine,
                                          Disused.Buried,
                                          Total.number.of.toilets.0.,
                                          Enough.space.another.toilet,
                                          Where.do.you.dispose.your.solid.wastes.,
                                          Age.of.toilet..Years...Age,
                                          Interface.Layout,
                                          CONTAINMENT.SUBSTRUCTURE,
                                          contains("Condition"),
                                          Record.the.observed.shape.of.the.substructure.containment.,
                                          Diameter,
                                          Length,
                                          Is.washing.hand.basin.present.,
                                          Any.overflow.flooding.
                                          )


# Selecting columns related to the toilet emptying process and its characteristics ----

Toilet.emptying.Info.Kanyama <- Kanyama %>% select(ID,
                                                   DATE.OF.INTERVIEW,
                                                   Upgraded.toilet.recently.,
                                                   Emptied.the.toilet.before.,
                                                   What.happens.when.the.toilet.gets.full.,
                                                   Last.time.emptied,
                                                   Was.the.fee.you.paid.affordable.,
                                                   How.often.do.you.empty.your.toilet.,
                                                   "Years to full" = When.next.do.you.think.your.toilet.will.be.due.for.emptying...Years.from.now...Period,
                                                   "Months to full" = When.next.do.you.think.your.toilet.will.be.due.for.emptying...Months.from.now...Period,
                                                   Perception.of.the.fill.level,
                                                   Is.emptying.feasible.
                                                   )


# Selecting columns related to People and Cleaning devices' accessibility ----

Accessibility.Info.Kanyama <- Kanyama %>% select(ID,
                                                 starts_with("Is.the.toilet"))


# Saving each dataset in the reduced_datasets folder ----

write.csv(Zone.Info.Kanyama, "reduced_datasets/Geo_Kanyama.csv", row.names = F)
write.csv(People.Info.Kanyama, "reduced_datasets/People_Kanyama.csv", row.names = F)
write.csv(Toilet.Info.Kanyama, "reduced_datasets/Toilet_Kanyama.csv", row.names = F)
write.csv(Toilet.emptying.Info.Kanyama, "reduced_datasets/Toilet_emptying_Kanyama.csv", row.names = F)
write.csv(Accessibility.Info.Kanyama, "reduced_datasets/Access_Kanyama.csv", row.names = F)



# To reunite just use the join function, combining any datasets you want

People.and.Toilet <- inner_join(People.Info.Kanyama, Toilet.Info.Kanyama, by= 'ID')
People.and.Toilet <- inner_join(People.and.Toilet, Toilet.emptying.Info.Kanyama, by = "ID")




