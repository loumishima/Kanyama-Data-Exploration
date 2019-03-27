#Loading the libraries and dataset ----
library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)

#Loading the reduced Dataset ---
setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Data")
Kanyama <- read.csv("Kanyama_reduced.csv")

#Function to remove by percentage of NA's

Columns.Remover <- function(ds, percentage){
  
  
  selection <- apply(ds, 2, function(x)  (sum( !is.na(x) ) / nrow(ds)) > percentage )
  
  return(selection)
  
}

# Bathroom fill time preview based on:
# Average human fecal waste per day = 0.000128 g
# density similar to water

Fill.time <- function(area, number.people, fill.level, flow.rate = 1.28e-4){
  
  fill.level <- case_when(fill.level == "Empty" ~ 0.0,
                          fill.level == "Almost empty" ~ 0.25,
                          fill.level == "Half-full" ~ 0.5,
                          fill.level == "Almost full" ~ 0.75,
                          fill.level == "Full" ~ 0.9,
                          TRUE ~ 0.5)
  
  days.to.fill <- ((1  - fill.level) * area) / (number.people * flow.rate)
  return( days.to.fill )
  
}


Fill.date <- function(area, number.people, fill.level, interview.date){
  
  days.to.fill <- Fill.time(area, number.people, fill.level)
  
  return(days.to.fill + as.Date(interview.date))
}



Boolean.50 <- Columns.Remover(Kanyama, 0.5)
Kanyama.50perc <- Kanyama[Boolean.50]

Boolean.75 <- Columns.Remover(Kanyama, 0.75)
Kanyama.75perc <- Kanyama[Boolean.75]

Boolean.80 <- Columns.Remover(Kanyama, 0.80)
Kanyama.80perc <- Kanyama[Boolean.80]

Boolean.90<- Columns.Remover(Kanyama, 0.90)
Kanyama.90perc <- Kanyama[Boolean.90]


Kanyama <- Kanyama %>% select(-c(Are.you.willing.to.participate.,
                                  Record_plot_number,
                                  Where.do.you.dispose.your.solid.wastes...Other..please.specify.....specify,
                                  How.much.did.you.pay.for.the.upgrades.in.ZMW.,
                                  How.did.you.finance.for.the.upgrades.,
                                  How.did.you.finance.for.the.upgrades...Other..please.specify.....specify,
                                  X3.7.2..Other..please.specify.....specify,
                                  Condition.of.the.components..Roof...Score.Condition,
                                  Condition.of.the.components..Wall...Score.Condition,
                                  Condition.of.the.components..Floor...Score.Condition,
                                  DATE.OF.INTERVIEW..Time.Answered.
                                  ))


Kanyama <- Kanyama %>% mutate(Enough.space.another.toilet = 
                                case_when(is.na(X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.) ~ FALSE,
                                          grepl("No", X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.) == TRUE ~ FALSE,
                                          grepl("\\d0", X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.) == TRUE ~ TRUE,
                                          grepl("0", X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.) == TRUE ~ FALSE,
                                          grepl("None", X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.) == TRUE ~ FALSE,
                                          grepl("Zero", X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.) == TRUE ~ FALSE,
                                          TRUE ~ TRUE))
                              
Kanyama <- Kanyama %>% mutate(What.happens.when.the.toilet.gets.full. =
                                case_when(What.happens.when.the.toilet.gets.full. == "1" ~ "Bury and dig another one",
                                          What.happens.when.the.toilet.gets.full. == "2" ~ "Empty and reuse",
                                          What.happens.when.the.toilet.gets.full. == "3" ~ "Abandone",
                                          What.happens.when.the.toilet.gets.full. == "Other (please specify)" ~ "Other (please specify)",
                                          TRUE ~ "Multiple options"))

Kanyama <- Kanyama %>% mutate(Perception.of.the.fill.level =
                                case_when(Perception.of.the.fill.level == "1" ~ "Full",
                                          Perception.of.the.fill.level == "2" ~ "Almost full",
                                          Perception.of.the.fill.level == "3" ~ "Half-full",
                                          Perception.of.the.fill.level == "4" ~ "Almost empty",
                                          Perception.of.the.fill.level == "5" ~ "Empty",
                                          TRUE ~ NA_character_))

# Adding the substructure area:

# Pit latrine = 4.5 m^3
# Septic tank = 12.5 m^3

Kanyama <- Kanyama %>% mutate(area.m3 = case_when(CONTAINMENT.SUBSTRUCTURE == "Pit latrine" ~ 4.5,
                                                  CONTAINMENT.SUBSTRUCTURE == "Septic Tank" ~ 12.5,
                                                  TRUE ~ 0.0))


Kanyama <- Kanyama %>% mutate(Toilet.emptying.time = paste(When.next.do.you.think.your.toilet.will.be.due.for.emptying...Years.from.now...Period,
                                                           When.next.do.you.think.your.toilet.will.be.due.for.emptying...Months.from.now...Period,
                                                           sep = "."),
                              Toilet.emptying.time = gsub("NA", "0", Toilet.emptying.time))


Kanyama <- Kanyama %>% mutate(Interface.Layout =
                                case_when(Interface.Layout == "1" ~ "Sit down toilet with manual (hand) flushing system",
                                          Interface.Layout == "2" ~ "Sit down toilet with pour (bucket) flushing system",
                                          Interface.Layout == "3" ~ "Squat with pour (bucket) flushing system",
                                          Interface.Layout == "4" ~ "Squat hole (Dry toilet)",
                                          Interface.Layout == "5" ~ "Urine Diversion Toilet",
                                          Interface.Layout == "6" ~ "Other",
                                          is.na(Interface.Layout) ~ NA_character_,
                                          TRUE ~ "Multiple choice"))


Kanyama <- Kanyama %>% mutate(adults.using.toilet = rowSums(select(Kanyama, How.many.people.use.the.toilets.on.this.plot...Adults...Female, How.many.people.use.the.toilets.on.this.plot...Adults...Male), na.rm = T),
                              children.using.toilet = rowSums(select(Kanyama, How.many.people.use.the.toilets.on.this.plot...Children...Male, How.many.people.use.the.toilets.on.this.plot...Children...Female), na.rm = T),
                              people.using.toilet = rowSums(select(Kanyama, How.many.people.use.the.toilets.on.this.plot...Children...Male, How.many.people.use.the.toilets.on.this.plot...Children...Female, How.many.people.use.the.toilets.on.this.plot...Adults...Male, How.many.people.use.the.toilets.on.this.plot...Adults...Female), na.rm = T))

Kanyama <- Kanyama %>% mutate(Total.number.of.toilets.0. = rowSums(select(Kanyama, VIP.toilets: Disused.Buried), na.rm = T),
                              Pit.Latrine = rowSums(select(Kanyama, Unlined.Pit.latrine, Lined.Pit.latrine), na.rm = T))


Kanyama <- Kanyama %>% select(-c(X1.6.1.Do.you.think.there.is.space.on.this.plot.to.construct.another.toilet...Yes...If.Yes.how.many.more..If.No..why.is.that.the.case.,
                                 Explain.reason.for.not.being.able.to.take.the.reading.of.the.height.,
                                 contains("X1.9..Location.Ans")))

Kanyama <- Kanyama %>% mutate(Where.do.you.dispose.your.solid.wastes. = case_when(grepl(",",Where.do.you.dispose.your.solid.wastes.) == TRUE ~ "Multiple Choices",
                                                                                  Where.do.you.dispose.your.solid.wastes. == "null" ~ NA_character_,
                                                                                  TRUE ~ as.character(Where.do.you.dispose.your.solid.wastes.)))

Kanyama <- Kanyama %>% mutate(days.to.fill = Fill.time(area = area.m3, number.people = people.using.toilet, fill.level = Perception.of.the.fill.level),
                              date.full = Fill.date(area = area.m3, number.people = people.using.toilet, fill.level = Perception.of.the.fill.level, interview.date = DATE.OF.INTERVIEW))


#Making a new Small Dataset ----

Kanyama_essential <- Kanyama %>% select(c(
  DATE.OF.INTERVIEW,
  RECORD.TYPE.OF.PROPERTY,
  Region,
  People_on_the_plot,
  Landlord.live.in.the.plot.,
  c(VIP.toilets: Poor.flush.Outside),
  Pit.Latrine,
  Disused.Buried,
  Total.number.of.toilets.0.,
  Enough.space.another.toilet,
  Water.source..fetch.,
  X1.9..latitude.,
  X1.9..longitude.,
  adults.using.toilet,
  children.using.toilet,
  people.using.toilet,
  Where.do.you.dispose.your.solid.wastes.,
  starts_with("Age.of.toilet"),
  What.happens.when.the.toilet.gets.full.,
  Toilet.emptying.time,
  Interface.Layout,
  CONTAINMENT.SUBSTRUCTURE,
  Record.the.observed.shape.of.the.substructure.containment.,
  Width:Height,
  Perception.of.the.fill.level:Is.the.toilet.easily.accessible.to.the.following...Push.Cart
))

write.csv(Kanyama, file = "Kanyama_organized.csv", row.names = F)
write.csv(Kanyama_essential, file = "Kanyama_to_plot.csv", row.names = F)
