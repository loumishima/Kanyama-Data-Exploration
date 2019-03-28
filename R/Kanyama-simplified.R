#Loading the libraries and dataset ----
library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)

setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/R")

source("functions.R")

setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/data")
Kanyama.raw <- read_xlsx("KANYAMA.xlsx", sheet = 2, skip = 1)

Kanyama <- Kanyama.raw %>% select(-c(1:23, 27)) %>% filter(.$`Are you willing to participate?` == "Yes")

#Renaming some columns for better understanding----
Kanyama <- Kanyama %>% rename("Record_plot_number" = `1.3`,
                                  "Families_on_the_plot" = `1.4`,
                                  "People_on_the_plot" = `1.5`,
                                  "VIP toilets" = `1.6 - 1 - 1.5.1`,
                                  "ECOSAN toilets" = `1.6 - 2 - 1.5.1`,
                                  "Inside waterflush toilets" = `1.6 - 3 - 1.5.1`,
                                  "Outside waterflush toilets" = `1.6 - 4 - 1.5.1`,
                                  "Poor flush Inside" = `1.6 - 5 - 1.5.1`,
                                  "Poor flush Outside" = `1.6 - 6 - 1.5.1`,
                                  "Lined Pit latrine" = `1.6 - 7 - 1.5.1`,
                                  "Unlined Pit latrine" = `1.6 - 8 - 1.5.1`,
                                  "Disused/Buried" = `1.6 - 9 - 1.5.1`,
                                  "Water source (fetch)" = `1.6.2`,
                                  "Emptied the toilet before?" = `3.7`,
                                  "Last time emptied" = `3.7.1`,
                                  "Who emptied?" = `3.7.2`,
                                  "Interface Layout" = `4.3 INTERFACE`,
                                  "Width" = `4.4`,
                                  "Diameter" = `4.416`,
                                  "Length" = `4.5`,
                                  "Height" = `4.6`,
                                  "Perception of the fill level" = `4.7`,
                                  "Is emptying feasible?" = `4.8`,
                                  "Is washing hand basin present?" = `4.9`,
                                  "Region" = `1.2`,
                                  "Landlord live in the plot?" = `1.8`,
                                  "Upgraded toilet recently?" = `3.4`
                                  )

#!na/total ratio filtering ----


Boolean.50 <- Columns.Remover(Kanyama, 0.5)
Kanyama.50perc <- Kanyama[Boolean.50]

Boolean.75 <- Columns.Remover(Kanyama, 0.75)
Kanyama.75perc <- Kanyama[Boolean.75]

Boolean.80 <- Columns.Remover(Kanyama, 0.80)
Kanyama.80perc <- Kanyama[Boolean.80]

Boolean.90<- Columns.Remover(Kanyama, 0.90)
Kanyama.90perc <- Kanyama[Boolean.90]

Deep.Learning <- Kanyama.90perc %>% select(contains("TAKE "),
                                          `Perception of the fill level`,
                                          contains("Condition")
                                           )

Deep.Learning <- Deep.Learning %>% filter(!is.na(`TAKE  PHOTO OF INSIDE THE TOILET`) & 
                                            !is.na(`TAKE PHOTO OF OUTSIDE THE TOILET/CONTAINMENT `))

write.csv(Deep.Learning, file = "Deep_Learning.csv" , row.names = F )

#Removing the answers about the respondent and another columns with no use ----



Kanyama.reduced <- simplify(Kanyama)

#Grouping the last questions----


columns_yes <- list(Kanyama.reduced$`Is the toilet easily accessible to the following people?: Children - Yes`,
                    Kanyama.reduced$`Is the toilet easily accessible to the following people?: Persons with dissability - Yes`,
                    Kanyama.reduced$`Is the toilet easily accessible to the following people?: Women at night - Yes`,
                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Vacuum Tanker  - Yes`,
                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Light Truck - Yes`,
                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Push Cart - Yes`)

columns_no <- list(Kanyama.reduced$`Is the toilet easily accessible to the following people?: Children - No`,
                   Kanyama.reduced$`Is the toilet easily accessible to the following people?: Persons with dissability - No`,
                   Kanyama.reduced$`Is the toilet easily accessible to the following people?: Women at night - No`,
                   Kanyama.reduced$`Is the toilet easily accessible to the following?: Vacuum Tanker  - No`,
                   Kanyama.reduced$`Is the toilet easily accessible to the following?: Light Truck - No`,
                   Kanyama.reduced$`Is the toilet easily accessible to the following?: Push Cart - No`)


names <- c("Is the toilet easily accessible to the following people?: Children",
           "Is the toilet easily accessible to the following people?: Persons with dissability",
           "Is the toilet easily accessible to the following people?: Women at night",
           "Is the toilet easily accessible to the following?: Vacuum Tanker",
           "Is the toilet easily accessible to the following?: Light Truck",
           "Is the toilet easily accessible to the following?: Push Cart")



Kanyama.reduced <- grouping.columns(Kanyama.reduced, columns_yes, columns_no, names)

Kanyama.reduced <- select(Kanyama.reduced, -`Is there another toilet to observe`)

#Solving the Multiple toilets problem ----

more.than.1.toilet <- Kanyama %>% filter(`Is there another toilet to observe` == "Yes")
more.than.2.toilet <- Kanyama %>% filter(`Is there a third toilet to observe` == "Yes")

#removing unused parameters
more.than.1.toilet <- simplify(more.than.1.toilet)

index_beg <- grep("Is there another", colnames(more.than.1.toilet))
index_end <- grep("Is there a third", colnames(more.than.1.toilet))
index_sub <- grep("Inter", colnames(more.than.1.toilet))

test1 <- more.than.1.toilet[, 1:55]
test2 <- more.than.1.toilet[,87:117]
test3 <- cbind(test1,test2)


columns_yes <- list(test3$`Is the toilet easily accessible to the following people?: Children - Yes`,
                    test3$`Is the toilet easily accessible to the following people?: Persons with dissability - Yes`,
                    test3$`Is the toilet easily accessible to the following people?: Women at night - Yes`,
                    test3$`Is the toilet easily accessible to the following?: Vacuum Tanker  - Yes`,
                    test3$`Is the toilet easily accessible to the following?: Light Truck - Yes`,
                    test3$`Is the toilet easily accessible to the following?: Push Cart - Yes`)

columns_no <- list(test3$`Is the toilet easily accessible to the following people?: Children - No`,
                   test3$`Is the toilet easily accessible to the following people?: Persons with dissability - No`,
                   test3$`Is the toilet easily accessible to the following people?: Women at night - No`,
                   test3$`Is the toilet easily accessible to the following?: Vacuum Tanker  - No`,
                   test3$`Is the toilet easily accessible to the following?: Light Truck - No`,
                   test3$`Is the toilet easily accessible to the following?: Push Cart - No`)

test3 <- grouping.columns(test3, columns_yes, columns_no, names)
rm(test1,test2)

test3 <- select(test3, -`Is there a third toilet to observe`)

more.than.2.toilet <- simplify(more.than.2.toilet)

test4 <- more.than.2.toilet[,118:ncol(more.than.2.toilet)]
test5 <- more.than.2.toilet[,1:55]
test6 <- cbind(test5,test4)

columns_yes <- list(test6$`Is the toilet easily accessible to the following people?: Children - Yes`,
                    test6$`Is the toilet easily accessible to the following people?: Persons with dissability - Yes`,
                    test6$`Is the toilet easily accessible to the following people?: Women at night - Yes`,
                    test6$`Is the toilet easily accessible to the following?: Vacuum Tanker  - Yes`,
                    test6$`Is the toilet easily accessible to the following?: Light Truck - Yes`,
                    test6$`Is the toilet easily accessible to the following?: Push Cart - Yes`)

columns_no <- list(test6$`Is the toilet easily accessible to the following people?: Children - No`,
                   test6$`Is the toilet easily accessible to the following people?: Persons with dissability - No`,
                   test6$`Is the toilet easily accessible to the following people?: Women at night - No`,
                   test6$`Is the toilet easily accessible to the following?: Vacuum Tanker  - No`,
                   test6$`Is the toilet easily accessible to the following?: Light Truck - No`,
                   test6$`Is the toilet easily accessible to the following?: Push Cart - No`)

test6 <- grouping.columns(test6, columns_yes, columns_no, names)

rm(test4,test5)


names(test3) <- names(Kanyama.reduced)
names(test6) <- names(Kanyama.reduced)

Kanyama.final <- rbind(Kanyama.reduced, test3, test6)

write.csv(Kanyama.final, file = "Kanyama_reduced.csv", row.names = F)

#Still have to organize the code (`Check Columns tomorrow`)