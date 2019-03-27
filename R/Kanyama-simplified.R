#Loading the libraries and dataset ----
library(dplyr)
library(readxl)
library(leaflet)
library(ggplot2)

setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Data")
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

Columns.Remover <- function(ds, percentage){
  
  
  selection <- apply(ds, 2, function(x)  (sum( !is.na(x) ) / nrow(ds)) > percentage )
  
  return(selection)
  
}

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


simplify <- function(df){
  df.reduced <- df %>% 
    select(-starts_with("DESCRIPTION OF RESPONDENT:"),
           -starts_with("SELECT ZONE (Other"), 
           -starts_with("SELECT ZONE SECTION (Other"),
           -`RECORD TYPE OF PROPERTY (Other (please specify)) - specify`,
           - `1.3 (Don't Know)`,
           - `1.5 (Don't Know)`,
           -`1.6.1 Do you think there is space on this plot to construct another toilet?: No - If Yes how many more? If No, why is that the case?`,
           -`1.6.2 (Other (please specify)) - specify`,
           -`1.7`,
           -`1.7.1`,
           -`2.1D`,
           -`What is the designation of the respondent?`,
           -`3.4 (Don't Know)`,
           -starts_with("3.5"),
           -starts_with("What do you want to upgrade your toilet"),
           -starts_with("What happens when the toilet gets full? (Other"),
           -ends_with("Months - Age"),
           -`3.7 (Other (please specify)) - specify`,
           -starts_with("3.7.1 ("),
           -starts_with("How did you know about the service of emptying your toilet"),
           -starts_with("How would you rate your level of satisfaction with the service you received from the emptiers?" ),
           -starts_with("3.7.3"),
           -starts_with("Was the fee you paid affordable? (" ),
           -starts_with("How often do you empty your toilet? ("),
           -starts_with("3.8"),
           -starts_with("4.1"),
           -starts_with("4.2"),
           -starts_with("4.3 SLAB"),
           -starts_with("4.3 INTERFACE ("),
           -starts_with("CONTAINMENT/SUBSTRUCTURE ("),
           -starts_with("Record the observed shape of the substructure/containment  ("),
           -starts_with("TAKE PHOTO OF"),
           -starts_with("TAKE  PHOTO OF"),
           -starts_with("4.8 (Don't Know)")
           
    )
  
  if(any(df.reduced$`Is there another toilet to observe` == "No")){
    
    index <- grep("Is there another", colnames(df.reduced)) + 1
    df.reduced <- select(df.reduced, -c(index:ncol(df.reduced)))
  }
  
  return(df.reduced)
}

Kanyama.reduced <- simplify(Kanyama)

#Grouping the last questions----

grouping.columns <- function(df, column1, column2, name){
 
  res <- select(df, -starts_with(name)) 
  mutate(res, !!name := case_when(
    column1 == T ~ T,
    column1 != T & column2 == T ~ F,
    TRUE ~ NA))
  
}

Kanyama.reduced <- grouping.columns(Kanyama.reduced,
                 Kanyama.reduced$`Is the toilet easily accessible to the following people?: Children - Yes`,
                 Kanyama.reduced$`Is the toilet easily accessible to the following people?: Children - No`,
                 "Is the toilet easily accessible to the following people?: Children")

Kanyama.reduced <- grouping.columns(Kanyama.reduced,
                                Kanyama.reduced$`Is the toilet easily accessible to the following people?: Persons with dissability - Yes`, 
                                Kanyama.reduced$`Is the toilet easily accessible to the following people?: Persons with dissability - No`,
                                "Is the toilet easily accessible to the following people?: Persons with dissability")

Kanyama.reduced <- grouping.columns(Kanyama.reduced,
                                    Kanyama.reduced$`Is the toilet easily accessible to the following people?: Women at night - Yes`, 
                                    Kanyama.reduced$`Is the toilet easily accessible to the following people?: Women at night - No`,
                                    "Is the toilet easily accessible to the following people?: Women at night")

Kanyama.reduced <- grouping.columns(Kanyama.reduced,
                                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Vacuum Tanker  - Yes`, 
                                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Vacuum Tanker  - No`,
                                    "Is the toilet easily accessible to the following?: Vacuum Tanker")

Kanyama.reduced <- grouping.columns(Kanyama.reduced,
                                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Light Truck - Yes` , 
                                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Light Truck - No`,
                                    "Is the toilet easily accessible to the following?: Light Truck")

Kanyama.reduced <- grouping.columns(Kanyama.reduced,
                                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Push Cart - Yes`, 
                                    Kanyama.reduced$`Is the toilet easily accessible to the following?: Push Cart - No`,
                                    "Is the toilet easily accessible to the following?: Push Cart")


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

test3 <- grouping.columns(test3,
                                    test3$`Is the toilet easily accessible to the following people?: Children - Yes61`,
                                    test3$`Is the toilet easily accessible to the following people?: Children - No62`,
                                    "Is the toilet easily accessible to the following people?: Children")

test3 <- grouping.columns(test3,
                                    test3$`Is the toilet easily accessible to the following people?: Persons with dissability - Yes59`, 
                                    test3$`Is the toilet easily accessible to the following people?: Persons with dissability - No60`,
                                    "Is the toilet easily accessible to the following people?: Persons with dissability")

test3 <- grouping.columns(test3,
                                    test3$`Is the toilet easily accessible to the following people?: Women at night - Yes63`, 
                                    test3$`Is the toilet easily accessible to the following people?: Women at night - No64`,
                                    "Is the toilet easily accessible to the following people?: Women at night")

test3 <- grouping.columns(test3,
                                    test3$`Is the toilet easily accessible to the following?: Vacuum Tanker  - Yes53`, 
                                    test3$`Is the toilet easily accessible to the following?: Vacuum Tanker  - No54`,
                                    "Is the toilet easily accessible to the following?: Vacuum Tanker")

test3 <- grouping.columns(test3,
                                    test3$`Is the toilet easily accessible to the following?: Light Truck - Yes55` , 
                                    test3$`Is the toilet easily accessible to the following?: Light Truck - No56`,
                                    "Is the toilet easily accessible to the following?: Light Truck")

test3 <- grouping.columns(test3,
                                    test3$`Is the toilet easily accessible to the following?: Push Cart - Yes57`, 
                                    test3$`Is the toilet easily accessible to the following?: Push Cart - No58`,
                                    "Is the toilet easily accessible to the following?: Push Cart")
rm(test1,test2)
test3 <- select(test3, -`Is there a third toilet to observe`)

more.than.2.toilet <- simplify(more.than.2.toilet)

test4 <- more.than.2.toilet[,118:ncol(more.than.2.toilet)]
test5 <- more.than.2.toilet[,1:55]
test6 <- cbind(test5,test4)

test6 <- grouping.columns(test6,
                          test6$`Is the toilet easily accessible to the following people?: Children - Yes109`,
                          test6$`Is the toilet easily accessible to the following people?: Children - No110`,
                          "Is the toilet easily accessible to the following people?: Children")

test6 <- grouping.columns(test6,
                          test6$`Is the toilet easily accessible to the following people?: Persons with dissability - Yes107`, 
                          test6$`Is the toilet easily accessible to the following people?: Persons with dissability - No108`,
                          "Is the toilet easily accessible to the following people?: Persons with dissability")

test6 <- grouping.columns(test6,
                          test6$`Is the toilet easily accessible to the following people?: Women at night - Yes111`, 
                          test6$`Is the toilet easily accessible to the following people?: Women at night - No112`,
                          "Is the toilet easily accessible to the following people?: Women at night")

test6 <- grouping.columns(test6,
                          test6$`Is the toilet easily accessible to the following?: Vacuum Tanker  - Yes101`, 
                          test6$`Is the toilet easily accessible to the following?: Vacuum Tanker  - No102`,
                          "Is the toilet easily accessible to the following?: Vacuum Tanker")

test6 <- grouping.columns(test6,
                          test6$`Is the toilet easily accessible to the following?: Light Truck - Yes103` , 
                          test6$`Is the toilet easily accessible to the following?: Light Truck - No104`,
                          "Is the toilet easily accessible to the following?: Light Truck")

test6 <- grouping.columns(test6,
                          test6$`Is the toilet easily accessible to the following?: Push Cart - Yes105`, 
                          test6$`Is the toilet easily accessible to the following?: Push Cart - No106`,
                          "Is the toilet easily accessible to the following?: Push Cart")
rm(test4,test5)


names(test3) <- names(Kanyama.reduced)
names(test6) <- names(Kanyama.reduced)

Kanyama.final <- rbind(Kanyama.reduced, test3, test6)

write.csv(Kanyama.final, file = "Kanyama_reduced.csv", row.names = F)

#Still have to organize the code (`Check Columns tomorrow`)