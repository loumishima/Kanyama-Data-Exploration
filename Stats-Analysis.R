library(tidyverse)
library(data.table)
library(ggplot2)
require(gridExtra)
require(boot)
require(Rmisc)

setwd("/Users/gather3/Documents/Kanyama - Data Exploration/Kanyama Data Exploration/data")


Kanyama <- read.csv("Kanyama_to_plot.csv")

summary(Kanyama)

Kanyama <- Kanyama %>% filter(RECORD.TYPE.OF.PROPERTY == "Residential Plot" & people.using.toilet <= 100 ) 

numeric_stats <- Kanyama %>% summarise_if(is.numeric,
                                          list("~max" = ~max(., na.rm = T),
                                               "~min" = ~min(., na.rm = T),
                                               "~mean" = ~mean(., na.rm = T),
                                               "~median" =~median(., na.rm = T),
                                               "~sd" =~sd(., na.rm = T),
                                               "~mad" =~mad(., na.rm = T),
                                               "~IQR" =~IQR(., na.rm = T)))

numeric_stats <- numeric_stats %>% select(c(starts_with("People"),
                                            starts_with("Total"),
                                            starts_with("adults"),
                                            starts_with("children")))


numeric_stats <- gather(numeric_stats, "Name" , "Stats"  )
numeric_stats <- separate(numeric_stats, Name, c("Name","Metric"), sep = "~" )
numeric_stats <- spread(numeric_stats, "Metric", "Stats")
numeric_stats <- select(numeric_stats, c("Name", "min", "max","mean", "median", "sd", "mad", "IQR"))


toiletQuantiles <- quantile(Kanyama$Total.number.of.toilets.0., p=c(.05,.25,.5,.75,.95,.96,.97,.98,.99,1))
peopleQunatiles <- quantile(Kanyama$people.using.toilet, p=c(.05,.25,.5,.75,.95,.96,.97,.98,.99,1))


boxplot(Kanyama$Total.number.of.toilets.0., ylab= "Toilets per plot")
boxplot(Kanyama$people.using.toilet, ylab= "People using toilet")

ggplot(data = Kanyama[, c("Total.number.of.toilets.0.", "people.using.toilet")]) +
  geom_boxplot(aes(x = "Total.number.of.toilets.0.",y = "people.using.toilet" ))

require(reshape2)
ggplot(data = melt(Kanyama[, c("Total.number.of.toilets.0.", "people.using.toilet")]),
       aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable))


ggplot(data = Kanyama , aes(x = people.using.toilet)) +
  geom_histogram(stat = "count", fill = "#B10048") +
  labs(title = "People using Toilet in Kanyama",
       x = "People living in the plots",
       y = "Count of plots")


p1 <- ggplot(data = Kanyama , aes(x = people.using.toilet)) +
  geom_histogram(fill = "#B10048", bins = 30, aes(y = ..density..)) +
  geom_density() +
  labs(title = "People using Toilet in Kanyama vs Number of Toilets",
       x = "People living in the plots",
       y = "Density")

p2 <- ggplot(data = Kanyama , aes(x = Total.number.of.toilets.0.)) +
  geom_histogram( fill = "#B10048", bins = 30 , aes(y = ..density..)) +
  geom_density() +
  labs(x = "Number of Toilets",
       y = "Density")


p3 <- ggplot(data = Kanyama , aes(x = people.using.toilet)) +
  geom_histogram(stat = "count", fill = "#B10048", bins = 30) +
  labs(title = "People using Toilet in Kanyama vs Number of Toilets",
       x = "People living in the plots",
       y = "Count")

p4 <- ggplot(data = Kanyama , aes(x = Total.number.of.toilets.0.)) +
  geom_histogram(stat= "count", fill = "#B10048", bins = 30) +
  labs(x = "Number of Toilets",
       y = "Count")

grid.arrange(p1, p2, nrow=2)
grid.arrange(p3, p4, nrow=2)

# To use Correlation it must be all numeric
# One hot encoding needs to be applied

numericKanyama <- Kanyama %>%
  select_if(is.numeric) %>%
  select(-c(starts_with("Age"), Width,Diameter,Length, Height)) %>% 
  filter(complete.cases(.))

correlationKanyama <- cor(numericKanyama)


stat_fun <- function(x,idx) median(x[idx])

boot_obj <- boot(numericKanyama$Perception.of.the.fill.level, R = 1000, statistic = stat_fun)
boot_obj

# Confidence Interval about all numeric columns

KanyamaCI<- t(apply(numericKanyama, 2, CI))

# Using qqplot to check the data distribution

NormalizedKanyama <- scale(numericKanyama)

# applying qqplot to one variable

qqnorm(NormalizedKanyama[,"people.using.toilet"])
abline(a = 0, b = 1, col = 'grey')

qqnorm(NormalizedKanyama[,"Total.number.of.toilets.0."])
abline(a = 0, b = 1, col = 'grey')




