##### Data Science CS429 Homework 1 #####
nyt1 <- read.csv("HW1_data/nyt1.csv")
View(nyt1)

install.packages("ggplot2")
library(ggplot2)


hist(nyt1$Age)
hist(nyt1$Gender)
hist(nyt1$Impressions)
hist(nyt1$Clicks)
hist(nyt1$Signed_In)

# Example code from "Doing Data Science"
nyt1$agecat <- cut(nyt1$Age, c(-Inf,18,24,34,54,64,Inf))
hist(as.integer(nyt1$agecat))

ggplot(nyt1, aes(x=Impressions, fill=agecat))+
  geom_histogram(binwidth=1)

ggplot(nyt1, aes(x=agecat, y=Impressions, fill=agecat))+
  geom_boxplot()