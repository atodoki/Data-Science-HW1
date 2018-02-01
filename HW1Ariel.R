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

# Using all data
ggplot(nyt1, aes(x=Impressions, fill = agecat))+geom_histogram(binwidth = 1)


## Data of people who signed in
signIn <- subset(nyt1, Signed_In==1)
nrow(signIn)
hist(signIn$Age, breaks = 10)
hist(signIn$Impressions)
ggplot(signIn, aes(x=Impressions, fill = agecat))+geom_histogram(binwidth = 1)

# CTR (click through rate)
ggplot(subset(signIn,Impressions>0 & Clicks != 0), aes(x=Clicks/Impressions, fill=agecat))+geom_histogram(binwidth = 0.04)

## Data of people who did not sign in
noSignIn <- subset(nyt1, Signed_In == 0)

# histogram comparing impressions of sign in and no sign in (they are layerd on top of each other)
ggplot(signIn, aes(x=Impressions))+geom_histogram(binwidth = 1, fill="blue")+geom_histogram(data = noSignIn, binwidth = 1, fill="red")



######## Example code from "Doing Data Science" ########

# categorize
nyt1$agecat <- cut(nyt1$Age, c(-Inf,18,24,34,54,64,Inf))
hist(as.integer(nyt1$agecat))

# view
summary(nyt1)

# brackets
install.packages("doBy")
library("doBy")
siterange <- function(x){
  c(length(x), min(x), mean(x), max(x))
}
summaryBy(Age~agecat, data = nyt1, FUN=siterange)

# so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data = nyt1)

# plot
ggplot(nyt1, aes(x=Impressions, fill=agecat))+geom_histogram(binwidth=1)

ggplot(nyt1, aes(x=agecat, y=Impressions, fill=agecat))+
  geom_boxplot()

# create click thru rate
# we don't care about clicks if there are no impressions
# if there are clicks with no imps my assumption about
# this data are wrong
nyt1$hasimps <- cut(nyt1$Impressions, c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data = nyt1, FUN = siterange)
ggplot(subset(nyt1, Impressions>0), aes(x=Clicks/Impressions, colour=agecat))+ geom_density()
ggplot(subset(nyt1, Clicks>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()
ggplot(subset(nyt1, Clicks>0), aes(x=agecat, y=Clicks,fill=agecat)) + geom_boxplot()
ggplot(subset(nyt1, Clicks>0), aes(x=Clicks, colour=agecat)) + geom_density()

# create categories
nyt1$scode[nyt1$Impressions==0] <- "NoImps"
nyt1$scode[nyt1$Impressions>0] <- "Imps"
nyt1$scode[nyt1$Clicks>0] <- "Clicks"

# Convert the column to a factor
nyt1$scode <- factor(nyt1$scode)
head(nyt1)

# look at levels
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat, data = nyt1, FUN=clen)
