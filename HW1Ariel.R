##### Data Science CS429 Homework 1 #####
nytData <- read.csv("HW1_data/nyt1.csv")
View(nytData)
str(nytData)

install.packages("ggplot2")
library(ggplot2)

# Initial histograms of each variable
hist(nytData$Age)
hist(nytData$Gender, breaks=2)
hist(nytData$Impressions)
hist(nytData$Clicks)
hist(nytData$Signed_In, breaks = 2)

# categorize age_group
nytData$age_group <- cut(nytData$Age, c(-Inf,18,24,34,54,64,Inf))
hist(as.integer(nytData$age_group))

# Using all data
nytData$genderFactor <- factor(nytData$Gender)
ggplot(nytData, aes(x=Impressions, fill = age_group))+geom_histogram(binwidth = 1)
ggplot(nytData, aes(x=Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) # is gender 0 for people who did not sign in?

####### Data of people who signed in #######
signIn <- subset(nytData, Signed_In==1)
nrow(signIn)
hist(signIn$Age, breaks=10)
hist(signIn$Impressions)
ggplot(signIn, aes(x=Impressions, fill = age_group))+geom_histogram(binwidth = 1)
ggplot(signIn, aes(x=Impressions, fill = genderFactor))+geom_histogram(binwidth = 1)

# CTR (click through rate)
ggplot(subset(signIn,Impressions>0 & Clicks != 0), aes(x=Clicks/Impressions, fill=age_group))+geom_histogram(binwidth = 0.04)

# gender categorization
gen_click_counts <- table(signIn$Gender, signIn$Clicks)
barplot(gen_click_counts, col=c("darkblue", "red"), main = "Number of Clicks by Gender", xlab = "Number of Clicks", legend = c("Male", "Female"), beside=TRUE)

####### Data of people who did not sign in ########
# People who did not sign in have no data on gender or age
noSignIn <- subset(nytData, Signed_In == 0)

# histogram comparing impressions of sign in and no sign in
nytData$signInFactor <- factor(nytData$Signed_In) # create a factor of Signed_In
ggplot(nytData, aes(x=Impressions, fill=signInFactor))+geom_histogram(binwidth = 1) # stacked on each other

#### new category (may show that more impressions will mean more clicks) ###
nytData$impcat = cut(nytData$Impressions, c(0,5,10,15,20,Inf))
ggplot(subset(nytData, Clicks>0), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)
ggplot(subset(nytData, Clicks>1), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)
ggplot(subset(nytData, Clicks>2), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)
ggplot(subset(nytData, Clicks>3), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)

######## Example code from "Doing Data Science" ########

# categorize
nytData$agecat <- cut(nytData$Age, c(-Inf,18,24,34,54,64,Inf))
hist(as.integer(nytData$agecat))

# view
summary(nytData)

# brackets
install.packages("doBy")
library("doBy")
siterange <- function(x){
  c(length(x), min(x), mean(x), max(x))
}
summaryBy(Age~agecat, data = nytData, FUN=siterange)

# so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data = nytData)

# plot
ggplot(nytData, aes(x=Impressions, fill=agecat))+geom_histogram(binwidth=1)

ggplot(nytData, aes(x=agecat, y=Impressions, fill=agecat))+
  geom_boxplot()

# create click thru rate
# we don't care about clicks if there are no impressions
# if there are clicks with no imps my assumption about
# this data are wrong
nytData$hasimps <- cut(nytData$Impressions, c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data = nytData, FUN = siterange)
ggplot(subset(nytData, Impressions>0), aes(x=Clicks/Impressions, colour=agecat))+ geom_density()
ggplot(subset(nytData, Clicks>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()
ggplot(subset(nytData, Clicks>0), aes(x=agecat, y=Clicks,fill=agecat)) + geom_boxplot()
ggplot(subset(nytData, Clicks>0), aes(x=Clicks, colour=agecat)) + geom_density()

# create categories
nytData$scode[nytData$Impressions==0] <- "NoImps"
nytData$scode[nytData$Impressions>0] <- "Imps"
nytData$scode[nytData$Clicks>0] <- "Clicks"

# Convert the column to a factor
nytData$scode <- factor(nytData$scode)
head(nytData)

# look at levels
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat, data = nytData, FUN=clen)
