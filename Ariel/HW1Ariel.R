##### Data Science CS429 Homework 1 #####

day1 <- 1
day2 <- 6
day3 <- 8
day4 <- 10
day5 <- 27
day6 <- 13
day7 <- 20
nytData <- read.csv(paste0("HW1_data/nyt",toString(day1),".csv"))
View(nytData)
str(nytData)

nytData2 <- read.csv(paste0("HW1_data/nyt",toString(day2),".csv"))
nytData3 <- read.csv(paste0("HW1_data/nyt",toString(day3),".csv"))
nytData4 <- read.csv(paste0("HW1_data/nyt",toString(day4),".csv"))
nytData5 <- read.csv(paste0("HW1_data/nyt",toString(day5),".csv"))
nytData6 <- read.csv(paste0("HW1_data/nyt",toString(day6),".csv"))
nytData7 <- read.csv(paste0("HW1_data/nyt",toString(day7),".csv"))

install.packages("ggplot2")
library(ggplot2)

# Initial histograms of each variable
hist(nytData$Age , col = "skyblue")
hist(nytData$Gender, breaks=2)
hist(nytData$Impressions)
hist(nytData$Clicks)
hist(nytData$Signed_In, breaks = 2)

# categorize age_group
nytData$age_group <- cut(nytData$Age, c(-Inf,18,24,34,54,64,Inf))
hist(as.integer(nytData$age_group))

# Using all data
# factor gender
nytData$genderFactor[nytData$Gender == 0 & nytData$Signed_In == 1] <- "Female"
nytData$genderFactor[nytData$Gender == 0 & nytData$Signed_In == 0] <- "Unknown"
nytData$genderFactor[nytData$Gender == 1] <- "Male"
nytData$genderFactor <- factor(nytData$genderFactor)

ggplot(nytData, aes(x=Impressions, fill = age_group))+geom_histogram(binwidth = 1)+labs(title = "Histogram of Impressions by Age Group", fill = "Age Group")
ggplot(nytData, aes(x=Impressions, fill = genderFactor))+geom_histogram(binwidth = 1)+labs(title = "Histogram of Impressions by Gender", fill = "Gender")

####### Data of people who signed in #######
signIn <- subset(nytData, Signed_In==1)
nrow(signIn)
hist(signIn$Age, breaks=10)
hist(signIn$Impressions)
plot(density(signIn$Impressions))
ggplot(subset(signIn, Age < 18), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age < 18", fill = "Gender")
ggplot(signIn, aes(x=Impressions, fill = age_group))+geom_histogram(binwidth = 1)
ggplot(signIn, aes(x=Impressions, fill = genderFactor))+geom_histogram(binwidth = 1)

# Histogram with Normal Curve 
# code found at https://www.statmethods.net/graphs/density.html
x <- signIn$Impressions
h <- hist(x, xlab = "Impressions", main="Histogram of Impressions with Normal Curve \n(signed in users)")
xfit <- seq(min(x),max(x), length = 40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)


# CTR (click through rate)
ggplot(subset(signIn,Impressions>0 & Clicks != 0), aes(x=Clicks/Impressions, fill=age_group))+geom_histogram(binwidth = 0.05) + labs(title="Click Through Rate (CTR)\n(does not include 0 clicks)")

# gender categorization
gen_click_counts <- table(signIn$Gender, signIn$Clicks)
barplot(gen_click_counts, col=c("darkblue", "red"), main = "Number of Clicks by Gender", xlab = "Number of Clicks", legend = c("Male", "Female"), beside=TRUE)

####### Data of people who did not sign in ########
# People who did not sign in have no data on gender or age
noSignIn <- subset(nytData, Signed_In == 0)
hist(noSignIn$Impressions)

# histogram comparing impressions of sign in and no sign in
nytData$signInFactor[nytData$Signed_In==0] <- "no"
nytData$signInFactor[nytData$Signed_In==1] <- "yes"
nytData$signInFactor <- factor(nytData$signInFactor) # create a factor of Signed_In
ggplot(nytData, aes(x=Impressions, fill=signInFactor))+geom_histogram(binwidth = 1)+labs(title="Impressions", subtitle="signed in users vs non signed in users", fill = "Signed In") # stacked on each other

#### new category (may show that more impressions will mean more clicks) ###
nytData$impcat = cut(nytData$Impressions, c(0,5,10,15,20,Inf))
ggplot(subset(nytData, Clicks>0), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")
ggplot(subset(nytData, Clicks>1), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")
ggplot(subset(nytData, Clicks>2), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")
ggplot(subset(nytData, Clicks>3), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")


#### Plots on multiple days
dayNames <- c(day1, day2, day3, day4, day5, day6, day7)

clicks1 <- sum(nytData$Clicks)
clicks2 <- sum(nytData2$Clicks)
clicks3 <- sum(nytData3$Clicks)
clicks4 <- sum(nytData4$Clicks)
clicks5 <- sum(nytData5$Clicks)
clicks6 <- sum(nytData6$Clicks)
clicks7 <- sum(nytData7$Clicks)

totalClicks <- c(clicks1, clicks2, clicks3, clicks4, clicks5, clicks6, clicks7)
names(totalClicks) <- dayNames
barplot(totalClicks, col=c("lightgreen", "lightcoral"), main = "Number of Clicks Per Day", xlab = "Day")

imp1 <- sum(nytData$Impressions)
imp2 <- sum(nytData2$Impressions)
imp3 <- sum(nytData3$Impressions)
imp4 <- sum(nytData4$Impressions)
imp5 <- sum(nytData5$Impressions)
imp6 <- sum(nytData6$Impressions)
imp7 <- sum(nytData7$Impressions)

totalImp <- c(imp1, imp2, imp3, imp4, imp5, imp6,imp7)
names(totalImp) <- dayNames
barplot(totalImp, col=c("lightgreen", "lightcoral"), main = "Number of Impressions Per Day", xlab = "Day")

sign1 <- sum(nytData$Signed_In)
sign2 <- sum(nytData2$Signed_In)
sign3 <- sum(nytData3$Signed_In)
sign4 <- sum(nytData4$Signed_In)
sign5 <- sum(nytData5$Signed_In)
sign6 <- sum(nytData6$Signed_In)
sign7 <- sum(nytData7$Signed_In)

totalSign <- c(sign1, sign2, sign3, sign4, sign5, sign6, sign7)
names(totalSign) <- dayNames
barplot(totalSign, col=c("lightgreen", "lightcoral"), main = "Number of Sign Ins Per Day", xlab = "Day")

# Plot the total number of people per day
totalVisits <- c(nrow(nytData),nrow(nytData2),nrow(nytData3),nrow(nytData4),nrow(nytData5),nrow(nytData6),nrow(nytData7))
names(totalVisits) <- dayNames
barplot(totalVisits, col=c("lightgreen", "lightcoral"), main = "Number of Visitors Per Day", xlab = "Day")

# Plot CTR for each day
totalCTR <- c(clicks1/imp1,clicks2/imp2,clicks3/imp3,clicks4/imp4,clicks5/imp5,clicks6/imp6,clicks7/imp7)
names(totalCTR) <- dayNames
barplot(totalCTR, col=c("lightgreen", "lightcoral"), main = "Click Through Rate Per Day", xlab = "Day")

######################### Example code from "Doing Data Science" #####################

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
