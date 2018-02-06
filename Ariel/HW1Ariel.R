##### Data Science CS429 Homework 1 #####

## Install and load these packages if have not already done so
# install.packages("ggplot2")
# library(ggplot2)

# Read in all 31 csv files into a list of data frames
datalist <- list()
for (i in 1:31){
  datapoint <- read.csv(paste0("HW1_data/nyt",toString(i),".csv"))
  datalist[[i]]<-datapoint 
}

# day1 <- 25
# day2 <- 26
# day3 <- 27
# day4 <- 28
# day5 <- 29
# day6 <- 30
# day7 <- 31
# nytData <- read.csv(paste0("HW1_data/nyt",toString(day1),".csv"))
# View(nytData)
# str(nytData)
# 
# nytData2 <- read.csv(paste0("HW1_data/nyt",toString(day2),".csv"))
# nytData3 <- read.csv(paste0("HW1_data/nyt",toString(day3),".csv"))
# nytData4 <- read.csv(paste0("HW1_data/nyt",toString(day4),".csv"))
# nytData5 <- read.csv(paste0("HW1_data/nyt",toString(day5),".csv"))
# nytData6 <- read.csv(paste0("HW1_data/nyt",toString(day6),".csv"))
# nytData7 <- read.csv(paste0("HW1_data/nyt",toString(day7),".csv"))

############## New variables to categorize or segment users #############
# categorize age_group
for(i in 1:31){
  datalist[[i]]$age_group <- cut(datalist[[i]]$Age, c(-Inf,18,24,34,54,64,Inf))
}
# factor gender
for(i in 1:31){
  datalist[[i]]$genderFactor[datalist[[i]]$Gender == 0 & datalist[[i]]$Signed_In == 1] <- "Female"
  datalist[[i]]$genderFactor[datalist[[i]]$Gender == 0 & datalist[[i]]$Signed_In == 0] <- "Unknown"
  datalist[[i]]$genderFactor[datalist[[i]]$Gender == 1] <- "Male"
  datalist[[i]]$genderFactor <- factor(datalist[[i]]$genderFactor)
}
# factor Signed_In
for(i in 1:31){
  datalist[[i]]$signInFactor[datalist[[i]]$Signed_In==0] <- "no"
  datalist[[i]]$signInFactor[datalist[[i]]$Signed_In==1] <- "yes"
  datalist[[i]]$signInFactor <- factor(datalist[[i]]$signInFactor)
}
# categorize Impressions
for(i in 1:31){
  datalist[[i]]$impcat = cut(datalist[[i]]$Impressions, c(0,5,10,15,20,Inf))
}
####################################################

# Initialize nytData to be the dataframe for one day
nytData <- datalist[[1]]

# Initial histograms of each variable
initHistograms <- function(data){
  hist(data$Age , col = "skyblue", main= "Histogram of Age")
  hist(data$Gender, breaks=2, col = "skyblue", main = "Histogram of Gender")
  hist(data$Impressions, col = "skyblue", main = "Histogram of Impressions")
  hist(data$Clicks, col="skyblue", main = "Histogram of Clicks")
  hist(data$Signed_In, breaks = 2, col="skyblue", main = "Histogram of Sign Ins")
  hist(as.integer(data$age_group), col = "skyblue", main = "Histogram of Age Group")
}

initHistograms(nytData)


################ Data of people who signed in ##############
signIn <- subset(nytData, Signed_In==1)
hist(signIn$Age, breaks=10, col = "skyblue")
hist(signIn$Impressions, col="skyblue")
hist(signIn$Gender, breaks = 2,col="skyblue")

ggplot(subset(signIn, Age <= 18), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age \u2264 18", fill = "Gender")
ggplot(subset(signIn, Age > 18 & Age <= 24), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age (18, 24]", fill = "Gender")
ggplot(subset(signIn, Age > 24 & Age <= 34), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age (24, 34]", fill = "Gender")
ggplot(subset(signIn, Age > 34 & Age <= 54), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age (34, 54]", fill = "Gender")
ggplot(subset(signIn, Age > 54 & Age <= 64), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age (54, 64]", fill = "Gender")
ggplot(subset(signIn, Age > 64), aes(x = Impressions, fill = genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age > 64", fill = "Gender")


ggplot(signIn, aes(x=Impressions, fill = age_group))+geom_histogram(binwidth = 1)+labs(title="Histogram of Impressions by Age Group", fill = "Age Group")
ggplot(signIn, aes(x=Impressions, fill = genderFactor))+geom_histogram(binwidth = 1)+labs(title="Histogram of Impressions by Gender", fill = "Gender")

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
subClicks <- subset(signIn, Clicks >0)
gen_click_counts <- table(subClicks$Gender, subClicks$Clicks)
barplot(gen_click_counts, col=c("red", "darkblue"), main = "Number of Clicks by Gender", xlab = "Number of Clicks", legend = c("Female", "Male"), beside=TRUE)

gen_imp_counts <- table(signIn$Gender, signIn$Impressions)
barplot(gen_imp_counts, col=c("red", "darkblue"), main = "Number of Impressions by Gender", xlab = "Number of Impressions", legend = c("Female", "Male"), beside = TRUE)


# histogram comparing impressions of sign in and no sign in
ggplot(nytData, aes(x=Impressions, fill=signInFactor))+geom_histogram(binwidth = 1)+labs(title="Impressions", subtitle="signed in users vs non signed in users", fill = "Signed In") # stacked on each other

#### new category (may show that more impressions will mean more clicks) ###
ggplot(subset(nytData, Clicks>0), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")
ggplot(subset(nytData, Clicks>1), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")
ggplot(subset(nytData, Clicks>2), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")
ggplot(subset(nytData, Clicks>3), aes(x=Clicks, fill = impcat))+geom_histogram(binwidth = 1)+labs(title="Histogram of Clicks",fill="# of Impressions")


############# Plots on multiple days ###############
dayNames <- c(1:31) # vector of the days used for graph label

clickList <- lapply(datalist, "[",,"Clicks") # create list of the clicks for every day
sumClick <- sapply(clickList,sum) # vector of sum of clicks per day

names(sumClick) <- dayNames # name the entries of the vector to the corresponding day
barplot(sumClick, col=c("cadetblue1", "cadetblue3"), main = "Number of Clicks per Day", xlab = "Day")

# clicks1 <- sum(nytData$Clicks)
# clicks2 <- sum(nytData2$Clicks)
# clicks3 <- sum(nytData3$Clicks)
# clicks4 <- sum(nytData4$Clicks)
# clicks5 <- sum(nytData5$Clicks)
# clicks6 <- sum(nytData6$Clicks)
# clicks7 <- sum(nytData7$Clicks)
# 
# totalClicks <- c(clicks1, clicks2, clicks3, clicks4, clicks5, clicks6, clicks7)
# names(totalClicks) <- dayNames
# barplot(totalClicks, col=c("lightgreen", "lightcoral"), main = "Number of Clicks Per Day", xlab = "Day")


impList <- lapply(datalist, "[",,"Impressions")
sumImp <- sapply(impList, sum)

names(sumImp) <- dayNames
barplot(sumImp, col=c("cadetblue1", "cadetblue3"), main = "Number of Impressions per Day", xlab = "Day")


# imp1 <- sum(nytData$Impressions)
# imp2 <- sum(nytData2$Impressions)
# imp3 <- sum(nytData3$Impressions)
# imp4 <- sum(nytData4$Impressions)
# imp5 <- sum(nytData5$Impressions)
# imp6 <- sum(nytData6$Impressions)
# imp7 <- sum(nytData7$Impressions)
# 
# totalImp <- c(imp1, imp2, imp3, imp4, imp5, imp6,imp7)
# names(totalImp) <- dayNames
# barplot(totalImp, col=c("lightgreen", "lightcoral"), main = "Number of Impressions Per Day", xlab = "Day")


signInList <- lapply(datalist, "[",,"Signed_In")
sumSignIn <- sapply(signInList, sum)

names(sumSignIn) <- dayNames
barplot(sumSignIn, col=c("cadetblue1", "cadetblue3"), main = "Number of Sign Ins per Day", xlab = "Day")


# sign1 <- sum(nytData$Signed_In)
# sign2 <- sum(nytData2$Signed_In)
# sign3 <- sum(nytData3$Signed_In)
# sign4 <- sum(nytData4$Signed_In)
# sign5 <- sum(nytData5$Signed_In)
# sign6 <- sum(nytData6$Signed_In)
# sign7 <- sum(nytData7$Signed_In)
# 
# totalSign <- c(sign1, sign2, sign3, sign4, sign5, sign6, sign7)
# names(totalSign) <- dayNames
# barplot(totalSign, col=c("lightgreen", "lightcoral"), main = "Number of Sign Ins Per Day", xlab = "Day")

# Plot the total number of people per day
totalVisits <- sapply(datalist, nrow)
names(totalVisits) <- dayNames
barplot(totalVisits, col=c("cadetblue1", "cadetblue3"), main = "Number of Visitors Per Day", xlab = "Day")


# totalVisits <- c(nrow(nytData),nrow(nytData2),nrow(nytData3),nrow(nytData4),nrow(nytData5),nrow(nytData6),nrow(nytData7))
# names(totalVisits) <- dayNames
# barplot(totalVisits, col=c("lightgreen", "lightcoral"), main = "Number of Visitors Per Day", xlab = "Day")

# Plot CTR for each day
totalCTR <- sumClick/sumImp
names(totalCTR) <- dayNames
barplot(totalCTR, col=c("cadetblue1", "cadetblue3"), main = "Click Through Rate Per Day", xlab = "Day")


# totalCTR <- c(clicks1/imp1,clicks2/imp2,clicks3/imp3,clicks4/imp4,clicks5/imp5,clicks6/imp6,clicks7/imp7)
# names(totalCTR) <- dayNames
# barplot(totalCTR, col=c("lightgreen", "lightcoral"), main = "Click Through Rate Per Day", xlab = "Day")
