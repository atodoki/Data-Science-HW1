# CS 429 Lab 1
# Using samplecode as basis
# load packages (install if needed)
library("doBy")
library(ggplot2)
# Read in 1 file
data1 <- read.csv(file="C:/Users/tvtuttle/Desktop/Willamette Work/Junior/2018 Spring/CS 429/Homework 1/HW1_data/nyt1.csv", header=TRUE, sep=",")
# create age categories
data1$age_group <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
summary(data1)
# brackets
siterange <- function(x){c(length(x),min(x), mean(x), max(x))}
summaryBy(Age~age_group, data = data1, FUN=siterange)
# make sure only signed-in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group, data = data1)

# plot impressions vs age group using ggplot (histogram and boxplot)
ggplot(data1, aes(x=Impressions, fill=age_group))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=age_group, y=Impressions, fill=age_group))+geom_boxplot()

# create click-thru rate
# cut away points with no impressions (no impression = no care)
# if there are clicks with no impressions, not my fault
data1$hasimps <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data = data1, FUN=siterange)
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions,colour=age_group)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions, colour=age_group)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=age_group, y=Clicks, fill=age_group)) + geom_boxplot()
ggplot(subset(data1, Clicks>0), aes(x=Clicks, colour=age_group)) + geom_density()

# create more categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions > 0] <- "Imps"
data1$scode[data1$Clicks > 0] <- "Clicks"

# convert the column into a factor (??????????????)
data1$scode <- factor(data1$scode)
head(data1)

# look at levels (also ????????????)
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+age_group, data = data1, FUN=clen)

#Book code ends here. This is where the "fun" begins.
#Define a new variable to segment or categorize users based on their click behavior.
data1$clickgroup <- cut(data1$Clicks,c(-Inf,0,1,Inf))
data1$cbehavior[data1$Clicks<=0] <- "NonClicker"
data1$cbehavior[data1$Clicks>0] <- "Clicker"
data1$cbehavior <- factor(data1$cbehavior)

# let's loop through using paste0 to assign data to variables
# install.packages("rlist")
# library(rlist)
datalist <- list()
for (i in 1:31){
  datapoint <- read.csv(paste0("C:/Users/tvtuttle/Desktop/Willamette Work/Junior/2018 Spring/CS 429/Homework 1/HW1_data/nyt",toString(i),".csv"))
  datalist[[i]]<-datapoint 
}

# let's begin some analysis withOUT using lapply
signinlist <- list()
mvector <- c(1:31)
wvector <- c(1:31)
for (i in 1:31){
  # let's begin by creating a subset of signed-in users only
  signinpoint <- subset(datalist[[i]], Signed_In==1)
  signinlist[[i]]<-signinpoint
  # now let's do a graph of gender vs time
  # let's fill a vector with the gender data for men and women (1=men, 0=women)
  mvector[i] <- sum(signinlist[[i]]$Gender==1)
  wvector[i] <- sum(signinlist[[i]]$Gender==0)
  # now display mvector and wvector as plots
}
barplot(mvector, col=c("red"), main = "Number of (Signed-In) Male Logins Per Day")
barplot(wvector, col=c("blue"), main = "Number of (Signed-In) Female Logins Per Day")

# let's try doing some daily analysis of all users
# total clicks and impressions per day
# total click thru rate per day
clickvector <- c(1:31)
impvector <- c(1:31)
ctrvector <- c(1:31)
for (i in 1:31){
  clickvector[i]<-sum(datalist[[i]]$Clicks)
  impvector[i]<-sum(datalist[[i]]$Impressions)
  ctrvector[i]<-clickvector[i]/impvector[i]
}
barplot(clickvector, col="blue", main = "Number of Clicks Per Day (All Users)", xlab = "Days", ylab="Clicks")
barplot(impvector, col="red", main = "Number of Impressions Per Day (All Users)", xlab = "Days", ylab="Impressions")
barplot(ctrvector, col="purple", main="Clicks Per Impressions Per Day (All Users)", xlab="Days",ylab="Clicks/Impressions")

# i'd argue that click-through rate is the most important statistic for advertisers;
# let's compare across demographics instead of time (for the ENTIRE MONTH)
