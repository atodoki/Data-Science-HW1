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
plot(mvector, col=c("red"),type="l", main="Number Of Logins Per Day (Men=Red, Women=Blue")
lines(wvector, col=c("blue"))
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
# ... and try using lapply and sapply
impList <- lapply(datalist, '[', "Impressions")
sumImp <- sapply(impList, sum)

# more stuff (copied from Ariel)
totalVisits <- sapply(datalist, nrow)
names(totalVisits) <- dayNames
barplot(totalVisits, col=c("green"), main = "Number of Visitors Per Day", xlab = "Day")

# let's do ctr by gender over the whole period
# begin by building a lists of tables
malelist <- list()
femlist <- list()
for (i in 1:31){
  # separate each table by gender using already-build signinlist
  malepoint <- subset(signinlist[[i]], Gender==1)
  fempoint <- subset(signinlist[[i]], Gender==0)
  malelist[[i]] <- malepoint
  femlist[[i]]<- fempoint
}
# now let's do total click counts by gender
mClkList <- lapply(malelist, '[', "Clicks")
sumMClk <- sapply(mClkList, sum)
fClkList <- lapply(femlist, '[', "Clicks")
sumFClk <- sapply(fClkList, sum)
clickCounts <- c(sum(sumMClk), sum(sumFClk))
barplot(clickCounts, col=c("red", "blue"), main = "Total Clicks by Gender, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

# total impression counts by gender
mImpList <- lapply(malelist, '[', "Impressions")
sumMImp <- sapply(mImpList, sum)
fImpList <- lapply(femlist, '[', "Impressions")
sumFImp <- sapply(fImpList, sum)
impCounts <- c(sum(sumMImp), sum(sumFImp))
barplot(impCounts, col=c("red", "blue"), main = "Total Impressions by Gender, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

# ctr by gender
ctrCounts <- clickCounts/impCounts
barplot(ctrCounts, col=c("red", "blue"), main = "Total Clicks/Impression by Gender, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

# ariel discovered that there was an interesting pattern in >64 men vs women;
# let's investigate age groups
for (i in 1:31){
  # let's simplify age groups
  signinlist[[i]]$Age_Cat <- cut(signinlist[[i]]$Age, c(-Inf, 20,40,60,Inf))
}
# let's try and graph impressions by age group
youngAgeList <- list()
lowAgeList <- list()
midAgeList <- list()
oldAgeList <- list()
for (i in 1:31){
  youngAgePoint <- subset(signinlist[[i]], Age_Cat=='(-Inf,20]')
  lowAgePoint <- subset(signinlist[[i]], Age_Cat=='(20,40]')
  midAgePoint <- subset(signinlist[[i]], Age_Cat=='(40,60]')
  oldAgePoint <- subset(signinlist[[i]], Age_Cat=='(60, Inf]')
  youngAgeList[[i]] <- youngAgePoint
  lowAgeList[[i]] <- lowAgePoint
  midAgeList[[i]] <- midAgePoint
  oldAgeList[[i]] <- oldAgePoint
}

youngImps <- lapply(youngAgeList, '[', "Impressions")
lowImps <- lapply(lowAgeList, '[', "Impressions")
midImps <- lapply(midAgeList, '[', "Impressions")
oldImps <- lapply(oldAgeList, '[', "Impressions")
youngImps_sum <- sapply(youngImps, sum)
lowImps_sum <- sapply(lowImps, sum)
midImps_sum <- sapply(midImps, sum)
oldImps_sum <- sapply(oldImps, sum)
ageImps <- c(sum(youngImps_sum), sum(lowImps_sum), sum(midImps_sum), sum(oldImps_sum))
barplot(ageImps, col= c('cadetblue1', 'cadetblue2', 'cadetblue3','cadetblue4'), main = "Total Impressions by Age Group", ylab='Impressions')

# let's focus in and investigate age 60+
old_male <- lapply(oldAgeList, subset, Gender==1)
old_fem <- lapply(oldAgeList, subset, Gender==0)
oldImps_male <- lapply(old_male, '[', "Impressions")
oldImps_fem <- lapply(old_fem, '[', "Impressions")
oim_sum <- sapply(oldImps_male, sum)
oif_sum <- sapply(oldImps_fem, sum)
oldImps_gen <- c(sum(oim_sum), sum(oif_sum))
barplot(oldImps_gen, col=c('blue', 'red'), main='Impressions by Gender, Age 60+', legend=c('Male', 'Female'))

# WARNING: experimental stuff ahead
# concatenate all of our dataframes in a list into one MASSIVE dataframe
# allOldMales <- Reduce(rbind, old_male)
# the above command works, now let's try it with a larger dataset: all old people
allOld <- Reduce(rbind, oldAgeList)
# now that we have a dataframe with ALL data, let's use ggplot!
# NOTE: gender must be made a factor to differentiate fill colors!
allOld$genderFactor[allOld$Gender==1] <- "Male"
allOld$genderFactor[allOld$Gender==0] <- "Female"
allOld$genderFactor <- factor(allOld$genderFactor)
ggplot(allOld, aes(x=Impressions, fill=genderFactor))+geom_histogram(binwidth = 1) + labs(title="Age > 60", fill = "Gender")
