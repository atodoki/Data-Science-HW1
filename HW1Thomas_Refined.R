# This is a more refined compilation of what I've done -- specifically with datalist
# I've begun by applying techniques I learned later to streamline work
# Begin by loading and installing libraries
# install.packages(ggplot2)
# install.packages(doBy)
library(ggplot2)
library(doBy)

# now use a for loop to load in all the csv files
datalist <- list()
for (i in 1:31){
  datapoint <- read.csv(paste0("C:/Users/tvtuttle/Desktop/Willamette Work/Junior/2018 Spring/CS 429/Homework 1/HW1_data/nyt",toString(i),".csv"))
  datalist[[i]]<-datapoint
}
# datapoint is a waste of space, so let's remove it
rm(datapoint)
# When inspecting the data, there were 2 main methods I used to get info from the datalist
# 1. Using for loops/lapply (later on) to create lists of subsets, to perform operations 
#   on data of smaller size
# 2. Using Reduce(rbind, listname) to combine the list (or lists of subsets) into dataframes
#   NOTE: This might not work with large lists; I haven't tried it with datalist
#   but am 90% sure it fails. It's mainly for lists of subsets

# let's begin by using a for loop to define useful things in each list item before messing
# we use a for loop because i don't know how to do variable assignment with lapply
for (i in 1:31){
  # since we plan on compiling data into one large dataframe, let's set a "day" variable
  datalist[[i]]$Day <- i
  # and let's create some useful factors: age cat, gender, sign_in, day, basically everything lol
  datalist[[i]]$AgeCat <- cut(datalist[[i]]$Age, c(-Inf, 18, 24, 34, 54, 64, Inf))
  datalist[[i]]$AgeCat <- factor(datalist[[i]]$AgeCat)
  
  datalist[[i]]$Gender[datalist[[i]]$Gender==0 & datalist[[i]]$Signed_In==1] <- "Female"
  datalist[[i]]$Gender[datalist[[i]]$Gender==1] <- "Male"
  datalist[[i]]$Gender[datalist[[i]]$Signed_In==0] <- "Undefined"
  datalist[[i]]$Gender <- factor(datalist[[i]]$Gender)
  
  datalist[[i]]$Signed_In[datalist[[i]]$Signed_In==0] <- "No"
  datalist[[i]]$Signed_In[datalist[[i]]$Signed_In==1] <- "Yes"
  datalist[[i]]$Signed_In <- factor(datalist[[i]]$Signed_In)
  
  datalist[[i]]$ImpCat <- cut(datalist[[i]]$Impressions, c(-Inf,5,10,15,20,Inf))
  datalist[[i]]$ImpCat <- factor(datalist[[i]]$ImpCat)
  
  datalist[[i]]$ClickCat[datalist[[i]]$Clicks <= 0] <- "Non-Clicker"
  datalist[[i]]$ClickCat[datalist[[i]]$Clicks > 0] <- "Clicker"
  datalist[[i]]$ClickCat <- factor(datalist[[i]]$ClickCat)
}

# Now that our data has been sufficiently factored, let's produce some subsets
# The reason for this is helping with time constraints when using specific users
# (mainly, signed-in users, since only they have age and gender data)
signinlist <- lapply(datalist, subset, Signed_In=="Yes")

# While we're at it, how about we create a list of clickers too? They're the target demographic
clickerlist <- lapply(datalist, subset, ClickCat=="Clicker")
# note that clickerlist is about 1/10th the size of datalist
# since it's so small, it's a good candidate for concatenation
# but before we do that, let's get an even smaller (but important) list...
# of identifiable clickers!
signinclickerlist <- lapply(clickerlist, subset, Signed_In=="Yes")

# since this newest list is our smallest yet, let's concatenate it!
signinclickerframe <- Reduce(rbind, signinclickerlist)

# let's create some graphs!
# i began with simple bar graphs, with data from vectors extracted from the lists
clickvector <- c(1:31)
impvector <- c(1:31)
ctrvector <- c(1:31)
for (i in 1:31){
  clickvector[i]<-sum(clickerlist[[i]]$Clicks) # because all clickers are in there and its smaller
  impvector[i]<-sum(datalist[[i]]$Impressions)
  ctrvector[i]<-clickvector[i]/impvector[i]
}
barplot(clickvector, col=c('cadetblue1', 'cadetblue3'), main = "Number of Clicks Per Day (All Users)", xlab = "Days", ylab="Clicks")
barplot(impvector, col=c('cadetblue1', 'cadetblue3'), main = "Number of Impressions Per Day (All Users)", xlab = "Days", ylab="Impressions")
barplot(ctrvector, col=c('cadetblue1', 'cadetblue3'), main="Clicks Per Impressions Per Day (All Users)", xlab="Days",ylab="Clicks/Impressions")

# the following block is from ariel's code
dayNames <- c(1:31)
totalVisits <- sapply(datalist, nrow)
names(totalVisits) <- dayNames
barplot(totalVisits, col=c("green"), main = "Number of Visitors Per Day", xlab = "Day")

# female vs male logins
mvector <- c(1:31)
wvector <- c(1:31)
for (i in 1:31){
  mvector[i] <- sum(signinlist[[i]]$Gender=="Male")
  wvector[i] <- sum(signinlist[[i]]$Gender=="Female")
}
barplot(mvector, col=c("blue"), main = "Number of (Signed-In) Male Logins Per Day")
barplot(wvector, col=c("red"), main = "Number of (Signed-In) Female Logins Per Day")
# now with line plots!
plot(mvector, col=c("blue"),type="l", main="Number Of Logins Per Day (Men=Red, Women=Blue")
lines(wvector, col=c("red"))

# now we begin investigating multiple variables at once! (before using ggplot)
# how gender relates with clicking (among clickers and all signedin)
# how many men/women are clickers vs total men/women

malelist <- lapply(signinlist, subset, Gender=="Male")
femlist <- lapply(signinlist, subset, Gender=="Female")

maleclicklist <- lapply(clickerlist, subset, Gender=="Male")
femclicklist <- lapply(clickerlist, subset, Gender=="Female")

mClkList <- lapply(maleclicklist, '[', "Clicks")
sumMClk <- sapply(mClkList, sum)
fClkList <- lapply(femclicklist, '[', "Clicks")
sumFClk <- sapply(fClkList, sum)
clickCounts <- c(sum(sumMClk), sum(sumFClk))
barplot(clickCounts, col=c("red", "blue"), main = "Total Clicks by Gender, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

mImpList <- lapply(malelist, '[', "Impressions")
sumMImp <- sapply(mImpList, sum)
fImpList <- lapply(femlist, '[', "Impressions")
sumFImp <- sapply(fImpList, sum)
impCounts <- c(sum(sumMImp), sum(sumFImp))
barplot(impCounts, col=c("red", "blue"), main = "Total Impressions by Gender, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

ctrCounts <- clickCounts/impCounts
barplot(ctrCounts, col=c("red", "blue"), main = "Total Clicks/Impressions by Gender, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

# now let's find ctr among clickers only
mImpList2 <- lapply(maleclicklist, '[', "Impressions")
sumMImp2 <- sapply(mImpList2, sum)
fImpList2 <- lapply(femclicklist, '[', "Impressions")
sumFImp2 <- sapply(fImpList2, sum)
impCounts2 <- c(sum(sumMImp2), sum(sumFImp2))

ctrCounts2 <- clickCounts/impCounts2
barplot(ctrCounts2, col=c("red", "blue"), main = "Total Clicks/Impressions by Gender For Clickers, May 2012", ylab="Number of Clicks", legend=c("Male", "Female"))

# finally, let's do some ggplots with our concatenated dataframe, signinclickerframe
ggplot(signinclickerframe, aes(x=Impressions, fill=Gender))+geom_histogram(binwidth = 1) + labs(title="Impressions by Gender", fill = "Gender")
ggplot(signinclickerframe, aes(x=Impressions, fill=AgeCat))+geom_histogram(binwidth = 1) + labs(title="Impressions by Gender", fill = "Gender")

# earlier, we noticed interesting pattern differences between old men/women and young men/women
# but that was only on a single day, let's compare them over all days total!
# for that, we'll need to try to concatenate a very large list: signinlist
# signinframe <- Reduce(rbind, signinlist) takes too long; let's break it down first
oldlist <- lapply(signinlist, subset, AgeCat=='(64, Inf]')
younglist <- lapply(signinlist, subset, AgeCat=='(-Inf,18]')
# now concatenate each of these subsets to get frames we can pass to ggplot
oldframe <- Reduce(rbind, oldlist)
youngframe <- Reduce(rbind, younglist)
ggplot(oldframe, aes(x = Impressions, fill = Gender))+geom_histogram(binwidth = 1) + labs(title="Age > 64", fill = "Gender")
ggplot(youngframe, aes(x=Impressions, fill=Gender))+geom_histogram(binwidth = 1)+labs(title="Age < 18", fill = "Gender")