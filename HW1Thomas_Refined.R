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