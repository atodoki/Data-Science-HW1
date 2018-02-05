datalist <- list()
for (i in 1:31){
  datapoint <- read.csv(paste0("C:/Users/tvtuttle/Desktop/Willamette Work/Junior/2018 Spring/CS 429/Homework 1/HW1_data/nyt",toString(i),".csv"))
  datalist[[i]]<-datapoint 
}