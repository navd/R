# Assignment 8 Tidy Data, Navdeep Sharma
library(xlsx)
library(ggplot2)
library(reshape2)
library(stringr)
library(zoo)
library(plyr)
library(Hmisc)


rawData <- read.xlsx("C:\\Users\\hp\\Documents\\A8\\CSO Property Assignment.xlsx", 1 , header = TRUE)
meltedData <- melt(rawData,id.vars = "Year.Month")

r1 <- strsplit(as.character(meltedData$variable), split = "...", fixed = TRUE)  
matx <- matrix(unlist(r1),ncol = 2, byrow = TRUE)
dtframe1 <- data.frame(matx)                          
names(dtframe1) = c("Area","Property")               
meltedData$variable <- NULL                     
meltedData <- cbind(meltedData, dtframe1)            

r2 <- strsplit(as.character(meltedData$Year.Month), split = "M", fixed = TRUE)
maty <- matrix(unlist(r2),ncol = 2, byrow = TRUE)
dtframe2 <- data.frame(maty)                          
names(dtframe2) = c("Year","Month")                  
meltedData$Year.Month <- NULL                   
meltedData <- cbind(meltedData, dtframe2)            

meltedData$Area <- sub("National.excluding.Dublin","Ex-Dublin",meltedData$Area, fixed = TRUE)
meltedData$Property <- sub("all.residential.properties","All Residential",
                           meltedData$Property, fixed = TRUE)


proData <- ddply(meltedData, .(Property, Year), summarise, avgValue = mean(value))
qplot(x=Year, y = avgValue, fill = Property, data = proData[proData$Property == "All Residential",], geom = "bar", stat = "identity")
qplot(x=Year,y=avgValue, fill = Property, data = proData[proData$Property == "apartments",], geom = "bar",stat = "identity")
qplot(x=Year, y = avgValue, fill = Property, data = proData[proData$Property == "houses",], geom = "bar", stat = "identity")

qplot(x=Year, y = avgValue, fill = Property, data = proData, geom = "bar", stat = "identity")

# Converting the year and month column into a date column
meltedData$Date <- as.Date(paste(mdata$Month,mdata$Year, "01", sep = "-"),format = "%m-%Y-%d")

summary <- ddply(meltedData, .(Area,Property), summarise, Min = min(value), Max = max(value), 
                 Average = mean(value), Datemin = Date[which.min(value)], Datemax = Date[which.max(value)])

summary$Datemin <- format(summary$Datemin,"%m-%Y")   
summary$Datemax <- format(summary$Datemax,"%m-%Y")   
summary$Property<-gsub("\\."," ",summary$Property)
summary$Property<-gsub("properties"," ",summary$Property)
summary<-arrange(summary,Property,desc(Area))
print(summary)