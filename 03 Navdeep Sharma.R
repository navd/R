##Assignment 3- submitted by - Navdeep Sharma
library(gdata)
library(reshape)
library(ggplot2)
library(xlsx)
library(sp)
library(scales)
path<-"assignment3\\data.xlsx"
male<-read.xlsx(path,1,header = TRUE)
female<-read.xlsx(path,2,header = TRUE)
Leinster <- c("Kildare","Dublin","Louth","Wexford","Longford","Meath","Westmeath","Carlow","Laois","Offaly","Kilkenny","Wicklow")
Munster <- c("Clare","Cork","Kerry","Limerick","North.Tipperary","South.Tipperary","Waterford")
Connacht <- c("Galway","Leitrim","Mayo","Roscommon","Sligo")
Ulster <- c("Cavan","Donegal","Monaghan")
new_male<-melt(male,id.vars = "Year")
new_female<-melt(female,id.vars = "Year")
names(new_male)<-c("Year", "County","Population")
names(new_female)<-c("Year", "County","Population")
new_male$Gender<- "Male"
new_female$Gender<- "Female"
plotData<-rbind(new_male,new_female)
plotData$Province<-factor(ifelse(plotData$County %in% Leinster,"Leinster",ifelse(plotData$County %in% Munster,"Munster",ifelse(plotData$County %in% Connacht,"Connacht",ifelse(plotData$County %in% Ulster,"Ulster",NA)))))
aggProvince<-aggregate(plotData$Population,by = list(Province=plotData$Province,Year=plotData$Year), FUN=sum)
qplot(data = aggProvince, x=Year, y=x, color=Province, ymax=max(agg$x)) + geom_line(position=position_dodge(1),aes(group=Province)) +scale_y_continuous("Population")
aggGender<-aggregate(plotData$Population,by = list(Gender=plotData$Gender,Year=plotData$Year), FUN=sum)
qplot(data = aggGender, x=Year, y=x, color=Gender, ymax=max(agg$x)) + geom_line(position=position_dodge(1),aes(group=Gender)) +scale_y_continuous("Population")




