# Data set from  http://www.ics.uci.edu/~mlearn/MLRepository.html
library("kernlab")
library("stats")
data(spam)
#View(head(spam)) #show dataset
#plot data with x -axis with frequency of word 'your' appear in email and y-axis as density of word appears in all emails
plot(density(spam$your[spam$type=="nonspam"]), col="blue",main="",xlab="Frequency of word 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
#Now find a threshold value, if that exceeds we mark it as spam
#Here selecting threshold as 0.5
abline(v=0.5,col="black")
#build simple prediction model for threshold
prediction<-ifelse(spam$your>0.5,"spam","nonspam")
#generating confusion matrix spam and nonspam data prediction
table(prediction, spam$type)/length(spam$type)
