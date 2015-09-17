## Programming for Data Analytics - Assignment 02
## Submitted by - Navdeep Sharma (15231076)
## Date - 17/09/2015

process.vectors<-function(...){lapply(list(...),function(x){list(data=x,min=min(x),max=max(x),mean=mean(x),sd=sd(x))})}
ans<-process.vectors(c(1,2,3),34:78,c(8,9,10))
str(ans)