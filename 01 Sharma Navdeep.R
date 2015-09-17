## Programming for Data Analytics - Assignment 01
## Submitted by - Navdeep Sharma (15231076)
## Date - 11/09/2015

set.seed(111)
x<-rnorm(n=101,mean=72,sd=8)

## Calculating the mean
mean<-sum(x)/length(x)
cat("The mean of the Vector is ", mean, "\n\n\n")


## Calculating standard deviation

y<- x - mean
sd=sqrt((sum(y^2))/(length(y)-1))
cat("The Standard Deviation of the Vector is ", sd, "\n\n\n")


## calculating the range 

cat("The range of the vector from ",min(x)," to ", max(x), "\n\n\n")

## Calculating the median
sortedX=sort(x)
index<-ifelse(length(x)%%2==0,length(x)/2,(length(x)+1)/2)
cat("The median of the vector is ", sortedX[index], "\n\n\n")


## vector with values less than

lessThanMean<-x[x<mean]
cat("Vector	with	all	values	less	than	the	mean (",mean,") is \n ", lessThanMean,"\n\n\n" )

## vector with values greater and equal to mean

greaterThanMean<-x[x>=mean]
cat("Vector	with	all	values greater and equal to	the	mean (",mean,") is \n ", greaterThanMean,"\n\n\n" )

## top 5 values from the vector

topValues<-tail(sortedX,5)
cat("Top 5 values from the Vector x are \n", topValues,"\n\n\n")

## Bottom 5 values from the vector

bottomValues<-head(sortedX,5)
cat("Top 5 values from the Vector x are \n", bottomValues,"\n\n")


## Calculating absolute difference between vector

absVector<-x-mean
absVector<- ifelse(absVector<0,absVector*-1,absVector)
cat("The absolute	difference	from	the	mean	for	each	value	\n", absVector, "\n\n")
