# Assignment 09- Navdeep Sharma
# Programming for Data Analytics


#Make generic function
rinfo<-function(X){# This is the "constructor" function...
  UseMethod("rinfo")# ... UseMethod should have the name of the function!
}

#writing rinfo function for matrix class
rinfo.matrix<-function(X){
  cat("Processing Matrix \n\n")
  rsum<-rowSums(X)
  rmax<-apply(X, 1, max)
  rmin<-apply(X, 1, min)
  rmean<-apply(X, 1, mean)
  rsd<-apply(X, 1, sd)
  rmedian<-apply(X, 1, median)
  X<-cbind(X,rsum)
  X<-cbind(X,rmax)
  X<-cbind(X,rmin)
  X<-cbind(X,rmean)
  X<-cbind(X,rsd)
  X<-cbind(X,rmedian)
  X
}

#writing rinfo function for data.frame class
rinfo.data.frame<-function(X){
  cat("Processing Data frame \n\n")
  rsum<-rowSums(X)
  rmax<-apply(X, 1, max)
  rmin<-apply(X, 1, min)
  rmean<-apply(X, 1, mean)
  rsd<-apply(X, 1, sd)
  rmedian<-apply(X, 1, median)
  X<-cbind(X,rsum)
  X<-cbind(X,rmax)
  X<-cbind(X,rmin)
  X<-cbind(X,rmean)
  X<-cbind(X,rsd)
  X<-cbind(X,rmedian)
  X
}

m1<-matrix(1:20,nrow=5)
d1<-data.frame(matrix(1:20,nrow=5))
rinfo(m1)
rinfo(d1)
