##Assignment 4- submitted by - Navdeep Sharma
ages<-c(25,26,55,37,21,42)
affils<-c("R","D","D","R","U","D")
gender<-c("M","M","F","F","F","M")
#Declaring my_tapply function
my_tapply<-function(X,INDEX,FUN=NULL){
  if(is.null(INDEX)|| is.null(X) || is.null(FUN)){#Checking for Invalid arguments
    print("Invalid Arguments Provided")
  }else{
    if(!is.list(INDEX)){# If there is only one factor
      m1<-sapply((split(X,INDEX)),FUN)# using split function to group data based on factors and applying passed function
    }
    else{# If there are two factores combined in a list
      m1<-matrix(sapply((split(X,INDEX)),FUN),nrow=3,ncol=2) # using split function to group data based on factors and applying passed function FUN
      colnames(m1)<-c("F","M") #Naming columns
      rownames(m1)<-c("D","R","U") # Naming Rows
      m1[is.nan(m1[,])]<-NA #Replacing NAN with NA
    }
    m1 # returning last computed value
  }
}

tapply(ages,affils,mean) #Calling predefined tapply for one factor
tapply(ages,list(affils,gender),mean) #Calling predefined tapply for two factors
my_tapply(ages,affils,mean)#Calling userdefined my_tapply for one factor
my_tapply(ages,list(affils,gender),mean) #Calling userdefined my_tapply for two factors