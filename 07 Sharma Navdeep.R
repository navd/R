my_environment<- function(){
  # For listing variables
  v.size <- sapply(ls(.GlobalEnv), function(y){object.size(get(y))})  #getting size of all variables from global environment
  v.type<- sapply(ls(.GlobalEnv), function(x){typeof(get(x))}) #extracting type of each variable
  df1 <- data.frame(v.type,v.size) # creating data frame with size and type
  sorted.value <- df1[with(df1,order(-v.size,v.type)),] #sorting the data frame
  sorted.value$Variable_Names <- row.names(sorted.value)
  row.names(sorted.value) <- NULL
  variable.list<-sorted.value[c(3,1,2)] # reordering the columns
  #For listing funtions
  func<-variable.list[grepl("closure",variable.list$v.type),"Variable_Names"]
  func_name<-as.character(func)
  params<-sapply(func_name,function(x){as.list(formals(get(x)))}) #getting parameters as a list
  code <- sapply(func_name,function(x)body(get(x))) # getting the code of function
  size <- sapply(func_name,function(x)object.size(get(x))) # getting the size of the object
  matrix.value<-cbind(params,code,size)
  matrix.value[,"params"]<-as.character(matrix.value[,"params"]) #converting parameter value as char
  func.df <- data.frame(matrix.value)#converting matrix to data frame
  func.df$Funtion_name <- row.names(func.df)#getting row names as function names
  row.names(func.df) <- NULL#setting row names to null
  func.df <-func.df[c(4,1,2,3)]# reordering columns
  
  func.df<-func.df[ order(func.df[,1]), ]# sorting the data frame based on function names.
  names(func.df) <- c("Function Names", "parameters", "Code", "Size (byte)")#Naming the columns
  print(variable.list)
  print(func.df)
  list(variable.list,func.df)#returning list of 2 data frames
}
# Calling my environment function:
all.values<-my_environment()
all.values[[1]]
all.values[[2]]