library(gdata)
library("splitstackshape")
library("BradleyTerry2")
library("ISLR")
library(caret)
mydata=data.frame(read.table("illness.txt"))
data=t(mydata)
colnames(data)<-c("plasma_glucose","bp", "test_result","skin_thickness","num_pregnancies","insulin", "bmi", "pedigree", "age")
my_data<-cSplit(data, c("plasma_glucose","bp", "test_result","skin_thickness","num_pregnancies","insulin", "bmi", "pedigree", "age"), ",", direction = "long")
raw.data<- my_data

#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = raw.data$test_result,p = 0.75,list = FALSE)
training <- raw.data[indxTrain,]
testing <- raw.data[-indxTrain,]
dim(training)
dim(testing)

#Checking distibution in origanl data and partitioned data
prop.table(table(training$test_result)) * 100

prop.table(table(testing$test_result)) * 100

prop.table(table(raw.data$test_result)) * 100

trainX <- training[,names(training) != "test_result"]
knnFit <- train(test_result ~ ., data = training, method = "knn",preProcess = "scale", tuneLength = 15)
#knnFit <- train(test_result ~ ., data = training, method = "knn",preProcess = "scale", trControl = trainControl(method = "cv"), tuneLength = 30)# code for performing 10 fold cross validation
plot(knnFit)
knnPredict <- predict(knnFit,newdata = testing )
confusionMatrix(knnPredict, testing$test_result )
mean(knnPredict == testing$test_result)
