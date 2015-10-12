library(gdata)
library("splitstackshape")
library("BradleyTerry2")
library("ISLR")
library(caret)
library(ggplot2)
library("randomForest")
mydata=data.frame(read.table("illness.txt"))
data=t(mydata)
colnames(data)<-c("plasma_glucose","bp", "test_result","skin_thickness","num_pregnancies","insulin", "bmi", "pedigree", "age")
my_data<-cSplit(data, c("plasma_glucose","bp", "test_result","skin_thickness","num_pregnancies","insulin", "bmi", "pedigree", "age"), ",", direction = "long")
raw.data<- my_data
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = raw.data$test_result,p = 0.75,list = FALSE)
training <- raw.data[indxTrain,]
testing <- raw.data[-indxTrain,]
rfFit <- train(test_result ~ ., data = training, method = "rf", prox= TRUE, allowParallel=TRUE, tuneLength = 5)
#rfFit <- train(test_result ~ ., data = training, method = "rf", prox= TRUE, allowParallel=TRUE, trControl=trainControl(method="cv", repeats=3), tuneLength = 5) ## To perform 10 fold cross validation

getTree(rfFit$finalModel,k=5)
plot(rfFit)
training<-as.data.frame.matrix(training) 
illnessP<- classCenter(training[,c(1,2)],training$test_result,rfFit$finalModel$prox);
illnessP<-as.data.frame(illnessP);
illnessP$test_result<-rownames(illnessP)
p<- qplot(plasma_glucose,bp, col=test_result, data=training)
p + geom_point(aes(x=plasma_glucose,y=bp, col=test_result), size=5, shape=4, data=training)

pred<-predict(rfFit,testing);testing$predRight<-pred==testing$test_result;
pred
table(pred,testing$test_result)
mean(pred==testing$test_result)
qplot(plasma_glucose,bp,col=testing$predRight, data = testing, main="New data Prediction")