library(mlr)
library(mlbench)

# defining the task
task = makeClassifTask(data = illness.data, target = "test_result")
print(task)

#build the leanrer
learnerKNN = makeLearner("classif.knn")
learnerRF = makeLearner("classif.randomForest")

# 10 fold cross validation
cv10f <- makeResampleDesc("CV", iters = 10)

# various type of measures like mmce(error), accuracy plus tp(true positive), fp, tn, tp
measures = list(mmce, acc)
resample(learnerKNN, task, cv10f, measures)$aggr  # resampling 


data("raw.data",package = "mlbench")
learners = list(learnerKNN,learnerRF)       # we can pass list of learner and also one learner
benchmark(learners, task, cv10f, mmce)
benchmark(learners, task, cv10f, acc)
#plotLearnerPrediction(learnerKNN, task,features = c("bp","test_result"))
#plotLearnerPrediction(learnerKNN, task,features = c("bp","age"))
#plotLearnerPrediction(lrnsvm, task,features = c("bp","age"))
#plotLearnerPrediction(lrnsvm, task,features = c("bmi","age"))
#plotLearnerPrediction(learnerKNN, task,features = c("bmi","age"))

# will plot a learning curve(accuaracy & error)Vs training data size
set.seed(335)
learningCurve<-generateLearningCurveData(list("classif.knn","classif.randomForest"), task, 
                              percs = seq(0.1,1,by=0.1),
                              measures = list(acc,mmce), resampling = makeResampleDesc("CV", iters = 10),
                              show.info = TRUE)
plotLearningCurve(learningCurve)

learningCurve<-generateLearningCurveData("classif.randomForest", task, 
                              percs = seq(0.1,1,by=0.1),
                              measures = list(acc,mmce), resampling = makeResampleDesc("CV", iters = 10),
                              show.info = TRUE)

plotLearningCurve(learningCurve)

# not needed 
getParamSet(learnerKNN)
getParamSet(lrnsvm)
print(mmce)
print(timetrain)