# project
Practical Machine Learning
#Loading Libraries 
library(caret)
library(randomForest)
library(rpart)
#Import training and testing data. Identify “”, “NA” and “#DIV/0!” as “NA” 
url.train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(url.train), na.strings = c("NA", "", "#DIV0!"))
testing <- read.csv(url(url.test), na.strings = c("NA", "", "#DIV0!"))
#define the same columns
sameColumsName <- colnames(training) == colnames(testing)
colnames(training)[sameColumsName==FALSE]
#Cleaning data
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]
#Removing first 7 columns. They are not relevant to our project
training <- training[,8:dim(training)[2]]
testing <- testing[,8:dim(testing)[2]]
#Data Slicing
set.seed(12345)
inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
trainingCV <- training[inTrain,]
testingCV <- training[-inTrain,]
dim(trainingCV)
dim(testingCV)
#Apply Classification Tree model
modelCTree <- rpart(classe ~ ., data=trainingCV, method="class")
predictionCTree <- predict(modelCTree, testingCV, type="class")
CTree <- confusionMatrix(predictionCTree, testingCV$classe)
CTree
library(rpart.plot)
rpart.plot(modelCTree)
#Apply Random forest model
modelRF <- randomForest(classe ~ ., data=trainingCV, method="class")
predictionRF <- predict(modelRF, testingCV, type="class")
RF <- confusionMatrix(predictionRF, testingCV$classe)
RF
CV <- testingCV
CV$GOODpred <- testingCV$classe == predictionRF
qplot(accel_forearm_x, accel_forearm_y, col=GOODpred, data=CV)
#Final Prediction
FinalPrediction <- predict(modelRF, testing)
kable(t(data.frame(FinalPrediction)))
