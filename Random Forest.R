#Changing working dictionary path
setwd("C:/Users/Lenovo/Desktop/NCI/Semester 1/Data Mining and Machine Learning 1/Project")

#reading data from CSV file
Data <- read.csv("Poverty Data.csv", header=T)

#checking for NA values
sapply(Data, function(x) sum(is.na(x)))

#remove na
Data <- Data[complete.cases(Data),] 

#removing unwanted columns
Data <- Data[,-6:-16]

#checking for structure of data
str(Data)

#converting continuous data into categorical data
Data$Poverty <- cut(Data$Poverty, breaks = c(min(Data$Poverty),16.04,max(Data$Poverty)), labels = c("Low", "High"))

#splitting data into training data and testing data
library(caTools)
split = sample.split(Data$Poverty, SplitRatio = 0.60)
Train <- subset(Data, split == TRUE)
Test <- subset(Data, split == FALSE)

sapply(Train, function(x) sum(is.na(x)))

#Applying random forest model on training data
library("randomForest")
modelRandomForest = randomForest(Poverty~., data = Train, ntree = 50, do.trace = T, importance = T)
modelRandomForest

plot(modelRandomForest)
#prediction using test data
predictTrain <- predict(modelRandomForest, newdata = Test, type = "response")

#creating confusiong matrix
library(caret)
caret::confusionMatrix(predictTrain, Test$Poverty)

varImpPlot(modelRandomForest)


