#set working dictionary
setwd("C:/Users/Lenovo/Desktop/NCI/Semester 1/Data Mining and Machine Learning 1/Project")

#extract data from CSV
Data <- read.csv("Poverty Data.csv", header=T)

#check for null values
sapply(Data, function(x) sum(is.na(x)))

#removing unwanted columns
Data <- Data[,-6:-16]

#remove na
Data <- Data[complete.cases(Data),] 

#checking data structure
str(Data)

#coverting continuous data to categorical
Data$Poverty <- cut(Data$Poverty, breaks = c(min(Data$Poverty),16.04,max(Data$Poverty)), labels = c("Low", "High"))

# Splitting data into traning and testing
library(caTools)
split = sample.split(Data$Poverty, SplitRatio = 0.60)
Train <- subset(Data, split == TRUE)
Test <- subset(Data, split == FALSE)

sapply(Train, function(x) sum(is.na(x)))

#apply decision tree algorithm on training data
library("party")
modeldecisiontree = ctree(Poverty~., data = Train,  controls = ctree_control(mincriterion = 0.99,minsplit = 15000))

modeldecisiontree

# Plot of decision tree model
plot(modeldecisiontree)

#prediction using test data
predictTrain <- predict(modeldecisiontree, newdata = Test)

#create confusion matrix
library(caret)
caret::confusionMatrix(predictTrain, Test$Poverty)

Test$predictTrain <- predictTrain

# Plot of actual vs predicted data
plot(Test$predictTrain,Test$Poverty,main = "Poverty", xlab = "predicted_data", ylab = "Actual_data",lwd=1,pch=1,cex=2)

#check Gini Index
library(ineq)
ineq(predictTrain,type="Gini")


