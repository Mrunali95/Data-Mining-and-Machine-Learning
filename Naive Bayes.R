#Changing working dictionary path
setwd("C:/Users/Lenovo/Desktop/NCI/Semester 1/Data Mining and Machine Learning 1/Project")

#reading data from CSVs
Data <- read.csv("Employement Change 2015.csv", header=T)

#checking for NA values
sapply(Data, function(x) sum(is.na(x)))

#check structure of data
str(Data)

#Additiong values of four columsns and saving in one column
Data$Total_Change_in_employement <- rowSums(Data[,c("CHANGE.IN.EMPLOYMENT.Birth","CHANGE.IN.EMPLOYMENT.Death","CHANGE.IN.EMPLOYMENT.Expand","CHANGE.IN..EMPLOYMENT.contract")])
Data$Final_Employemnt <- rowSums(Data[,c("Initial.EMPLOYMENT","Total_Change_in_employement")])

#converting data into factor
Data$States_Cat <- as.factor(Data$States_Cat)

#checking structure of data
str(Data)

#checking summary of data
summary(Data)

#converting continuous data into categorical data
FinalEmployement <- cut(Data$Final_Employemnt, breaks = c(min(Data$Final_Employemnt)-1,6.169,max(Data$Final_Employemnt)+1), labels = c("Low", "High"))

#creating new dataframe
DataNB <- data.frame(FinalEmployement, Data$NUMBER.OF.ESTABLISHMENTS.Birth, Data$NUMBER.OF.ESTABLISHMENTS.Death, Data$CHANGE.IN.EMPLOYMENT.Expand, Data$NUMBER.OF.ESTABLISHMENTS.Contract, Data$States_Cat, Data$NAICS.DESCRIPTION)

#splitting data into training and testing
library(caTools)
split = sample.split(DataNB$FinalEmployement, SplitRatio = 0.60)
Train <- subset(DataNB, split == TRUE)
Test <- subset(DataNB, split == FALSE)

#Applying Naive Bayes model on training data
library(e1071)
model1 <- naiveBayes(FinalEmployement ~ ., data = Train)
modelpredict <- predict(model1, newdata = Test)
modelpredict

#creating confusiong matrix
library(caret)
caret::confusionMatrix(Test$FinalEmployement, modelpredict)

#checking for Root mean square value
library(ModelMetrics)
r<-rmse(modelpredict,Test$FinalEmployement)
r

