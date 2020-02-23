#Chaging working dictionary path
setwd("C:/Users/Lenovo/Desktop/NCI/Semester 1/Data Mining and Machine Learning 1/Project")

#reading data from CSV file
Data <- read.csv("Employement Change 2015.csv", header=T)

#checking for null values
sapply(Data, function(x) sum(is.na(x)))

#plot of data to check missing values
library(Amelia)
missmap(Data, main = "Missing values vs observed")

#Additiong values of four columsns and saving in one column
Data$Total_Change_in_employement <- rowSums(Data[,c("CHANGE.IN.EMPLOYMENT.Birth","CHANGE.IN.EMPLOYMENT.Death","CHANGE.IN.EMPLOYMENT.Expand","CHANGE.IN..EMPLOYMENT.contract")])
Data$Final_Employemnt <- rowSums(Data[,c("Initial.EMPLOYMENT","Total_Change_in_employement")])

#checking structure of data
str(Data)

DataLR <- Data

#converting data into factor 
DataLR$States_Cat <- as.factor(DataLR$States_Cat)

str(DataLR)

#removing unwanted columns
DataLR <- DataLR[,-1]
DataLR <- DataLR[,-2]
DataLR <- DataLR[,-3]
DataLR <- DataLR[,-4]
DataLR <- DataLR[,-5]
DataLR <- DataLR[,-6]
DataLR <- DataLR[,-7]
DataLR <- DataLR[,-7]

#converting continuous data into categorical data
DataLR$Final_Employemnt <- cut(DataLR$Final_Employemnt, breaks = c(min(DataLR$Final_Employemnt)-1,6.169,max(DataLR$Final_Employemnt)+1), labels = c("Low", "High"))

#install.packages("caTools")
library(caTools)

#splitting data into Training and Testing
split = sample.split(DataLR$Final_Employemnt, SplitRatio = 0.64)
LRTrain <- subset(DataLR, split == TRUE)
LRTest <- subset(DataLR, split == FALSE)

#apply logistic rgression model on training data
LRModel = glm(Final_Employemnt ~ ., data=LRTrain, family=binomial(link = 'logit'))

str(LRModel)

#plot of model
par(mfrow=c(2,2))
plot(LRModel)

#prediction of model using test data
predictTrain <- predict(LRModel, newdata = LRTest)
predictTrain <- ifelse(predictTrain<14,"Low","High")
PredictTrainf <- as.factor(predictTrain)

table(predictTrain, LRTest$Final_Employemnt)

summary(predictTrain)

#checking accuracy of model
accuracy <- table(predictTrain, LRTest[,"Final_Employemnt"])
sum(diag(accuracy))/sum(accuracy)

#install.packages('ModelMetrics')
library(ModelMetrics)
#Checking root mean square value
r<-rmse(PredictTrainf,LRTest$Final_Employemnt)
r

#Plotting ROC curve
library(pROC)
# Compute AUC for predicting Class with the variable CreditHistory.Critical
library(ROCR)
#Evaluation Method-- ROC Curve
par(mfrow=c(1,1))
prob <- predict(LRModel, newdata=LRTest, type="response")
pred <- prediction(prob, LRTest$Final_Employemnt)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,colorize=T, main="ROC Curve", ylab = "Senstivity", xlab="Specificity")
abline(a=0,b=1)

# Area under Curve (AUC)
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
legend(.6,.4,auc, title="AUC")
