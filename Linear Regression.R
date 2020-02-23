#set working dictionary
setwd("C:/Users/Lenovo/Desktop/NCI/Semester 1/Data Mining and Machine Learning 1/Project")

#extract data from CSV
Data <- read.csv("Large Enterprise Annual Payroll.csv", header=T)

#check for null values
sapply(Data, function(x) sum(is.na(x)))

#removing unwanted columns
Data <- Data[,c(-1,-2,-11,-12,-14)]

#changing column names
colnames(Data)[9] <- "ANNUAL_PAYROLL"

#checking structure of data
str(Data)

#converting data into numeric form
Data$Number.of.Enterprises <- as.numeric(Data$Number.of.Enterprises)
Data$Number.of.Establishments <- as.numeric(Data$Number.of.Establishments)
Data$Classified.in.Same.Sector.as.Enterprise <- as.numeric(Data$Classified.in.Same.Sector.as.Enterprise)
Data$Classified.in.Different.Sector.than.Enterprise <- as.numeric(Data$Classified.in.Different.Sector.than.Enterprise)
Data$Classified.in.Same.Industry.Group.as.Enterprise <- as.numeric(Data$Classified.in.Same.Industry.Group.as.Enterprise)
Data$Classified.in.Different.Industry.Group.than.Enterprise <- as.numeric(Data$Classified.in.Different.Industry.Group.than.Enterprise)
Data$Auxiliaries..MSBOs..and.CAOs <- as.numeric(Data$Auxiliaries..MSBOs..and.CAOs)
Data$Employment <- as.numeric(Data$Employment)
Data$ANNUAL_PAYROLL <- as.numeric(Data$ANNUAL_PAYROLL)

#checking summary of data
summary(Data)

#create normalize function to normalize data
normalize <- function(x) {return((x-min(x))/(max(x) - min(x)))}
normData <- normalize(Data)

#check summary of data after normalization
summary(normData)

#split data into traning and testing
library(caTools)
split = sample.split(normData$ANNUAL_PAYROLL, SplitRatio = 0.60)
Train <- subset(normData, split == TRUE)
Test <- subset(normData, split == FALSE)

#apply linear regression model on training data
fit <- lm(ANNUAL_PAYROLL ~ ., data = Train)

#check summary of model
summary(fit) 

#plot of model
par(mfrow=c(2,2))
plot(fit)

#prediction using test data
pred = predict(fit,newdata = Test)

Test$preicted <- pred

#plot of actual vs predicted data
plot(Test$preicted,Test$ANNUAL_PAYROLL ,main = "Annual Payroll", xlab = "Predicted_data", ylab = "Actual_data",lwd=1,pch=1,cex=2,col="Blue")
abline(a=0,b=1, col = "red")

#cheching root mean square value
library(ModelMetrics)
r<-rmse(pred,Test$ANNUAL_PAYROLL)
r

#checking Durbin Watson Value 
library("lmtest")
dwtest(fit)

