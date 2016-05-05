library(caret)
library(randomForest)
library(dplyr)

setwd("C:/Users/johnecon/Projects/datasciencecoursera/Practical Machine Learning/assignment/")
pml_data <- read.csv("./data/pml-training.csv", na.strings=c("", "NA","#DIV/0!"))


inTrain <- createDataPartition(y=pml_data$classe, list=FALSE, p=0.7)
training <- pml_data[inTrain,]
testing <- pml_data[-inTrain,]
validation <- read.csv("./data/pml-testing.csv")
training <- training[,colSums(is.na(training))/nrow(training)<0.7]
training <- training[,-c(1:7)]
