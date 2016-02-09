# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# This script 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of
# each variable for each activity and each subject.

# Installing and importing needed libraries
install.packages('dplyr')
install.packages('stringr')
install.packages('reshape2')
library(dplyr)
library(stringr)
library(reshape2)

# Reading files
activityTest <- read.table("./data/test/y_test.txt", col.names = c("activityLabel"))
activityTrain <- read.table("./data/train/y_train.txt", col.names = c("activityLabel"))
activityLabels <- read.table("./data/activity_labels.txt", col.names = c("activityLabel", "activityName"))
subjectTrain <- read.table("./data/train/subject_train.txt", col.names = c("subject"))
subjectTest <- read.table("./data/test/subject_test.txt", col.names = c("subject"))
featuresTest <- read.table("./data/test/X_test.txt")
featuresTrain <- read.table("./data/train/X_train.txt")
featureNames <- read.table("./data/features.txt")


# 1) Merges the training and the test sets to create one data set.
dataSubject <- rbind(subjectTrain, subjectTest)
dataActivity<- rbind(activityTrain, activityTest)
dataFeatures<- rbind(featuresTrain, featuresTest)


# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# Regex escape parenthesis()
featuresOfInterest <- grep("mean\\(\\)|std\\(\\)", featureNames$V2)
filteredFeatures <- featureNames[featuresOfInterest, ]
filteredDataFeatures <- dataFeatures[, filteredFeatures$V2]
names(filteredDataFeatures)<- filteredFeatures$V2 %>% str_replace_all("\\(\\)", "")


# 3) Uses descriptive activity names to name the activities in the data set
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(filteredDataFeatures, dataCombine)
Data$activity <- factor(Data$activity, levels = activityLabels[,1], labels = activityLabels[,2])


# 4) Appropriately labels the data set with descriptive variable names
names(Data) <- names(Data) %>% str_replace_all("\\(\\)", "") %>%
  str_replace_all("mean", "Mean") %>%
  str_replace_all("std", "Std") %>%
  str_replace_all("-", "") %>%
  str_replace_all("^t", "time") %>%
  str_replace_all("^f", "frequency") %>%
  str_replace_all("Acc", "Accelerometer") %>%
  str_replace_all("Gyro", "Gyroscope") %>%
  str_replace_all("Mag", "Magnitude") %>%
  str_replace_all("BodyBody", "Body")


# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final.melted <- melt(Data, id = c("subject", "activity", "activityLabel"))
final.mean <- dcast(final.melted, subject + activity + activityLabel ~ variable, mean) 
final.mean.ordered <- final.mean %>% arrange(subject, activity, activityLabel)
View(final.mean.ordered)
write.table(final.mean.ordered, "tidy.txt", row.names = FALSE, quote = FALSE)