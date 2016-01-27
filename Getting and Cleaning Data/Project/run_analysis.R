# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# This script 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of
# each variable for each activity and each subject.

# Importing needed libraries
library(dplyr)
library(stringr)
library(reshape2)

# Reading files
test_labels <- read.table("./data/test/y_test.txt", col.names = c("label"))
train_labels <- read.table("./data/train/y_train.txt", col.names = c("label"))
activities <- read.table("./data/activity_labels.txt", col.names = c("label", "name"))
train_subject <- read.table("./data/train/subject_train.txt", col.names = c("subject"))
test_subject <- read.table("./data/test/subject_test.txt", col.names = c("subject"))
test <- read.table("./data/test/X_test.txt")
train <- read.table("./data/train/X_train.txt")
features <- read.table("./data/features.txt")

# Introducing activity names into labels
test_labels <- test_labels %>%
                merge(activities, by.x = "label", by.y = "label")
train_labels <- train_labels %>%
        merge(activities, by.x = "label", by.y = "label")

# 1) Merges the training and the test sets to create one data set.
merged_data <- rbind(train, test)
merged_labels <- rbind(train_labels, test_labels)
merged_subjects <- rbind(train_subject, test_subject)



# 2) Extracts only the measurements on the mean and standard deviation for each measurement.

# Regex escape parenthesis()
features_of_interest <- grep("mean\\(\\)|std\\(\\)", features$V2)
filtered_features <- features[features_of_interest, ]
# Extracted data
merged_data <- merged_data[, features_of_interest]
names(merged_data) <- filtered_features$V2 %>% str_replace_all("\\(\\)", "")



# 3) Uses descriptive activity names to name the activities in the data set
merged_data_with_activities <- merged_data %>% mutate(activity = merged_labels$label)





# 4) Appropriately labels the data set with descriptive variable names
# Rename header
names(merged_data_with_activities) <- names(merged_data_with_activities) %>% str_replace_all("\\(\\)", "") %>%
        str_replace_all("mean", "Mean") %>%
        str_replace_all("std", "Std") %>%
        str_replace_all("-", "")




# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
merged_data_with_activities_and_subjects <- merged_data_with_activities %>% mutate(subject = merged_subjects$subject)
final.melted <- melt(merged_data_with_activities_and_subjects, id = c("subject", "activity"))
final.mean <- dcast(final.melted, subject + activity ~ variable, mean) 
View(final.mean)
final.mean.ordered <- final.mean %>% arrange(activity, subject)
View(final.mean.ordered)
write.table(final.mean.ordered, "tidy.txt", row.names = FALSE, quote = FALSE)

