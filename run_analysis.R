# Getting and Cleaning Data Course Project

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load packages
library(data.table, quietly = TRUE)
library(reshape2, quietly = TRUE) #melt function
#sapply(packages, require, character.only=TRUE, quietly=TRUE)
# Load dataset
if(!file.exists("./data")){dir.create("./data")}
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="./data/Dataset.zip")
unzip(zipfile = "./data/Dataset.zip")

# Work on activity labels + features
activityLabels <- fread("UCI HAR Dataset/activity_labels.txt", 
                        col.names = c("classLabels", "activityName"))
features <- fread("UCI HAR Dataset/features.txt",
                  col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames]) #only mean and standard deviation
measurements <- features[featuresWanted, featureNames] #only for desirable features
measurements <- gsub('[()]', '', measurements) #delete () from dataset's names

# Work on train datasets
train <- fread("UCI HAR Dataset/train/X_train.txt")[, featuresWanted, with = FALSE] #only with desirable features
data.table::setnames(train, colnames(train), measurements) #use measurements names
trainActivities <- fread("UCI HAR Dataset/train/Y_train.txt",
                         col.names = c("Activity"))
trainSubjects <- fread("UCI HAR Dataset/train/subject_train.txt",
                       col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train) #combine by columns

# Work on test datasets (same as train datasets)
test <- fread("UCI HAR Dataset/test/X_test.txt")[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread("UCI HAR Dataset/test/Y_test.txt",
                        col.names = c("Activity"))
testSubjects <- fread("UCI HAR Dataset/test/subject_test.txt",
                      col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

#Combine datasets
bind_dataset <- rbind(train, test)

# Convert classLabels to activityName
bind_dataset[["Activity"]] <- factor(bind_dataset[, Activity]
                                     , levels = activityLabels[["classLabels"]]
                                     , labels = activityLabels[["activityName"]])
bind_dataset[["SubjectNum"]] <- as.factor(bind_dataset[, SubjectNum]) #use SubjectNum as factor
bind_dataset <- reshape2::melt(data = bind_dataset, id = c("SubjectNum", "Activity")) #melt data with id
bind_dataset <- reshape2::dcast(data = bind_dataset, SubjectNum + Activity ~ variable, fun.aggregate = mean) #cast a molten data frame into data frame using formula with aggregation function mean


data.table::fwrite(x = bind_dataset, file = "UCI HAR Dataset/TidyData.txt")
