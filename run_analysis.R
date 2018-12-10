# Getting and Cleaning Data Course
# Author: Dhaval Mandalia

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Download Data & set working directory
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setwd("D:/Documents/Training/John Hopkins Data Science/Course 3 - Getting & Cleanup Data/Project/UCI HAR Dataset")
workingdir<-getwd()
#download.file(url, file.path(workingdir, "FUCI HAR Dataset.zip"))
#unzip(zipfile = "FUCI HAR Dataset.zip")


#Load activity label and feature data to R
activitylabels <- fread(file.path(workingdir,"UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("Class", "Activity"))
features <- fread(file.path(workingdir, "UCI HAR Dataset/features.txt") , col.names = c("Index", "Feature"))
#\\ Explicit as there are some mean Frequency values with "mean" literal within which should not be considered
filtered_features<-grep("(mean|std)\\(\\)",features[,Feature])
measurements <- features[filtered_features,Feature]
# Removing () for cleaner operation later
measurements <- gsub('[()]', '', measurements)

# Load train datasets for only 66 activities filtered above
traindata <- fread(file.path(workingdir, "UCI HAR Dataset/train/X_train.txt"))[, filtered_features, with = FALSE]
data.table::setnames(traindata, colnames(traindata), measurements)
trainLabels <- fread(file.path(workingdir, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(workingdir, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
traindata <- cbind(trainSubjects, trainLabels, traindata)

# Load test datasets for only 66 activities filtered above
testdata <- fread(file.path(workingdir, "UCI HAR Dataset/test/X_test.txt"))[, filtered_features, with = FALSE]
data.table::setnames(testdata, colnames(testdata), measurements)
testLabels <- fread(file.path(workingdir, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(workingdir, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
testdata <- cbind(testSubjects, testLabels, testdata)

# merge datasets train and test as Alldata
Alldata<- cbind(traindata,testdata)
# Converting Class labels to Activity Name
Alldata[["Activity"]]<-factor(Alldata[,Activity],levels = activitylabels[["Class"]], labels = activitylabels[["Activity"]])
Alldata[["SubjectNum"]] <- as.factor(Alldata[, SubjectNum])
Alldata <-reshape2::melt(data=Alldata,id=c("SubjectNum","Activity"))

#Extracting Tidydata with mean values
Tidydata<-reshape2::dcast(data=Alldata,SubjectNum + Activity ~ variable, fun.aggregate = mean)
data.table::fwrite(x=Tidydata,file="tidydata.txt",quote=FALSE)
