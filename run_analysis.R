##Step 1 - Download dataset and unzip it

filename <- "getdata_dataset.zip"

if (!file.exists(filename)) {
        + download.file(url = paste("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", sep = ""), destfile = filename, method = "curl")}

if (!file.exists("UCI HAR Dataset")) {unzip(filename)}

##Step 2 - Load activity labels and features as tables, and convert class to characters

activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])

features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

##Step 3 - Extract only the measurements on the mean and standard deviation

featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

##Step 4 - Load the training datasets, training lables and subjects, and then merge them all as one dataset called 'train'

train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

##Step 5 - Load the testing datasets, testing lables and subjects, and then merge them all as one dataset called 'test'

test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

##Step 6 - Merge the training and the testing data sets to create one data set called "CompleteData"

CompleteData <- rbind(train, test)

##Step 7 - Add appropriate labels to the data set with descriptive variable names

colnames(CompleteData) <- c("subject", "activity", featuresWanted.names)

##Step 8 - Convert activities & subjects into factors from activity labels

CompleteData$activity <- factor(CompleteData$activity, 
                                levels = activityLabels[,1], 
                                labels = activityLabels[,2])

CompleteData$subject <- as.factor(CompleteData$subject)

##Step 9 - Create a second, independent tidy data set with the average of each variable for each activity and each subject

library(reshape2)

CompleteData.melted <- melt(CompleteData, id = c("subject", "activity"))

CompleteData.mean <- dcast(CompleteData.melted, 
                           subject + activity ~ variable, mean)

write.table(CompleteData.mean, file = "TidyDataSet.txt", 
            row.names = FALSE, quote = FALSE)