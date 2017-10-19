#### Getting Data ####

library(dplyr)

# download zip file containing data #

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
#the files are binary so we have to use mode = wb

download.file(zipUrl, zipFile, mode = "wb")
dataPath <- "UCI HAR Dataset"
#unzip the file in dataPath
unzip(zipFile)

#### Reading Data ####

# Train Data #

#We use read.table to read data from text file and creates a data frame from i

trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingSet <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingLabels <- read.table(file.path(dataPath, "train", "y_train.txt"))

# Set Data #

testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testSet <- read.table(file.path(dataPath, "test", "X_test.txt"))
testLabels <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))

###### Step 2 : Merging Data #####
#cbind = combine df by columns
#rbind = combine df vy rows

humanActivity <- rbind(
  cbind(trainingSubjects, trainingSet, trainingLabels),
  cbind(testSubjects, testSet, testLabels)
)

# Rename column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

###### Step 3 Extracts only the measurements on the mean and standard deviation for each measurement #####

# looking for columns that contains mean or std subject or activity 
NewColumns <- grepl("mean|std|subject|activity", colnames(humanActivity))

humanActivity <- humanActivity[, NewColumns]

###### Step 4 Uses descriptive activity names to name the activities in the data set  ######

humanActivity$activity <- factor(humanActivity$activity,levels = activities[, 1], labels = activities[, 2])

###### Step 5 Appropriately labels the data set with descriptive variable names ######

humanActivityCols <- colnames(humanActivity)

#  gsub(pattern, replacement, x) => replace x pattern by replacement

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
#remove abreviations
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

#  correct some columns names

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

##### Step 6 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject###

##group by subject & activity & summarise each variable

library(dplyr)
humanActivitySummary <- humanActivity %>% group_by(subject, activity) %>% summarise_each(funs(mean))

#export data to text file
write.table(humanActivitySummary, "tidy_data.txt", row.names = FALSE,quote = FALSE)
