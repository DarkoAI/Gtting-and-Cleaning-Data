## trainData making
trainData <- read.table("~/Desktop/UCI HAR Dataset/train/X_train.txt")
dim(trainData)

head(trainData)
## trainLabel making
trainLabel <- read.table("~/Desktop/UCI HAR Dataset/train/y_train.txt")
table(trainLabel)
## trainSubject making
trainSubject <- read.table("~/Desktop/UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("~/Desktop/UCI HAR Dataset/test/X_test.txt")
dim(testData)
## testLabel making
testLabel <- read.table("~/Desktop/UCI HAR Dataset/test/y_test.txt") 
table(testLabel)
## testSubject making
testSubject <- read.table("~/Desktop/UCI HAR Dataset/test/subject_test.txt")
## join (Data, Label, Subject)
joinData <- rbind(trainData, testData)
dim(joinData)

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel)

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) 
## Extracts only the measurements on the mean and standard deviation for each measurement
features <- read.table("~/Desktop/UCI HAR Dataset/features.txt")
dim(features) 

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)

joinData <- joinData[, meanStdIndices]
dim(joinData)

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

## Uses descriptive activity names to name the activities in the data set

activity <- read.table("~/Desktop/UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

## Labels data set with descriptive activity names

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData)

##Creates a second, independent tidy data set with the average of each variable for each activity and each subject

write.table(cleanedData, "merged_data.txt")
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
        for(j in 1:activityLen) {
                result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
                row <- row + 1
        }
}
head(result)

## Making second dataset

write.table(result, "tidy.txt")
