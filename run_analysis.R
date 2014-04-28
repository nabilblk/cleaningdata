## 1. Merging X Data (Traning and testing)

## Extract Train Data to tmpTrain
tmpTrain <- read.table("train/X_train.txt")
## Extract Test Data to tmpTest
tmpTest <- read.table("test/X_test.txt")
## Merge Testing and Training data into a single dataTable.
X <-rbind(tmpTrain,tmpTest)

## Extract Subject Train to Data 
tmpSubjectTrain <- read.table("train/subject_train.txt")
## Extract Subject Train to Data 
tmpSubjectTest <- read.table("test/subject_test.txt")
subject <-rbind(tmpSubjectTrain,tmpSubjectTest)

## Extract Y Training Data 
tmpYTrain <- read.table("train/y_train.txt")
## Extract Subject Train to Data 
tmpYTest <- read.table("test/y_test.txt")
Y <-rbind(tmpYTrain,tmpYTest)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
meanStdPattern <- "-mean\\(\\)|-std\\(\\)"
# get Indice to analyze 
indexToAnalyze <- grep(meanStdPattern,features[, 2])
# substruct X 
X <- X[, indexToAnalyze]
# Set Column names of X with the Features Name 
names(X) <- features[indexToAnalyze,2]
# Clean Variables Names remove () from column names
names(X) <- gsub("\\(|\\)", "", names(X))

## 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = as.character(activities[, 2])
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

## 4. Appropriately labels the data set with descriptive activity names. 

names(subject) <- "subject"
finalTidyData <- cbind(subject, Y, X)
write.table(finalTidyData, "merged_clean_data.txt")

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
uniqueSubjects = unique(subject)[,1]
numSubjects = length(unique(subject)[,1])
numActivities = length(activities[,1])
numCols = dim(finalTidyData)[2]
result = finalTidyData[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- finalTidyData[finalTidyData$subject==s & finalTidyData$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_mean.txt")




