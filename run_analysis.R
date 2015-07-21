## 1. Merges the training and the test sets to create one data set.
ActivLbl <- read.table("./activity_labels.txt", stringsAsFactors = F)
Features <- read.table("./features.txt", stringsAsFactors = F)
SubTrain <- read.table("./train/subject_train.txt", stringsAsFactors = F)
XTrain   <- read.table("./train/X_train.txt", stringsAsFactors = F)
YTrain   <- read.table("./train/y_train.txt", stringsAsFactors = F)
SubTest  <- read.table("./test/subject_test.txt", stringsAsFactors = F)
XTest    <- read.table("./test/X_test.txt", stringsAsFactors = F)
YTest    <- read.table("./test/y_test.txt", stringsAsFactors = F)

trainData <- cbind(YTrain,SubTrain,XTrain)
testData  <- cbind(YTest,SubTest,XTest)

FinalDataSet <- rbind(trainData,testData)


colnames(FinalDataSet) <- c("ACTIVITY_ID","SUBJECT_ID", c(Features[,2]))

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

library(dplyr)

DF <- FinalDataSet[grepl("ACTIVITY_ID",colnames(FinalDataSet)) | 
                       grepl("SUBJECT_ID",colnames(FinalDataSet)) |
                       grepl("mean()",colnames(FinalDataSet)) & 
                      !grepl("meanFreq",colnames(FinalDataSet)) |
                       grepl("std()",colnames(FinalDataSet))
                      ]

## 3. Uses descriptive activity names to name the activities in the data set

TidyDataSet <- rename((select((merge(x = ActivLbl,y = DF, by.x = "V1", by.y = "ACTIVITY_ID")),
              -1)), ACTIVITY = V2)

## 4. Appropriately labels the data set with descriptive variable names. 

colnames(TidyDataSet) <- gsub("-mean\\()","_Mean",colnames(TidyDataSet))
colnames(TidyDataSet) <- gsub("-std\\()","_StDev",colnames(TidyDataSet))
colnames(TidyDataSet) <- gsub("Mag","Magnitude",colnames(TidyDataSet))

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)

AvgDataSet <- ddply(TidyDataSet, .(ACTIVITY, SUBJECT_ID), function(x) (colMeans(x[, 3:68], na.rm = T)))

write.table(AvgDataSet, "TidyDataSet.txt", row.name=FALSE)
