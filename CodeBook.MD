## Data description

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

## Variables

The final output retrieved in the `TidyDataSet.txt` file contains 68 columns, among which you will find 2 grouping variables and average values grouped by these 2 variables :
* `ACTIVITY` describes the type of activity performed during the measurement ("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
* `SUBJECT_ID` corresponds to the different people who participated in the survey (numbered from 1 to 30)
*  The following 66 variables are average values computed by type of activity per survey participant :

"tBodyAcc_Mean-X",

"tBodyAcc_Mean-Y",

"tBodyAcc_Mean-Z",

"tBodyAcc_StDev-X",

"tBodyAcc_StDev-Y",

"tBodyAcc_StDev-Z",

"tGravityAcc_Mean-X",

"tGravityAcc_Mean-Y",

"tGravityAcc_Mean-Z",

"tGravityAcc_StDev-X",

"tGravityAcc_StDev-Y",

"tGravityAcc_StDev-Z",

"tBodyAccJerk_Mean-X",

"tBodyAccJerk_Mean-Y",

"tBodyAccJerk_Mean-Z",

"tBodyAccJerk_StDev-X",

"tBodyAccJerk_StDev-Y",

"tBodyAccJerk_StDev-Z",

"tBodyGyro_Mean-X",

"tBodyGyro_Mean-Y",

"tBodyGyro_Mean-Z",

"tBodyGyro_StDev-X",

"tBodyGyro_StDev-Y",

"tBodyGyro_StDev-Z",

"tBodyGyroJerk_Mean-X",

"tBodyGyroJerk_Mean-Y",

"tBodyGyroJerk_Mean-Z",

"tBodyGyroJerk_StDev-X",

"tBodyGyroJerk_StDev-Y",

"tBodyGyroJerk_StDev-Z",

"tBodyAccMagnitude_Mean",

"tBodyAccMagnitude_StDev",

"tGravityAccMagnitude_Mean",

"tGravityAccMagnitude_StDev",

"tBodyAccJerkMagnitude_Mean",

"tBodyAccJerkMagnitude_StDev",

"tBodyGyroMagnitude_Mean",

"tBodyGyroMagnitude_StDev",

"tBodyGyroJerkMagnitude_Mean",

"tBodyGyroJerkMagnitude_StDev",

"fBodyAcc_Mean-X",

"fBodyAcc_Mean-Y",

"fBodyAcc_Mean-Z",

"fBodyAcc_StDev-X",

"fBodyAcc_StDev-Y",

"fBodyAcc_StDev-Z",

"fBodyAccJerk_Mean-X",

"fBodyAccJerk_Mean-Y",

"fBodyAccJerk_Mean-Z",

"fBodyAccJerk_StDev-X",

"fBodyAccJerk_StDev-Y",

"fBodyAccJerk_StDev-Z",

"fBodyGyro_Mean-X",

"fBodyGyro_Mean-Y",

"fBodyGyro_Mean-Z",

"fBodyGyro_StDev-X",

"fBodyGyro_StDev-Y",

"fBodyGyro_StDev-Z",

"fBodyAccMagnitude_Mean",

"fBodyAccMagnitude_StDev",

"fBodyBodyAccJerkMagnitude_Mean",

"fBodyBodyAccJerkMagnitude_StDev",

"fBodyBodyGyroMagnitude_Mean",

"fBodyBodyGyroMagnitude_StDev",

"fBodyBodyGyroJerkMagnitude_Mean",

"fBodyBodyGyroJerkMagnitude_StDev".
