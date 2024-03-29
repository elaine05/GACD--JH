﻿The data come from the UCI Machine Learning Human Activity Recognition Using Smartphones Data Set. 

1. Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Check the README.txt file for further details about this dataset. 

A video of the experiment including an example of the 6 recorded activities with one of the participants can be seen in the following link: [Web Link]

An updated version of this dataset can be found at [Web Link]. It includes labels of postural transitions between activities and also the full raw inertial signals instead of the ones pre-processed into windows. 
Data Files
The original dataset included the following files:
•	‘README.txt’
•	‘features_info.txt’: Shows information about the variables used on the feature vector.
•	‘features.txt’: List of all features.
•	‘activity_labels.txt’: Links the class labels with their activity name.
•	‘train/X_train.txt’: Training set.
•	‘train/y_train.txt’: Training labels.
•	‘test/X_test.txt’: Test set.
•	‘test/y_test.txt’: Test labels.
The following files are available for the train and test data. Their descriptions are equivalent.
•	‘train/subject_train.txt’: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
•	‘train/Inertial Signals/total_acc_x_train.txt’: The acceleration signal from the smartphone accelerometer X axis in standard gravity units ‘g’. Every row shows a 128 element vector. The same description applies for the ‘total_acc_x_train.txt’ and ‘total_acc_z_train.txt’ files for the Y and Z axis.
•	‘train/Inertial Signals/body_acc_x_train.txt’: The body acceleration signal obtained by subtracting the gravity from the total acceleration.
•	‘train/Inertial Signals/body_gyro_x_train.txt’: The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.

2. Features

Each row contains, for a given subject and activity, 79 averaged signal measurements.
Identifiers 

•	subject
Subject identifier, Integer which ranges from 1 to 30.

•	activity
Activity identifier, string with 6 possible values:
o	WALKING: subject was walking
o	WALKING_UPSTAIRS: subject was walking upstairs
o	WALKING_DOWNSTAIRS: subject was walking downstairs
o	SITTING: subject was sitting
o	STANDING: subject was standing
o	LAYING: subject was laying

•	Mean and standart deviation for the following features (other values are presented in initial dataset, but for this reasearch only these parameters were used) 
o	tBodyAcc-XYZ
o	tGravityAcc-XYZ
o	tBodyAccJerk-XYZ
o	tBodyGyro-XYZ
o	tBodyGyroJerk-XYZ
o	tBodyAccMag
o	tGravityAccMag
o	tBodyAccJerkMag
o	tBodyGyroMag
o	tBodyGyroJerkMag
o	fBodyAcc-XYZ
o	fBodyAccJerk-XYZ
o	fBodyGyro-XYZ
o	fBodyAccMag
o	fBodyAccJerkMag
o	fBodyGyroMag
o	fBodyGyroJerkMag

3. Data Transformations

1) Download the provided source data and upzip the file.
2)	Read and merge the test and train data into one data frame.
3)	Relabel the data frame and extract required measurements.
4)	Clean the data set and export.
-	Values of Varible Activity consist of data from "Y_train.txt" and "Y_test.txt"
-	values of Varible Subject consist of data from "subject_train.txt" and subject_test.txt"
-	Values of Varibles Features consist of data from "X_train.txt" and "X_test.txt"
-	Names of Varibles Features come from "features.txt"
-	Levels of Varible Activity come from "activity_labels.txt"
The Activity, Subject and Features were used as part of descriptive variable names for data in data frame.
