library(plyr)
library(data.table)


# 0. Download the data
# 1. Import the downloaded table into workspace
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
featureTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
featureTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)



#2. Merge Data for each Dataframe
## 2.1. Append the dataframe by rows
subdf <- subjectTrain %>% rbind(subjectTest)
actdf <- activityTrain %>% rbind(activityTest)
fdf <- featureTrain %>% rbind(featureTest)

## 2.2. Set the labels
colnames(subdf) <- 'Subject'
colnames(actdf) <- 'Activity'
fnames <- read.table("UCI HAR Dataset/features.txt", head = FALSE)
colnames(fdf) <- fnames$V2

## 2.3. Merge all the dataframe into one
df <- subdf %>% cbind(actdf) %>% cbind(fdf)

# 3. Extracts only the measurements on the mean and standard deviation for each measurement
## 3.1. Get the list contain name of Features by measurements to get the mean and standard deviation
ffnames <- fnames$V2[grep(c('-(mean|std)\\(\\)', 'Subject', 'Activity'), fnames$V2)]

## 3.2. Subset the data based on our features list
sfnames <- c(as.character(ffnames), "Subject", "Activity")
newdf <- subset(df, select = sfnames)

## 3.3. Label the variables
actlab <- read.table("UCI HAR Dataset/activity_labels.txt")
newdf$Activity <- factor(newdf$Activity, levels = actlab$V1, 
                                 labels = actlab$V2)
newdf$Subject <- as.factor(newdf$Subject)

## 3.4. Change variables names to descriptive feature names
names(newdf) <- gsub("^t", "time", names(newdf))
names(newdf) <- gsub("^f", "frequency", names(newdf))
names(newdf) <- gsub("Acc", "Accelerometer", names(newdf))
names(newdf) <- gsub("Gyro", "Gyroscope", names(newdf))
names(newdf) <- gsub("Mag", "Magnitutde", names(newdf))
names(newdf) <- gsub("BodyBody", "Body", names(newdf))
View(head(newdf))

#4. Create a second,independent tidy data set and ouput it, 
#with the average of each variable for each activity and each subject

tidydata <- aggregate(.~Subject + Activity, newdf, mean)
tidydata <- tidydata[order(tidydata$Subject, tidydata$Activity), ]
write.table(tidydata, file = "tidy.txt", row.name = FALSE)






