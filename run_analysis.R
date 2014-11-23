library(tidyr)
library(dplyr)
library(plyr)

### Section 1. Merge the training and the test sets to create one data set.
subject_train <- read.table(file = "UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table(file = "UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table(file = "UCI HAR Dataset/train/Y_train.txt")

subject_test <- read.table(file = "UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table(file = "UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table(file = "UCI HAR Dataset/test/Y_test.txt")

names_X_from_file <- read.table(file = "UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
activity_labels = read.table('UCI HAR Dataset/activity_labels.txt',header=FALSE)

#merge
X <- rbind(X_train, X_test)
Y <- rbind(Y_train, Y_test)
subject <- rbind(subject_train,subject_test)


## Section 2. Extract only the measurements on the mean and standard deviation for each measurement. 
## Section 3. Use descriptive activity names to name the activities in the data set
#collecting variables names
Y$V1 <- factor(Y$V1)


names(activity_labels) <- c("activity", "description")

names(Y) <- c("activity")
names(subject) <- c("subject")

#filtering the X variables names
names(X) <- names_X_from_file$V2
namesX <- c(names(X))
namesX <- data.frame(namesX, stringsAsFactors = FALSE)
index <-  c(grepl("-mean..",namesX$namesX) & !grepl("-meanFreq..",namesX$namesX) & !grepl("mean..-",namesX$namesX) | grepl("-std..",namesX$namesX) & !grepl("-std()..-",namesX$namesX))
namesX <-namesX[index,]



#extraction of the variables
dataset <-  X[,match(namesX,names(X))] %>%
  cbind(subject) %>%
  cbind(Y)
colNamesDataSet <- names(dataset)


## Section 4. Appropriately label the data set with descriptive activity names.
for (i in 1:length(colNamesDataSet)) 
{
  colNamesDataSet[i] = gsub("\\()","",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("-std$","StdDev",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("-mean","Mean",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("^(t)","time",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("^(f)","freq",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("([Gg]ravity)","Gravity",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("[Gg]yro","Gyro",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("AccMag","AccMagnitude",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("JerkMag","JerkMagnitude",colNamesDataSet[i])
  colNamesDataSet[i] = gsub("GyroMag","GyroMagnitude",colNamesDataSet[i])
};

names(dataset) <- colNamesDataSet;


## Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#create tidy table
avarageDataSet <- aggregate(
  dataset[,names(dataset) != c("subject","activity")],
  by=list(subject = dataset$subject,
          activity=dataset$activity),
  mean)

#arrange by subject and activity
avarageDataSet <- arrange(avarageDataSet, subject, activity)

#revalue activity for better understanding
avarageDataSet$activity <- revalue(avarageDataSet$activity, c("1" =  "WALKING",
                                "2" =  "WALKING_UPSTAIRS",
                                "3" =  "WALKING_DOWNSTAIRS",
                                "4" =  "SITTING",
                                "5" =  "STANDING",
                                "6" =  "LAYING"))
avarageDataSet <- tbl_df(avarageDataSet)
write.table(avarageDataSet, 'avarageDataSet.txt',row.names=TRUE,sep='\t');