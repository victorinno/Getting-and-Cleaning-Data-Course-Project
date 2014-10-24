library(tidyr)
library(dplyr)
library(plyr)

subject_train <- read.table(file = "UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table(file = "UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table(file = "UCI HAR Dataset/train/Y_train.txt")

subject_test <- read.table(file = "UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table(file = "UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table(file = "UCI HAR Dataset/test/Y_test.txt")

activity_labels = read.table('UCI HAR Dataset/activity_labels.txt',header=FALSE)


X <- rbind(X_train, X_test)
Y <- rbind(Y_train, Y_test)
subject <- rbind(subject_train,subject_test)


Y$V1 <- factor(Y$V1)

names(activity_labels) <- c("activity", "description")

names(Y) <- c("activity")
names(subject) <- c("subject")

names_X_from_file <- read.table(file = "UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
names(X) <- names_X_from_file$V2
namesX <- c(names(X), "subject", "activity")
namesX <- data.frame(namesX, stringsAsFactors = FALSE)
index <-  c(grepl("-mean..",namesX$namesX) & !grepl("-meanFreq..",namesX$namesX) & !grepl("mean..-",namesX$namesX) | grepl("-std..",namesX$namesX) & !grepl("-std()..-",namesX$namesX))
namesX <-namesX[index,]




dataset <-  select(X,match(namesX,names(X))) %>%
            cbind(subject) %>%
            cbind(Y)
colNamesDataSet <- names(dataset)

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

avarageDataSet <- aggregate(
  dataset[,names(dataset) != c("subject","activity")],
  by=list(activity=dataset$activity,
          subject = dataset$subject),
  mean)
            
final_result <- merge(avarageDataSet,activity_labels,by='activity',all.x=TRUE);

