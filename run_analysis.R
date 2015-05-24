require(dplyr);require(tidyr);require(reshape2)
# 1. Merge the training and the test sets [create one data set].

#set working directory 
setwd("~/econometrics/DataScienceSpecialist/3 Getting and Cleaning Data/UCI HAR Dataset")

# Read general info
features <- read.table('./features.txt',header=FALSE) #imports features.txt
activity <- read.table('./activity_labels.txt',header=FALSE) #imports activity_labels.txt

# Read the training data
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE) #imports subject_train.txt
xTrain <- read.table('./train/x_train.txt',header=FALSE) #imports x_train.txt
yTrain <- read.table('./train/y_train.txt',header=FALSE) #imports y_train.txt

# Read the test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE) #imports subject_test.txt
xTest <- read.table('./test/x_test.txt',header=FALSE) #imports x_test.txt
yTest <- read.table('./test/y_test.txt',header=FALSE) #imports y_test.txt

# Create training set
colnames(activity) <- c('activityId','activity')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

trainingData = cbind(yTrain,subjectTrain,xTrain)

# Create test set
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

testData <- cbind(yTest,subjectTest,xTest)

# create final data set [Combine training and test set]
# = one data set using rbind
finalData <- rbind(trainingData,testData)



# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logical vector: TRUE  for the ID, mean() & stddev() columns ,FALSE if not
#log_index
namesFD <- colnames(finalData)
log_index <- (grepl("activity..",namesFD) | grepl("subject..",namesFD) | grepl("-mean..",namesFD) & !grepl("-meanFreq..",namesFD) & !grepl("mean..-",namesFD) | grepl("-std..",namesFD) & !grepl("-std()..-",namesFD));

# Subset finalData table based on the log_index 
finalData <- finalData[log_index==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
# Updating the namesFD vector to include the new column names after merge

finalData <- merge(finalData,activity,by='activityId',all.x=TRUE);
namesFD <- colnames(finalData);

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
# Add new descriptive column names to the finalData set
for (i in 1:length(namesFD))
{
  namesFD[i] <- gsub("\\()","",namesFD[i])
  namesFD[i] <- gsub("-std$","StdDev",namesFD[i])
  namesFD[i] <- gsub("-mean","Mean",namesFD[i])
  namesFD[i] <- gsub("^(t)","time",namesFD[i])
  namesFD[i] <- gsub("^(f)","freq",namesFD[i])
  namesFD[i] <- gsub("([Gg]ravity)","grav",namesFD[i])
  namesFD[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","body",namesFD[i])
  namesFD[i] <- gsub("[Gg]yro","Gyro",namesFD[i])
  namesFD[i] <- gsub("AccMag","accM",namesFD[i])
  namesFD[i] <- gsub("([Bb]odyaccjerkmag)","bodyJM",namesFD[i])
  namesFD[i] <- gsub("JerkMag","JM",namesFD[i])
  namesFD[i] <- gsub("GyroMag","GM",namesFD[i])
};

colnames(finalData) <- namesFD

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, sub.NoA without the activity column
# Summarizing the sub.NoA table to include just the mean of each variable for each activity and each subject
# Merging the tidyData with activity to include descriptive acitvity names
# Export the tidyData set locally

sub.NoA <- finalData[,names(finalData) != 'activity']
tidyData <- aggregate(sub.NoA[,names(sub.NoA) != c('activityId','subjectId')],by=list(activityId=sub.NoA$activityId,subjectId = sub.NoA$subjectId),mean);
tidyData <- merge(tidyData,activity,by='activityId',all.x=TRUE)
write.table(tidyData, './tidyData.txt',row.names=TRUE,col.names=NA,sep='\t')
