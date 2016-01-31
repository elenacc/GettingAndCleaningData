
### Obtaining the data ###

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "HumanActivityRecognitionUsingSmartphones.zip"
download.file(url = url, destfile = file, method = "curl")
unzip(file)

###  Exercise ###
## You should create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set.

# test data
testSubject <- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
testX <- read.table('./UCI HAR Dataset/test/X_test.txt',header=FALSE)
testY <- read.table('./UCI HAR Dataset/test/Y_test.txt',header=FALSE)

# train data
trainSubject <- read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
trainX <- read.table('./UCI HAR Dataset/train/X_train.txt',header=FALSE)
trainY <- read.table('./UCI HAR Dataset/train/Y_train.txt',header=FALSE)

# reference data
activityLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', header=FALSE)
features <- read.table('./UCI HAR Dataset/features.txt', header=FALSE)

# assign column names (labels) to the data
colnames(activityLabels)  = c('ActivityID','ActivityDescr');

colnames(testSubject) = "SubjectID"
colnames(testX)       = features[,2] 
colnames(testY)       = "ActivityID"

names(testSubject)
names(testX)
names(testY)

colnames(trainSubject)  = "SubjectID"
colnames(trainX)        = features[,2] 
colnames(trainY)        = "ActivityID"

names(trainSubject)
names(trainX)
names(trainY)

# combine test data (columns)
testData <- cbind(testY, testSubject, testX)
head(testData)

# combine train data (columns)
trainData <- cbind(trainY, trainSubject, trainX)
head(trainData) 

# merge test and train data (rows)
data <- rbind(testData,trainData)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# extract desired subset
indexOfFeatures <- (grepl("ActivityID",names(data)) | grepl("SubjectID",names(data)) | grepl("-mean..",names(data)) & !grepl("-meanFreq..",names(data)) & !grepl("mean..-",names(data)) | grepl("-std..",names(data)) & !grepl("-std()..-",names(data)))
dataMeanStd <- data[indexOfFeatures == TRUE]
names(dataMeanStd)

data <- dataMeanStd

## 3. Uses descriptive activity names to name the activities in the data set
data <- merge(data,activityLabels,by='ActivityID',all.x=TRUE)
head(data)
names(data)

## 4. Appropriately labels the data set with descriptive activity names.

# tidy column names
colnames(data) <- gsub('-mean', 'Mean', names(data))
colnames(data) <- gsub('-std', 'Std', names(data))
colnames(data) <- gsub('Mag', 'Magnitude', names(data))
colnames(data) <- gsub('[-()]', '', names(data))
names(data)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dataAggr <- aggregate(data[,names(data) != c('ActivityID','ActivityDescr','SubjectID')],by=list(ActivityID = data$ActivityID, ActivityDescr = data$ActivityDescr, SubjectID = data$SubjectID), mean)
dataAggr <- dataAggr[,c(1:3,5:22)]
head(dataAgrr)


### Exporting the data ###
write.table(dataAggr, './FinalDataset.txt',row.names=FALSE,sep='\t')
