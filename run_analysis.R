#Merges the training and the testing sets to create one data set.

#Download file
packages <- c("data.table", "reshape2","dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Merges into one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

#Check data
as.data.frame(Merged_Data)
head(Merged_Data)
View(Merged_Data)

#mean and standard deviation
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
TidyData$code <- activities[TidyData$code, 2]

#replace variable names
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("tBody","TimeDomainBody",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("tGravity","TimeDomainGravity",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("fBody","FrequencyDomainBody",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("Acc","Acceleration",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("Gyro", "AngularVelocity",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-XYZ","3AxialSignals",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-X","XAxis",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-Y","YAxis",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-Z","ZAxis",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("Mag","MagnitudeSignals",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-mean()","MeanValue",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-std()","StandardDeviation",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-mad()","MedianAbsoluteDeviation ",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-max()","LargestValueInArray",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-min()","SmallestValueInArray",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-sma()","SignalMagnitudeArea",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-energy()","EnergyMeasure",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-iqr()","InterquartileRange ",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-entropy()","SignalEntropy",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-arCoeff()","AutoRegresionCoefficientsWithBurgOrderEqualTo4",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-correlation()","CorrelationCoefficient",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-maxInds()", "IndexOfFrequencyComponentWithLargestMagnitude",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-meanFreq()","WeightedAverageOfFrequencyComponentsForMeanFrequency",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-skewness()","Skewness",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-kurtosis()","Kurtosis",names(TidyData), fixed=TRUE)
names(TidyData)<-gsub("-bandsEnergy()","EnergyOfFrequencyInterval.",names(TidyData), fixed=TRUE)

#creates a second set of data
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all( list(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

str(FinalData)