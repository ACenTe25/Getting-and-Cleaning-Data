#First, all data should be read from the data folder:

featureNames <- c("subject","activity",as.character(read.table("UCI HAR Dataset/features.txt")[,2]))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

#Now, data will be "merged" together by rbind() and cbind() 
#functions.

X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
DS1 <- cbind(subject, y, X)

#At this point, Step 1 has been completed.
#In order to perform Step 2, we now assign the column names
#from the featureNames variable to DS1.

colnames(DS1) <- featureNames

#Now, according to "features_info.txt", we select the indexes
#corresponding to means and standard deviations for each
#measurement. This is done through the grep() function.

indexes <- sort(union(grep("mean()", featureNames, fixed = TRUE), grep("std()", featureNames, fixed = TRUE)))

#Now a subset of DS1 according to indexes is obtained to complete Step 2.

DS1 <- DS1[,c(1,2,indexes)]

#Just in case, remove the obsolete objects

rm(X, X_test, X_train, y, y_test, y_train, subject, subject_test, subject_train, featureNames, indexes)

#Now Step 3 requires to assign activity names to "activity" variable

DS1$activity <- as.factor(DS1$activity)
levels(DS1$activity) <- c("walking","walkingUpstairs","walkingDownstairs","sitting","standing","laying")
DS1$activity <- as.character(DS1$activity)

#Step 3 is done. Step 4 requires proper naming of the variables. Due to the
#length of the variable names, camel case is used for improved readability,
#as suggested by David Hood in the "Course Project - Step 4 - Descriptive
#Variable Names" forum. The gsub() function is used several times to fix the
#names.

colnames(DS1) <- gsub("-", "", colnames(DS1), fixed = TRUE)
colnames(DS1) <- gsub("mean()", "Mean", colnames(DS1), fixed = TRUE)
colnames(DS1) <- gsub("std()", "SD", colnames(DS1), fixed = TRUE)
colnames(DS1) <- gsub("tB", "timeB", colnames(DS1), fixed = TRUE)
colnames(DS1) <- gsub("fB", "freqB", colnames(DS1), fixed = TRUE)
colnames(DS1) <- gsub("tG", "timeG", colnames(DS1), fixed = TRUE)
colnames(DS1) <- gsub("Mag", "Magnitude", colnames(DS1), fixed = TRUE)

#Step 4 is done. Step 5 requires creating a second, independend tidy
#data set with the average of each variable for each activity and each
#subject. 

DS2 <- aggregate(DS1[,3:ncol(DS1)], by = list("subject" = DS1$subject, "activity" = DS1$activity), FUN = mean)