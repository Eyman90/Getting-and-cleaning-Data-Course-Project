###########################################################################################################################################
# Step 1: Merge the training and the test sets to create one data set.
###########################################################################################################################################

# Set WD to import files:
getwd()
mywd <- "C:/Users/sebas/Documents/R-Projects/Coursera - getting and cleaning data 1"
setwd(mywd)
getwd()


# 1.1 Read files

# 1.1.1  Read training tables:
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

dim(x_train)
dim(y_train)
dim(subject_train)

# 1.1.2 Read test tables:
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

dim(x_test)
dim(y_test)
dim(subject_test)

# 1.1.3 Read features vector:
features <- read.table("features.txt")
dim(features)

# 1.1.4 Read activity labels:
activityLabels = read.table("activity_labels.txt")
dim(activityLabels)


# 1.2 Assigning column names:

colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')


#1.3 Merging all data in one set:

total_train <- cbind(y_train, subject_train, x_train)
total_test <- cbind(y_test, subject_test, x_test)
FitData <- rbind(total_train, total_test)

dim(FitData)
#Health Check: 10299   563

###########################################################################################################################################
# Step 2: Extract only the measurements on the mean and standard deviation for each measurement. 
###########################################################################################################################################

#2.1 Reading column names:

colNames <- colnames(FitData)

#2.2 Create vector for defining ID, mean and standard deviation:

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

mean_and_std

#2.3 Making nessesary subset from FitData:

Subset_meanSD <- FitData[ , mean_and_std == TRUE]
Subset_meanSD

dim(Subset_meanSD)

###########################################################################################################################################
#Step 3: Use descriptive activity names to name the activities in the data set
###########################################################################################################################################

setWithActivityNames <- merge(Subset_meanSD, activityLabels,
                              by='activityId',
                              all.x=TRUE)

dim(setWithActivityNames)

###########################################################################################################################################
# Step 4: Label dataset appropriately with descriptive variable names. 
###########################################################################################################################################
#Done in previous steps, see 1.3,2.2 and 2.3!

###########################################################################################################################################
# Step 5: Create a second independent dataset from the data in step 4 with the average of each variable for each activity and each subject.
###########################################################################################################################################

#5.1 Making a second tidy data set

secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

dim(secTidySet)
View(secTidySet)
#5.2 Writing second tidy data set in txt file

write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
