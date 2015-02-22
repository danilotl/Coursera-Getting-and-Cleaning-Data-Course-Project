run_analysis <- function(){

##################
# Downloading data
##################

# Download the zip file and put it in the data directory
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/Dataset.zip", method="curl")
# Unzip the file
unzip(zipfile="./data/Dataset.zip", exdir="./data")
# Get data root folder
root.folder <- file.path("./data" , "UCI HAR Dataset")

##################
# Reading raw data
##################

# Reading names for features and labels for activities
features.names  <- read.table(file.path(root.folder, "features.txt"),        head = FALSE)
activity.labels <- read.table(file.path(root.folder, "activity_labels.txt"), head = FALSE)

# Test data
test.features <- read.table(file.path(root.folder, "test", "X_test.txt"),       header = FALSE)
test.activity <- read.table(file.path(root.folder, "test", "y_test.txt"),       header = FALSE)
test.subject  <- read.table(file.path(root.folder, "test", "subject_test.txt"), header = FALSE)

# Train data
train.features <- read.table(file.path(root.folder, "train", "X_train.txt"),       header = FALSE)
train.activity <- read.table(file.path(root.folder, "train", "y_train.txt"),       header = FALSE)
train.subject  <- read.table(file.path(root.folder, "train", "subject_train.txt"), header = FALSE)

#################################################################
# 1. Merges the training and the test sets to create one data set
#################################################################

# Merging rows
data.features <- rbind(test.features, train.features)
data.activity <- rbind(test.activity, train.activity)
data.subject  <- rbind(test.subject,  train.subject)

# Setting names
names(data.features) <- features.names[,2]
names(data.activity) <- c("activity")
names(data.subject)  <- c("subject")


# Merging columns to create the data frame for all data
data <- cbind(data.features, cbind(data.subject, data.activity))

###########################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement
###########################################################################################

# Subset features names by measurements on the mean and standard deviation
sub.features.names <- features.names[,2][grep("mean\\(\\)|std\\(\\)", features.names[,2])]

# Selected names from the previous subset, plus the "subject" and "activity" features names
selected.names <- c(as.character(sub.features.names), "subject", "activity" )

# Subset data by selected features names
data <- subset(data, select = selected.names)

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

# Add activity labels
data$activity <- factor(data$activity, levels = activity.labels[,1], labels = activity.labels[,2])

######################################################################
# 4. Appropriately labels the data set with descriptive variable names
######################################################################

names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "frequency", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))

#####################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
#####################################################################################################################

library(plyr);
tidy.data <- aggregate(. ~subject + activity, data, mean)
tidy.data <- tidy.data[order(tidy.data$subject,tidy.data$activity),]
write.table(tidy.data, file = "tidy_data.txt", row.name = FALSE)