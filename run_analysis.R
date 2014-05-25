# run_analysis.R

# Merge the training and the test sets to create one data set.

# Load primary tables
X <- rbind(read.table("UCI HAR Dataset/test/X_test.txt"),
           read.table("UCI HAR Dataset/train/X_train.txt"))
y <- rbind(read.table("UCI HAR Dataset/test/y_test.txt"),
             read.table("UCI HAR Dataset/train/y_train.txt"))
subject <- rbind(read.table("UCI HAR Dataset/test/subject_test.txt"),
           read.table("UCI HAR Dataset/train/subject_train.txt"))

# merge tables into single dataset
data <- cbind(X, subject, y)

# Add appropriate variable names
features <- read.delim("UCI HAR Dataset/features.txt", sep=" ", header=FALSE,
                       col.names=c("rowid", "feature"))
names(data) <-append(as.character(features$feature), c("subject", "activity"))

# Extract only the measurements on the mean and standard deviation for each measurement.
keeps <- sort(unique(append(grep("mean", names(data), ignore.case=TRUE),
                append(grep("std", names(data), ignore.case=TRUE),
                c(562, 563))))) # columns 562 and 563 are the unit identifiers
data <- data[, keeps]
drops <- grep("angle", names(data), ignore.case=TRUE)
data <- data[, !c(1:ncol(data)) %in% drops]

# Use descriptive activity names to name the activities in the data set
activity_labels <- read.delim("UCI HAR Dataset/activity_labels.txt", sep=" ",
                              header=FALSE, col.names=c("rowid", "activity"))
data$activity <- factor(data$activity, levels=activity_labels$rowid,
                        labels=activity_labels$activity)
data$subject <- factor(data$subject, levels=c(1:30), labels=paste("subject", c(1:30)))

# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject.
library("reshape")
tidy <- melt.data.frame(data) # correctly assumes that factor vars are id.vars
tidy <- cast(tidy, formula = activity + subject ~ ..., fun.aggregate=mean)

# Clean up names
tidy_names <- names(tidy)
tidy_names <- gsub("-", "_", tidy_names) # underscores instead of hyphens
tidy_names <- gsub("()", "", tidy_names, fixed=TRUE) # Remove parentheses 
tidy_names <- gsub("angle(", "angle_", tidy_names, fixed=TRUE)
tidy_names <- gsub(")", "", tidy_names, fixed=TRUE)
tidy_names <- gsub(",", ".", tidy_names, fixed=TRUE) # . instead of , for angles
tidy_names <- sub("^f", "freq_", tidy_names, fixed=FALSE) # expand time/freq domain labels
tidy_names <- sub("^t", "time_", tidy_names, fixed=FALSE)
tidy_names <- sub("BodyBody", "Body", tidy_names, fixed=FALSE)
names(tidy) <- tidy_names

# Save dataset
write.table(tidy, "tidy_activity_data.txt")