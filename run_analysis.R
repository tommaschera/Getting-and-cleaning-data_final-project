library(dplyr)

# checking for data directory
if(!dir.exists("Data")){
        dir.create("Data")
        setwd(paste0(getwd(), "/Data"))
} else {
        setwd(paste0(getwd(), "/Data"))
}

# downloading data
run_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(run_url, "dataset.zip")

### READING RELEVANT FILES ###

# features (column names)
features <- read.table("./UCI HAR Dataset/features.txt")
# train data (70% of observations)
train_set <- read.table("./UCI HAR Dataset/train/X_train.txt")
# test data (30% of observations)
test_set <- read.table("./UCI HAR Dataset/test/X_test.txt")
# subject data (train)
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
# subject data (test)
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
# activity labels (train)
train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
# activity labels (test)
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")

### MERGING FILES ###

# Merging train and test (vertically)
subject_data <- rbind(train_subject, test_subject)
obs_data <- rbind(train_set, test_set)
labels_data <- rbind(train_labels, test_labels)

# Labeling data
names(obs_data) <- features[,2]
names(subject_data) <- "subject"
names(labels_data) <- "activity"

# Merging subject, observations, labels (horizontally)
full_dataset <- cbind(subject_data, labels_data, obs_data) ## Entire dataset

### SELECTING ONLY MEANS AND STANDARD DEVIATIONS ###

# Logical indexing of mean() and sd() columns
mean_or_sd_cols <- grepl("mean\\(\\)", names(full_dataset)) | grepl("std\\(\\)", names(full_dataset))

# Subsetting full_dataset according to logical indexing
full_dataset <- cbind(full_dataset[,1:2], full_dataset[,mean_or_sd_cols])
full_dataset <- arrange(full_dataset, subject)

### ASSIGNING DESCRIPTIVE LABELS AND COLUMN NAMES ###

# Descriptive labels
full_dataset$activity <- gsub(1, "WALKING", full_dataset$activity)
full_dataset$activity <- gsub(2, "WALKING_UPSTAIRS", full_dataset$activity)
full_dataset$activity <- gsub(3, "WALKING_DOWNSTAIRS", full_dataset$activity)
full_dataset$activity <- gsub(4, "SITTING", full_dataset$activity)
full_dataset$activity <- gsub(5, "STANDING", full_dataset$activity)
full_dataset$activity <- gsub(6, "LAYING", full_dataset$activity)

# Cleaning column names
names(full_dataset) <- gsub("^t", "time", names(full_dataset))
names(full_dataset) <- gsub("^f", "frequency", names(full_dataset))
names(full_dataset) <- gsub("Acc", "accelerometer", names(full_dataset))
names(full_dataset) <- gsub("Gyro", "gyrometer", names(full_dataset))
names(full_dataset) <- gsub("Mag", "magnitude", names(full_dataset))
names(full_dataset) <- gsub("-", "", names(full_dataset))
names(full_dataset) <- gsub("\\(", "", names(full_dataset))
names(full_dataset) <- gsub("\\)", "", names(full_dataset))
names(full_dataset) <- tolower(names(full_dataset))

### AVERAGING VALUES PER SUBJECT PER ACTIVITY ###

tidyset <- group_by(full_dataset, subject, activity)
tidyset <- summarise_all(tidyset, .funs = mean)

# Cleaing environment except output
rm(list=setdiff(ls(), c("full_dataset", "tidyset")))

# Outputting .csv with tidyset results
setwd("../")
write.csv(tidyset, "run_analysis_tidyset.csv")