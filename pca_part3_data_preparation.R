############################################################################
# Bioramble
# PCA - Part 3: In the trenches (data preparation)
# by Jesse Lipp
# Aug 16, 2015
############################################################################

# --------------------------------------------------------------------------
# Set up environment
# --------------------------------------------------------------------------
# clean-up
rm(list = ls())

# load libraries (install if needed)
if (!require(stringr)) {
  install.packages("stringr")
}

# ----------------------------------------------------------
# Aquire data from UCI
# ----------------------------------------------------------
# set file directory
setwd("~/")
# download UCI HAR data set and extract specific components
if (!file.exists("UCI HAR Dataset/activity_labels.txt")) {
  download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip", 
                destfile = "smartphone_movement.zip", 
                method = "curl")
  unzip(zipfile = "smartphone_movement.zip", 
        files = c("UCI HAR Dataset/activity_labels.txt", 
                  "UCI HAR Dataset/features.txt", 
                  "UCI HAR Dataset/train/subject_train.txt", 
                  "UCI HAR Dataset/train/X_train.txt", 
                  "UCI HAR Dataset/train/y_train.txt"))
}

# ----------------------------------------------------------
# Process data
# ----------------------------------------------------------
# build experimental description data frame
subjects <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                       col.names = "subjects")
activity <- read.table("UCI HAR Dataset/train/y_train.txt", 
                       col.names = "activity_code")
activity_names <- read.table("UCI HAR Dataset/activity_labels.txt",
                             col.names = c("activity_code", "activity_name"), 
                             stringsAsFactors = FALSE)
activity_map <- activity_names$activity_name
names(activity_map) <- activity_names$activity_code
description <- cbind(subjects, activity)
description$activity_name <- activity_map[description$activity_code]

# build measurements data frame
feature_names <- read.table("UCI HAR Dataset/features.txt", 
                            colClasses = c("NULL", "character"))
feature_names <- unlist(feature_names)
feature_names <- str_replace_all(feature_names, "[()]", "")
features <- read.table("UCI HAR Dataset/train/X_train.txt", 
                       colClasses = rep("numeric", 561), 
                       col.names = feature_names)

# use a subset of the data (first three subjects)
measurements <- features[subjects < 6, ]
description <- description[subjects < 6, ]

# ----------------------------------------------------------
# Save data
# ----------------------------------------------------------
# save data sets
write.table(measurements, file = "pca_part3_measurements.txt")
write.table(description, file = "pca_part3_description.txt")