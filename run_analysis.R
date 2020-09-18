library(dplyr)

#read files
variables <- read.csv("./UCI HAR Dataset/features.txt", sep = "", header = FALSE)
test <- read.csv("./UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE)
test_labels <- read.csv("./UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE)
test_subject <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep = "", header = FALSE)
train <- read.csv("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
train_labels <- read.csv("./UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE)
train_subject <- read.csv("./UCI HAR Dataset/train/subject_train.txt", sep = "", header = FALSE)

#combine test and train
complete <- rbind(test, train)
complete_labels <- rbind(test_labels, train_labels)
complete_subject <- rbind(test_subject, train_subject)

#convert activity labels to factor
complete_labels_f <- factor(complete_labels$V1, 
                          labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS",
                                     "SITTING", "STANDING", "LAYING"))

#get only variables with mean() and std() calculations
mean_std_cols <- grep("mean\\(\\)|std\\(\\)", variables$V2)
colnames <- grep("mean\\(\\)|std\\(\\)", variables$V2, value = TRUE)

#rename the columns to the matching variable names
extract <- complete[, mean_std_cols]
names(extract) <- colnames

#bind the subject number and activity type to the front of the dataframe
extract <- cbind(activity = complete_labels_f, extract)
extract <- cbind(subject = complete_subject$V1, extract)

#convert to tibble for dplyr
extract_tbl <- tbl_df(extract)

#sort first by subject, then activity
extract_tbl <- extract_tbl %>% arrange(subject, activity)

#calculate the mean of all the non-grouping columns
grouped_extract_tbl_means <- extract_tbl %>% 
                    group_by(subject, activity) %>% 
                    summarize(across(
                        .cols = where(is.numeric), 
                        .fns = list(Mean = mean), 
                        .names = "{col}_{fn}"
                    ))

#write the means to a file
write.table(grouped_extract_tbl_means, "tidydata.txt", row.names = FALSE)