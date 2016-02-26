library("dplyr")
library("tidyr")

#READ IN VARIABLE LABELS

     variables <- read.table("UCI HAR Dataset/features.txt")

#READ IN TRAINING DATA
     dat_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
     colnames(dat_train) <- variables[,2]
     
#READ IN TRAINING SUBJECT NUMBER AND ADD TO TRAINING DATA TABLE    
     subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
     dat_train <- cbind(dat_train,subject_train)
     names(dat_train)[562] <- c("subject_number")

#READ IN AND ADD ACTIVITY NUMBER VARIABLE TO TRAINING DATA TABLE
     activity_train <- as.data.frame(read.table("UCI HAR Dataset/train/y_train.txt"))
     names(activity_train)[1] <- c("activity_number")
     dat_train <- cbind(dat_train,activity_train)
     
#REPEAT PROCESS FOR TEST DATA
     dat_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
     colnames(dat_test) <- variables[,2]

     subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
     dat_test <- cbind(dat_test, subject_test)
     names(dat_test)[562] <- c("subject_number")

     activity_test <- as.data.frame(read.table("UCI HAR Dataset/test/y_test.txt"))
     names(activity_test)[1] <- c("activity_number")
     dat_test <- cbind(dat_test,activity_test)

#COMBINE TRAINING AND TEST DATA     
     dat_complete <- rbind(dat_train, dat_test)

#READ IN ACTIVITY DESCRIPTIONS AND JOIN TO DATAFRAME     
     activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
     colnames(activity_labels) <- c("activity_number", "activity")
     dat_complete <- full_join(dat_complete, activity_labels, by = "activity_number")

#SELECT ONLY MEAN, SD COLUMNS AND ADD BACK SUBJECT, ACTIVITY, AND ACTIVITY DESCRIPTION VARIABLES     
     dat_meansd <- dat_complete[,grep("(mean|std)\\(\\)", colnames(dat_complete))]
     dat_meansd <- cbind(dat_meansd,dat_complete[,562:564])

#CREATE TIDY VERISON OF DAT_MEANSD
     dat_tidy <- gather(dat_meansd, reading, value, 1:66)

#REMOVE ACTIVITY NUMBER SINCE ACTIVITY DESCRIPTION IS INCLUDED     
     dat_tidy <- dat_tidy[,-2]
     
#CREATE TIDY DATA FRAME WITH MEANS FOR EACH SUBJECT/ACTIVITY/MEASUREMENT READING     
     dat_final <- dat_tidy %>% group_by(subject_number, activity, reading) %>% summarize(mean_value = mean(value))

#WRITE TO TEXT FILE     
     write.table(dat_final, "dat_final.txt", row.name = FALSE)