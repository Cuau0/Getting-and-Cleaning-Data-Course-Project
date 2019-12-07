# run_analisys.R
# Read de data in the file UCI HAR Dataset and leave the same name that appears in the file
library(dplyr)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


##############################################################################
### 1. Merges the training and the test sets to create one data set##############
##############################################################################
# We create the total data of each set: Train and test data
# And We only merges the train data and test data with the function rbind and create the Data_Total containing all data
 
Data_test <- cbind(cbind(subject_test,Y_test),X_test)
Data_train <- cbind(cbind(subject_train, Y_train), X_train)
Data_Total <- rbind(Data_train, Data_test)

################################################################################
################## 2. Extracts only the measurements on the mean ###############
################## and standard deviation for each measurement. ################
################################################################################
# Read the names of the data to know the position later
NamesF <- read.table("./UCI HAR Dataset/features.txt")

# Looking for posiction of names "Mean" and "std" and manually remove coincidences that don't work
# We add two positions because we have two more variables in the data
NamesMean <- grep("mean", NamesF$V2)
NamesMean <- NamesMean[-c(24:26,30:32,36:38,40,42,44,46)]+2
#NamesF$V2[NamesMean]#
NamesStd <- grep("std", NamesF$V2)+2

# For there to be an order we put the positions of the two sets together 
# and create a new table with only data of mean and std 
Var_ord <- sort(c(NamesMean,NamesStd))
length(NamesStd)
Data_Mean_Std <- Data_Total[,c(1,2,Var_ord)]

#######################################################################################
### 3. Uses descriptive activity names to name the activities in the data set #########
#######################################################################################
# We read the names of the activities and exchange the name with their respective number 
# in the same data of the mean and std

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
Act_Names <- activity_labels$V2

Data_Mean_Std$V1.1 <- Act_Names[Data_Mean_Std$V1.1]

###############################################################################
#### 4- Appropriately labels the data set with descriptive variable names #####
###############################################################################
# We save the names by subtracting two positions and convert them into characters
x <- NamesF$V2[Var_ord-2]
x_num <- as.character(x)

# We change the names with a simple function "names" and add "Subject" and "Activity" with their respective order
names(Data_Mean_Std) <- c("Subject", "Activity", x_num)

###############################################################################
### 5. From the data set in step 4, creates a second, independent #############
##### tidy data set with the average of each variable #########################
######## for each activity and each subject ###################################
###############################################################################

library(reshape2)
# Use as variables all the variables except the first two that record the subject and the activity
Data_Melt <- melt(Data_Mean_Std, id = c("Subject", "Activity"), measure.vars = names(Data_Mean_Std)[-c(1,2)])
head(Data_Melt)

# Sorting the variables that depend on subject and activity. 
# We take the average of the records that coincide in subject and activity
# and keep them in the same row to finally have 180 rows with each subject having 6 activities
Data_sort <- dcast(Data_Melt, Subject + Activity ~ variable, mean)

# we verify that the results are correct looking for the data of "tBodyAcc-mean () - X"
# and where the subject 1 and the activity "STANDING" coincide, from these data we take the average
x1 <- filter(Data_Mean_Std, Subject == 1 & Activity == "STANDING")
mean(x1$`tBodyAcc-mean()-X`)
