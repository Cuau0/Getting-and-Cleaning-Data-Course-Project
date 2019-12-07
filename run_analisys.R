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

Data_test <- cbind(cbind(subject_test,Y_test),X_test)
Data_train <- cbind(cbind(subject_train, Y_train), X_train)
Data_Total <- rbind(Data_train, Data_test)

################################################################################
##################Extracts only the measurements on the mean####################
##################and standard deviation for each measurement.##################
################################################################################
NamesF <- read.table("./UCI HAR Dataset/features.txt")

#Look for posiction of names "Mean" and "std"
NamesMean <- grep("mean", NamesF$V2)

NamesMean <- NamesMean[-c(24:26,30:32,36:38,40,42,44,46)]+2
NamesF$V2[NamesMean]

NamesStd <- grep("std", NamesF$V2)+2

Var_ord <- sort(c(NamesMean,NamesStd))
length(NamesStd)
Data_Mean_Std <- Data_Total[,c(1,2,Var_ord)]

#######################################################################################
###Uses descriptive activity names to name the activities in the data set##############
#######################################################################################

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
Act_Names <- activity_labels$V2

Data_Mean_Std$V1.1 <- Act_Names[Data_Mean_Std$V1.1]

###############################################################################
####Appropriately labels the data set with descriptive variable names #########
###############################################################################
x <- NamesF$V2[Var_ord-2]
x_num <- as.character(x)

names(Data_Mean_Std) <- c("Subject", "Activity", x_num)
###############################################################################
### From the data set in step 4, creates a second, independent ################
##### tidy data set with the average of each variable #########################
######## for each activity and each subject ###################################
###############################################################################

library(reshape2)

Data_Melt <- melt(Data_Mean_Std, id = c("Subject", "Activity"),
                  measure.vars = names(Data_Mean_Std)[-c(1,2)])
head(Data_Melt)

subje <- dcast(Data_Melt, Subject + Activity ~ variable, mean)

mean(Data_Mean_Std[1:27,3])

x1 <- filter(Data_Mean_Std, Subject == 1 & Activity == "STANDING")
mean(x1$`tBodyAcc-mean()-X`)
