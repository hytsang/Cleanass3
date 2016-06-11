#Read data, append the next variables to the set Train and Test,where x=set, y=labels and activity_labels.txt Links the class labels with their activity name

train = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
dcol<-ncol(train)
train[,dcol+1] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
train[,dcol+2] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
test = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
dcoltes<-ncol(test)
test[,dcoltes+1] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
test[,dcoltes+2] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
actlab = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Read features and normalize syntax to uniform calculations
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Merge sets
BruteData = rbind(train, test)

# Filter mean and std
SelectedColumns <- grep(".*Mean.*|.*Std.*", features[,2])
features <- features[SelectedColumns,]

# Add subject and activity columns and filter 
SelectedColumns <- c(SelectedColumns, dcol+1, dcol+2)
BruteData <- BruteData[,SelectedColumns]

# Add the column names to BruteData and "capitalize" colnames with correct typeset
colnames(BruteData) <- c(features$V2, "Activity", "Subject")
colnames(BruteData) <- tolower(colnames(BruteData))

currentActivity = 1
for (currentActivityLabel in actlab$V2) {
        BruteData$activity <- gsub(currentActivity, currentActivityLabel, BruteData$activity)
        currentActivity <- currentActivity + 1
}

BruteData$activity <- as.factor(BruteData$activity)
BruteData$subject <- as.factor(BruteData$subject)

#generate the final table

indeptidy = aggregate(BruteData, by=list(activity = BruteData$activity, subject=BruteData$subject), mean)

# Remove means where has no mean ing, subject and activity....

indeptidy[,90] = NULL
indeptidy[,89] = NULL

# write to disk

write.table(indeptidy, "indeptidy.txt", sep="\t", row.name=FALSE)