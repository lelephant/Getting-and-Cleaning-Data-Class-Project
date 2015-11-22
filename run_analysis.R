Clean_Data<- function(){
        library(readr)

### read in descriptive data
        actlabels<- c("Walking", "Walking Upstairs",
        "Walking Downstairs", "Sitting", "Standing", "Laying")
        Features<- readLines("UCI HAR Dataset/features.txt")

### read in test data
        subfactest<- as.numeric(readLines("UCI HAR Dataset/test/subject_test.txt"))
        ### creating vector to parse the fixed width data
        rep16 <- rep(16, times = 561)
        ### read in data using new vector
        testdata<- read_fwf(file = "UCI HAR Dataset/test/X_test.txt", fwf_widths(rep16))
        testlabels<- readLines("UCI HAR Dataset/test/Y_test.txt")

###read in training data
        traindata<- read_fwf("UCI HAR Dataset/train/X_train.txt", fwf_widths(rep16))
        subfactrain<- as.numeric(readLines("UCI HAR Dataset/train/subject_train.txt"))
        trainlabels<- readLines("UCI HAR Dataset/train/Y_train.txt")

#### combining test+train, train b4 test
        combinedlabels <- c(trainlabels, testlabels)
        Subjects<- c(subfactrain, subfactest)
        combineddata <- rbind(traindata, testdata)
###make label factor numeric
        combinedlabels<- as.numeric(combinedlabels)

### append features as the column names for my data
        colnames(combineddata)<- Features

###extracting the mean and standard deviation columns
        mean<- grep("mean", Features)
        std<- grep("std", Features)
        desiredcol <- c(mean, std)
        descolnames<- Features[desiredcol]
        extractcol<- combineddata[,descolnames]

### matching label factor with names
        namedlabels<- sapply(combinedlabels, function(x) actlabels[x])
###append activity labels and subject # as columns to data
        df1<- data.frame(Subject = Subjects, Activity = namedlabels, extractcol)

### find average of observations grouped by activity and subject
        df2<- aggregate(df1[,3:81], list(df1[,1], df1[,2]), mean)
        library(plyr)
        df2<-rename(df2, c("Group.1"="Subject", "Group.2"="Activity"))
        return(df2)
}
