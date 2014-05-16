run_Analysis <- function ()
{
# set working directory to to read train data 
    setwd("~/Datascience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train")
    
# read X_train.txt, y_train.txt and subject_train.txt data files as table
    trainXdataset <- read.table("X_train.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)
    trainYdataset <- read.table("y_train.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)
    trainSdataset <- read.table("subject_train.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)

# column bind to merge subject and train X and y  data 
    ctrainSYdataset <- cbind(trainSdataset,trainYdataset)
    ctrainSYXdataset <- cbind(ctrainSYdataset,trainXdataset)

# set working directory to to read test data.
    setwd("~/Datascience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test")

# read X_test.txt, y_test.txt and subject_test.txt data files   
    testXdataset <- read.table("X_test.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)
    testYdataset <- read.table("y_test.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)
    testSdataset <- read.table("subject_test.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)

# column bind to merge subject and test X and y  data 
    ctestSYdataset <- cbind(testSdataset,testYdataset )
    ctestSYXdataset <- cbind( ctestSYdataset,testXdataset )

# merge the test and trai ndata by appending the rows
    mergeSYXdata <- rbind(ctrainSYXdataset,ctestSYXdataset)

# set working directory to to read feature and activity data 
    setwd("~/Datascience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

# read features.txt & activity_labels.txt as tables
    features <- read.table("features.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)
    activity_labels <- read.table("activity_labels.txt",  header=FALSE, sep="",stringsAsFactors=FALSE)

# assign fetuure data to colnames and assign other column names
    names(mergeSYXdata)[3:563] <- features[,2]
    colnames(mergeSYXdata)[1] <- 'subject'
    colnames(mergeSYXdata)[2] <- 'activity'
    colnames(activity_labels)[1] <- 'activity'
    colnames(activity_labels)[2] <- 'activity_name'

# merge Activity names with the train/test data join by actvity 
    actmergedata <- merge(activity_labels,mergeSYXdata, by.x="activity", by.y="activity" )

# extract colnames that contain std or mean & subject and activity name
    mergesubsetcolnames <- c(colnames(actmergedata)[which(names(actmergedata) %in% c('subject','activity_name') )], colnames(actmergedata)[grep("mean\\(\\)|std\\(\\)",names(actmergedata),ignore.case = TRUE )] )

# extract the data for the required columns
    subsetrequiredfields <- actmergedata [,(names(actmergedata ) %in% mergesubsetcolnames)]

# clean up the names 
    names(subsetrequiredfields) <- gsub("\\(|\\)","",tolower(names(subsetrequiredfields)))

# convert the data into table to apply mean on the fields and group them ny subject and activity Name/description

    tidysubsetrequiredfieldsdata <- data.table(subsetrequiredfields)   
    averagetidydata <-  tidysubsetrequiredfieldsdata[, lapply(.SD, mean), by=c("subject", "activity_name")]
    averagetidydata <- averagetidydata[order(averagetidydata$activity_name),]

#  write to the results to a file
    write.table(averagetidydata, "tidydata.txt",sep='\t')

# cleanup the memory
    rm(trainXdataset) 
    rm(trainYdataset)
    rm(trainSdataset)
    rm(testXdataset)
    rm(testYdataset)
    rm(testSdataset)
    rm(ctrainSYdataset)
    rm( ctrainSYXdataset)
    rm(ctestSYdataset)
    rm(ctestSYXdataset)
    rm(mergeSYXdata)

  return (averagetidydata)
}
