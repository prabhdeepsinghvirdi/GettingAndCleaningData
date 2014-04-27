# Analysis 

# Setting Working directory
setwd("C:/Users/Prabhdeep/datasciencecoursera/UCI_HAR_Dataset")


# load x and y for Test and Train
xTrain = read.table("train/X_train.txt")
yTrain = read.table("train/y_train.txt")
xTest = read.table("test/X_test.txt")
yTest = read.table("test/y_test.txt")


# load variable names
varNames = read.table("features.txt"); varNames = varNames[,2];
activityLabels = read.table("activity_labels.txt"); 


# correctly name the columns of the data
names(xTrain) <- varNames
names(xTest) <- varNames

## Merge Test and Train
xMatrix = rbind(xTest,xTrain) # from 1 to 7532 they were used for training, from 7533 to 10299 they were used for testing
yVector = rbind(yTest,yTrain) # from 1 to 7532 they were used for training, from 7533 to 10299 they were used the result of testing

# which variables correspond to means 
whichMeans = grep("mean", varNames)

# correspondingly update xMatrix
xMatrixReduced = xMatrix[,whichMeans]

# write it:
write.table(xMatrixReduced,file="xMatrixReduced.txt")

yVectorLabels=vector(mode="character", length=length(yVector))

# for each activity label, actually put in a factor variable with the name of the activity
for(a in 1:6){
      inds = which(yVector==a)
      yVectorLabels[inds] <- as.character(activityLabels[a,2])
}

# loading subject IDs
subjectTrain = read.table("train/subject_train.txt")
subjectTest = read.table("test/subject_test.txt")

## renaming IDs to more appropriate 
# first create a function to add the word "Subject" to the number ID
subjectify = function(x){return(paste("Subject", as.character(x), sep=""))}
# apply said function to every element 
subjectNamesTest= sapply(subjectTest, FUN=subjectify)
subjectNamesTrain= sapply(subjectTrain, FUN=subjectify)

# also merge the test and train subject names
subjectNamesVector = rbind(subjectNamesTrain, subjectNamesTest)


### creating a second dataset that has the means of each variable for each subject
newData = matrix(ncol=length(names(xMatrixReduced)), nrow=length(unique(subjectNamesVector)))
rownames(newData) = unique(subjectNamesVector); 
colnames(newData) = names(xMatrixReduced);

# for each subject calculate the mean of all variables
for(s in unique(subjectNamesVector)){
      w = which(subjectNamesVector == s)
      cm = colMeans(xMatrixReduced[w,])
      newData[s,] = cm
}
write.table(newData,file="newData.txt")
