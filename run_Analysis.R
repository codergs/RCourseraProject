
# Read the activity_labels file using read.table function
labels <- read.table("activity_labels.txt",stringsAsFactors=FALSE,col.names=c("id","label"))
labels <- labels[,2]  


# Read the features file using read.table function
colNames <- read.table("features.txt",stringsAsFactors=FALSE,col.names=c("id","name"),check.names=FALSE)

# Droping the first column "id"
colNames$id<-NULL

# Reading the data files from train folder 

subject_Train<-read.table("./train/subject_train.txt",stringsAsFactors=FALSE,col.names="Subject")
x_Train<-read.table("./train/X_train.txt",stringsAsFactors=FALSE,col.names=colNames$name,check.names=FALSE)
y_Train<-read.table("./train/y_train.txt",stringsAsFactors=FALSE,col.names="Act",check.names=FALSE)

# Reading the data files from test folder

subject_Test <- read.table("./test/subject_test.txt",stringsAsFactors=FALSE,,col.names="Subject")
x_Test <- read.table("./test/X_test.txt",stringsAsFactors=FALSE,col.names=colNames$name,check.names=FALSE)
y_Test <- read.table("./test/y_test.txt",stringsAsFactors=FALSE,col.names="Act",check.names=FALSE)

# Find column names that have mean and std as sub-string 
colNames <- colNames[grep('mean|std',colNames$name),]
bad<-grepl("meanFreq",colNames)
newColNames<-colNames<-colNames[!bad]
newColNames<-gsub("tBody","timeBody",newColNames)
newColNames<-gsub("-mean\\(\\)","Mean",newColNames)
newColNames<-gsub("-std\\(\\)","Std",newColNames)
newColNames<-gsub("Acc","Accel",newColNames)
newColNames<-gsub("-Z","Z",newColNames)
newColNames<-gsub("-X","X",newColNames)
newColNames<-gsub("-Y","Y",newColNames)
newColNames<-gsub("fBody","freqBody",newColNames)
newColNames<-gsub("freqBodyBody","freqBody",newColNames)

# Droping column names that aren't in colNames for x_Train and y_Train
x_Train <- x_Train[,names(x_Train) %in% colNames]
x_Test  <- x_Test [,names(x_Test)  %in% colNames]

# Giving descriptive names to variables
names(x_Train)<-newColNames
names(x_Test)<-newColNames

# Column binding Test and Train 
subject_Test  <-  cbind (subject_Test,y_Test)
subject_Train <-  cbind (subject_Train,y_Train)

# Assisgning descriptive name to the dataset using the activity_labels.txt
subject_Test<-mutate(subject_Test,Activity = factor(Act, labels=labels))
subject_Train<-mutate(subject_Train,Activity = factor(Act, labels=labels))

# Droping Act column and cleaning the temporary variables
subject_Test$Act <-NULL
subject_Train$Act <-NULL
rm(y_Test)
rm(y_Train)
rm(bad)

# Column binding Test and Train 
subject_Test  <-  cbind (subject_Test,x_Test)
subject_Train <-  cbind (subject_Train,x_Train)

# Cleaning temporary variables
rm(x_Test)
rm(x_Train)
rm(labels)

# Row binding subject_Test and subject_Train data frames in one
tidyData <- rbind.fill(subject_Test, subject_Train)

# Cleaning temporary variables
rm(subject_Test)
rm(subject_Train)

# Sorting based on Subject
tidyData<-arrange(tidyData,Subject)

# Independent tidy data set 
tidy<-melt(tidyData,id=c("Subject","Activity"))
tidy<-dcast(Subject + Activity ~ variable, data = tidy, fun = mean)

# Giving descriptive names to tidy data set
names(tidy)<-c("Subject","Activity",paste("mean(",newColNames,")",sep=""))

# Removing temporary variables 
rm(newColNames)
rm(colNames)

# Write tidy data frame to tidy.txt
write.table(tidy,"tidy.txt",row.names=FALSE)

