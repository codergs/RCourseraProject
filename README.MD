#README.MD
This is a course project for Getting and Cleanning Data online course offered on COURSERA by Johns Hopkins University. As a part of program requirement one is suppose to clean a samasung wearable "UCI HAR Dataset", and present in a tidy format. Below are the instructions on how to use the R script to achieve the project goals. 

## Instructions to run the run_analysis.R
* It is assumed here that you already have the dataset required for this analysis. In case if you do not have the required files, please download and unzip the dataset from link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip into your RStudio/R working directory

* Open the run_analysis.R in RStudio

* Point your working directory to place where the samsung data is and set it inside the data containing directory.
	E.g, If Donwloaded file resides under /Users/home/abc/UCI HAR Dataset
	Then your working directory should be inside /Users/home/abc/UCI HAR Dataset/

* Import following library packages:
	library(plyr)
	, library(reshape2)

* 	Run the script

* The required tidy data set would be availabe under tidy data frame and tidy.txt file under your RStudio current directory	


## How is tidy data written to tidy.txt using write.table:
   write.table(tidy,"tidy.txt",row.names=FALSE,sep =" ")

## How to read tidy.txt for evalutation 
   tidy <- read.table("tidy.txt",sep=" ",check.names=FALSE,header=TRUE)
