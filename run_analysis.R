#setwd("~/Desktop/coursera/curso3")
library(dplyr)
#function to extract not na values
extract<-function(x){
  x<-as.numeric(x[[1]])
  x<-x[!is.na(x)]
}

#read train files
train_subject<-read.csv("UCI HAR Dataset/train/subject_train.txt",header = FALSE)
train_labels <- read.csv("UCI HAR Dataset/train/y_train.txt",header = FALSE)
train_set<-readLines("UCI HAR Dataset/train/X_train.txt")
#split train_set by " "
train_set<-lapply(train_set,function(x) strsplit(x," "))
#get only not na values
train_set<-sapply(train_set,function(x) extract(x),simplify = TRUE)
#formatting train_set
train_set<-as.data.frame(t(train_set))
#names of features
features<-read.csv(file = "UCI HAR Dataset/features.txt",sep=" ",header = FALSE)
features<-as.character(features[,2])
#set column names
colnames(train_set)<-features
#only mean or std columns
features2<-features[grepl("mean",features) | grepl("std",features)]
train_set<-train_set[,features2]
#formatting colnames
colnames(train_set)<-gsub(pattern = "\\(|\\)","",colnames(train_set))

#add subjects and labels
train_set$subject<-train_subject$V1
train_set$labels<-train_labels$V1

#read test files
test_subject<-read.csv("UCI HAR Dataset/test/subject_test.txt",header = FALSE)
test_labels <- read.csv("UCI HAR Dataset/test/y_test.txt",header = FALSE)
test_set<-readLines("UCI HAR Dataset/test/X_test.txt")
#split test_set by " "
test_set<-lapply(test_set,function(x) strsplit(x," "))
#get only not na values
test_set<-sapply(test_set,function(x) extract(x),simplify = TRUE)
#formatting train_set
test_set<-as.data.frame(t(test_set))
#names of features
features<-read.csv(file = "UCI HAR Dataset/features.txt",sep=" ",header = FALSE)
features<-as.character(features[,2])
#set column names
colnames(test_set)<-features
#only mean or std columns
features2<-features[grepl("mean",features) | grepl("std",features)]
test_set<-test_set[,features2]
#formatting colnames
features3<-gsub(pattern = "\\(|\\)","",colnames(test_set))
colnames(test_set)<-gsub(pattern = "\\(|\\)","",colnames(test_set))

#add subjects and labels
test_set$subject<-test_subject$V1
test_set$labels<-test_labels$V1

#merge train and test as matrix to do it faster
total_set<-rbind(train_set,test_set)
activities<-read.csv("UCI HAR Dataset/activity_labels.txt",header = FALSE,sep = " ")
colnames(activities)<-c("id","labels")
total_set$activity<-factor(total_set$labels,activities$id,activities$labels)
total_set$labels<-NULL

#main dplyr style
main_total_set<-total_set %>% group_by(subject,activity) %>% summarise_each(funs(mean))
write.table(main_total_set,row.names = FALSE,file = "getting_and_cleaning_data.txt")
