library("data.table")


GetFeatures <- function (data.location="./UCI HAR Dataset") {
  # Reads the variable features vector.
  #
  # Args:
  #   data.location: Location of raw data.
  #
  # Returns:
  #   All variables features vector.
  setwd(data.location)
  features <- read.table("./features.txt")
  # Creating features character vector
  features.vector = c()
  for (i in 1:length(features$V2)) {
    features.vector <- c(features.vector, toString(features$V2[i]))
  }
  setwd("../")
  return(features.vector)
}


DataSetCreator <- function(features.vector, data.set.label='train', data.location="./UCI HAR Dataset") {
  # Computes the sample covariance between two vectors.
  # Creates data set of training group.
  #
  # Args:
  #   data.set.labels: Name of set label.
  #   data.location: Location of raw data.
  #   features.vector: character vector of variables.
  #
  # Returns:
  #   Data set of selected group.
  setwd(data.location)
  # Changing working directory to the chosen data set folder.
  working.directory = paste("./", data.set.label, sep = "")
  setwd(working.directory)
  # Reading subject_<>.txt file (subject id).
  subject.file.name <- paste("./subject_", data.set.label, ".txt", sep = "")
  subject.id <- read.table(subject.file.name)
  # Reading y_<>.txt file (training activity label).
  training.file.name <- paste("./y_", data.set.label, ".txt", sep = "")
  training.label <- read.table(training.file.name)
  # Reading X_<>.txt file (training set).
  set.file.name <- paste("./X_", data.set.label, ".txt", sep = "")
  set <- read.table(set.file.name)
  # Creating training set table.
  set.table <- data.table(set)
  setnames(set.table, features.vector)
  # Adding training activity label to table.
  set.table[,training.label:=training.label$V1]
  # Adding subject id to table.
  set.table[,subject.id:=subject.id$V1]
  setwd("../../")
  return(set.table)
}

EditFeatures <- function (features.vector=c()) {
  # Creates features vector with only mean and std variables.
  # Merges reffered data to one data set
  #
  # Args:
  #   features.vector: All variables features vector.
  #
  # Returns:
  #   Variables with mean and std values features vector.
  # Creating features character vector
  new.features.vector = c()
  for (i in 1:length(features.vector)) {
    string <- features.vector[i]
    if (substr(string, nchar(string)-7, nchar(string)-2) == "mean()") { 
      new.features.vector <- c(new.features.vector, string)
    } else if (substr(string, nchar(string)-5, nchar(string)-0) == "mean()") {
      new.features.vector <- c(new.features.vector, string)
    } else if (substr(string, nchar(string)-6, nchar(string)-2) == "std()") {
      new.features.vector <- c(new.features.vector, string)
    } else if (substr(string, nchar(string)-4, nchar(string)-0) == "std()") {
      new.features.vector <- c(new.features.vector, string)
    }
    
  }
  
  return(new.features.vector)
}

LabelActivitys <- function (data.set) {
  # Creates descriptive activity names.
  #
  # Args:
  #   data.set: Data table with training variables values.
  #
  # Returns:
  #   Data Table with descriptive activity names.
  print(length(data.set$training.label))
  for (i in 1:length(data.set$training.label)) {
    if (data.set$training.label[i] == 1) {
      data.set$training.label[i] <- "WALKING"
    }
    if (data.set$training.label[i] == 2) {
      data.set$training.label[i] <- "WALKING_UPSTAIRS"
    }
    if (data.set$training.label[i] == 3) {
      data.set$training.label[i] <- "WALKING_DOWNSTAIRS"
    }
    if (data.set$training.label[i] == 4) {
      data.set$training.label[i] <- "SITTING"
    }
    if (data.set$training.label[i] == 5) {
      data.set$training.label[i] <- "STANDING"
    }
    if (data.set$training.label[i] == 6) {
      data.set$training.label[i] <- "LAYING"
    }
  }
  return(data.set)
}

CalculateAverages <- function (data.set, features.vector) {
  # Calculates averages of different activitys and subjects.
  #
  # Args:
  #   data.set: Data table with training variables values.
  #   features.vector: Variables features vector. 
  #
  # Returns:
  #   Data Table with descriptive activity names.
  activities <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "LAYING")
  subject.id <- c(1:30)
  new.data.set <- data.table(subject.id=c(1:180))
  ii <- 0
  for (activity in activities) {
    data.set.edited <- subset(data.set, training.label == activity)
    for (id in subject.id) {
      ii <- ii + 1 
      data.set.edited.edited <- subset(data.set.edited, subject.id == id)
      for (feature in features.vector) {
        new.data.set[[feature]][[ii]] <- mean(data.set.edited.edited[[feature]])
        new.data.set[["subject.id"]][[ii]] <- id
        new.data.set[["training.label"]][[ii]] <- activity
      }
    }
  }
  return(new.data.set)
}

# Get variable features.
features.vector <- GetFeatures() 
# Get "train" data set.
train.data.set <- DataSetCreator(features.vector, data.set.label = 'train')
# Get "test" data set.
test.data.set <- DataSetCreator(features.vector, data.set.label = 'test')
# Merge "Train" and "Test" data sets.
merged.data.set <- rbind(train.data.set,test.data.set)
# Extracting parameters names with mean and std values.
new.features.vector <- EditFeatures(features.vector)
# Creating data set with only mean and std values.
mean.std.data.set <- merged.data.set[, !setdiff(colnames(merged.data.set),c("subject.id", "training.label", new.features.vector)), with=FALSE]
# Creating data set with descriptive activity labels.
descriptive.data.set <- LabelActivitys(mean.std.data.set)
# Create data set with averages by subject and activity.
averages.data.set <- CalculateAverages(descriptive.data.set, new.features.vector)
# Writing summarized data to txt file
write.table(averages.data.set, "./mydata.txt", sep="\t")

