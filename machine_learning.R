library(RCurl)
training_URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training_file <- getURL(training_URL)
training_data <- read.csv(text= training_file, header=TRUE)

testing_URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testing_file <- getURL(testing_URL)
testing_data <- as.data.frame(read.csv(text= testing_file, header=TRUE))

library(caret)
library(kernlab)

# create some features
add_mag = function(data) {
  
  data <- transform(data, accel_arm_mag = sqrt(accel_arm_x^2 + accel_arm_y^2 + accel_arm_z^2))
  data <- transform(data, magnet_arm_mag = sqrt(magnet_arm_x^2 + magnet_arm_y^2 + magnet_arm_z^2))
  data <- transform(data, gyros_arm_mag = sqrt(gyros_arm_x^2 + gyros_arm_y^2 + gyros_arm_z^2))
  
  
  data <- transform(data, accel_dumbbell_mag = sqrt(accel_dumbbell_x^2 + accel_dumbbell_y^2 + accel_dumbbell_z^2))
  data <- transform(data, magnet_dumbbell_mag = sqrt(magnet_dumbbell_x^2 + magnet_dumbbell_y^2 + magnet_dumbbell_z^2))
  data <- transform(data, gyros_dumbbell_mag = sqrt(gyros_dumbbell_x^2 + gyros_dumbbell_y^2 + gyros_dumbbell_z^2))
  
  
  data <- transform(data, accel_forearm_mag = sqrt(accel_forearm_x^2 + accel_forearm_y^2 + accel_forearm_z^2))
  data <- transform(data, magnet_forearm_mag = sqrt(magnet_forearm_x^2 + magnet_forearm_y^2 + magnet_forearm_z^2))
  data <- transform(data, gyros_forearm_mag = sqrt(gyros_forearm_x^2 + gyros_forearm_y^2 + gyros_forearm_z^2))
  return(data)
}

testing_data <- add_mag(testing_data)
training_data <- add_mag(training_data)

# Must get rid of columns with NAs so that machine learning algorithm doesn't crash
remove_na = function(data) {
  return(data[, colSums(is.na(data)) != nrow(data)])
}
testing_data <- remove_na(testing_data)
columns <- colnames(testing_data)
remove <- c("problem_id", "X")
columns <- columns[! columns %in% remove]
training_data <- training_data[,c(columns, "classe")]

# Slice the data so we get an estimate on how well our model is performing
inTrain <- createDataPartition(y=training_data$classe, p=0.75, list=FALSE)
training_data.train <- training_data[inTrain,]
training_data.test <- training_data[-inTrain,]


