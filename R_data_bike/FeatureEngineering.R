#installed.packages(randomForest)
#installed.packages(e1071)
library(randomForest)
library(e1071)
setwd("/home/jelle/Projects/Fundamentals/R_data_bike/")

featureEngineering <- function(df) {
  
  # Save the current system timezone so weekdays returns English weekdays
  curr_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","en_US.UTF-8")
  
  # Factorize season, holiday, workingday and weather for usage in machine learning algorithms
  names <- c("season", "holiday", "workingday", "weather")
  df[,names] <-lapply(df[,names], factor)
  
  # Datetime objects cannot be directly used by machine learning algorithms, 
  # so we have to extract useful(?) predictors from datetime
  df$datetime <- as.character(df$datetime)
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T",tz="EST") # Parse input as datetime object
  
  df$hour <- as.integer(substr(df$datetime, 12, 13))
  df$hour <- as.factor(df$hour)
  
  # Use the English weekdays by default
  df$weekday <- as.factor(weekdays(df$datetime))
  df$weekday <- factor(df$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  df$year <- as.integer(substr(df$datetime,	1,4))	
  df$year	<- as.factor(df$year)	
  
  # Restore the system timezone
  Sys.setlocale("LC_TIME",curr_locale)
  
  return(df)
}


savePredictions <- function(model, testData, fileName){
  # Predict outcomes of testData using the model
  testData$count <- predict(model, testData)
  
  # Round down to nearest integer
  testData$count <- round(testData$count, 0)
  
  # Prepare columns for submission
  submission <- subset(testData, select=c(datetime, count))
  
  # Remove negative numbers: (HAXXORZ)
  submission[,2] <- pmax(submission[,2], 0)
  
  # Write submission data to csv file
  write.csv(submission, file=fileName, row.names=FALSE)
}

main <- function(modelGenerator, modelName){
  # Start by reading a fresh copy of the data
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
  # Factorise data in dataframe
  trainFE <- featureEngineering(train) 
  testFE <- featureEngineering(test)
  
  # Generate the model
  myModel <- modelGenerator(trainFE) # this line changes for different models
  # Use model 
  savePredictions(myModel, testFE, paste(modelName, "Predictions.csv"))
}

generateLinModel <- function(df){
  # A Linear Regression model aims to model the count as a linear combination of the other factors
  testDf <- subset(df, select = -c(datetime, registered, casual)) # Exclude columns that are not predictors  
  testFit <- lm(count~hour + year + temp + humidity, testDf)
  return(testFit)
}

generateSVMModel <- function(df){
  testDf <- subset(df, select = -c(datetime, registered, casual)) # Exclude columns that are not predictors  
  testFit <- svm(count~., testDf)
  #testFit[testFit<0] <- as.integer(0)
  return(testFit)
}

generateForest <- function(df){
  # A Random Forest based model uses decisionTrees to determine which factors influence the count most
  # Set seed to ensure consistent randomTree generation
  set.seed(1234)
  
  myNtree = 50 # Number of trees in forest
  myMtry = 42 # Number of attributes per tree
  myImportance = TRUE # Let the randomForest library determine which attributes are important
  testDf <- subset(df, select = -c(datetime, registered, casual)) # Exclude columns that are not predictors
  
  testFit <- randomForest(count ~., data=testDf, ntree=myNtree, mtry=myMtry, importance=myImportance) 
  return(testFit)
}


# For RandomTree: !!! VERY VERY SLOW
# main(generateForest, "Forest")
# For LM:
# main(generateLinModel, "Linear")
# For SVM:
# main(generateSVMModel, "SVM")



