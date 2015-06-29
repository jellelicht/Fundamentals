setwd("C:\\Users\\Rafik\\Downloads\\Data_project")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# pairs(train)

featureEngineering <- function(df) {
  
  
  
  names <- c("season", "holiday", "workingday", "weather")
  df[,names] <-lapply(df[,names], factor)
  
  df$datetime <- as.character(df$datetime)
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T",tz="EST")
  
  df$hour <- as.integer(substr(df$datetime, 12, 13))
  df$hour <- as.factor(df$hour)
  
  df$weekday <- as.factor(weekdays(df$datetime))
  df$weekday <- factor(df$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  
  return(df)
}

train <- featureEngineer(train)
test <- featureEngineer


