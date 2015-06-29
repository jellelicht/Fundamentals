#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("C:\\Users\\Rafik\\Downloads\\Data_project")

train <- read.csv("train.csv")
test <- read.csv("test.csv")


# pairs(train)

featureEngineering <- function(df) {
  
  # Save the current system timezone so weekdays returns English weekdays
  curr_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","English")
  
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

trainFE <- featureEngineer(train)
#test <- featureEngineer



#testprettrain <- subset(train, select = -c(datetime, registered, casual)) # Exclude columns that are not predictors

#fit <- rpart(count ~ ., data=train, method="class", control=rpart.control(minsplit = 20, minbucket = round(minsplit/3), 
#cp = 0.01, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0, maxdepth = 30, ... = ))
#new.fit <- prp(fit,snip=TRUE)$obj


 
 

#fancyRpartPlot(fit)

#Prediction <- predict(fit, test, type = "class")

formula_cas <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed 

model_count <- rpart(formula_cas, data =trainFE)

#pred_model <- round(predict(model_count, newdata =train.test)^2 + predict(model_count, newdata = train.test)^2)

# RMSLE(train.test, pred_model)
