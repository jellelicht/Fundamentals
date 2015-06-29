setwd("C:\\Users\\Rafik\\Downloads\\Data_project")

train <- read.csv("train.csv")
test <- read.csv("test.csv")



test$count <- NA

combi <- rbind(train[1:9],test[1:9])


names <- c("season", "holiday", "workingday", "weather")
combi[,names] <-lapply(combi[,names], factor)

combi$datetime <- as.character(combi$datetime)
combi$datetime <- strptime(combi$datetime, format="%Y-%m-%d %T",tz="EST")

combi$hour <- as.integer(substr(combi$datetime, 12, 13))
combi$hour <- as.factor(combi$hour)

combi$weekday <- as.factor(weekdays(combi$datetime))
combi$weekday <- factor(combi$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

testTrain <- subset(train, select = -c(datetime, count, registered))
testFit <- randomForest(casual~., data=testTrain, ntree=400, mtry=4, importance=TRUE)

varImpPlot(combi)

