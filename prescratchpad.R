mydata = read.csv("~/Downloads/train.csv", as.is=TRUE);
f <- c(2:1000)

dates <- seq(as.POSIXct("04/01/2012 00:00:00"),
             by = "hours", length = length(f))

mydata["datetime"] <- lapply(mydata["datetime"], as.POSIXct) # parse as date with timestamp

mydata["date"] <- lapply(mydata["datetime"], as.Date) # parse as date without timestamp
mydata["time"] <- as.POSIXlt(mydata$datetime)$hour

 # parse as date without timestamp
mydata.sumByDay <- aggregate(x = mydata$count, FUN = sum, 
                        by = list(Group.date = mydata$date)) #group by date (summing up count)

# parse as just timestamp
mydata.sumByHour <- aggregate(x = mydata$count, FUN = sum, 
                             by = list(Group.date = mydata$time)) #group by hour (summing up count)

# parse as just timestamp
mydata.sumByAtemp <- aggregate(x = mydata$count, FUN = sum, 
                              by = list(atemp = mydata$atemp)) #group by atemp

# parse as just timestamp
mydata.sumByTemp <- aggregate(x = mydata$count, FUN = sum, 
                               by = list(temp = mydata$temp)) #group by atemp
