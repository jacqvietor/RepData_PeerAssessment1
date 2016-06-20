setwd("D:/gitdir/ReproducibleResearch/Project1/RepData_PeerAssessment1")
activity <- read.csv("D:/gitdir/ReproducibleResearch/Project1/RepData_PeerAssessment1/activity/activity.csv")
nrow(activity)

## Convert DateString to POSIXlt format
activity$tmp <- strptime(activity$date,"%Y-%m-%d")

## create new date variable $dt that contains the measurement date in format yyyymmdd
activity$dt <- format(activity$tmp,"%Y%m%d")


stepsPerDay <- as.vector(aggregate(steps ~ dt, activity, sum))
sum(stepsPerDay$steps)
barplot(stepsPerDay$steps, names.arg=stepsPerDay$dt, xlab="Date", ylab="total # steps per day")

mean(stepsPerDay$steps)
median(stepsPerDay$steps)

table(activity$interval)

stepsPerInterval <- as.vector(aggregate(steps ~ interval, activity, sum))
stepsPerInterval$avgSteps <- (stepsPerInterval$steps)/nrDays
sum(stepsPerInterval$steps)
UniqueDays <- unique(activity$dt)
nrDays <- length(UniqueDays)
barplot(stepsPerInterval$avgSteps, names.arg=stepsPerInterval$interval, xlab="Interval", ylab="avg # steps per day")
maxAvg <- max(stepsPerInterval$avgSteps)
maxRec <- stepsPerInterval[stepsPerInterval$avgSteps == max(stepsPerInterval$avgSteps),]
maxRec$interval

NAsteps <- is.na(activity$steps)
NAsteps
NArecords <- activity[NAsteps,]
nrow(NArecords)

OKrecords <- activity[!NAsteps,]
nrow(OKrecords)

newRecords <- merge(NArecords,stepsPerInterval, by="interval")
newActivity <- newRecords[,c(7,3,1,4,5)]
names(newActivity)[1] <- "steps"

OKrecords$steps <- as.numeric(OKrecords$steps)

newActivity <- rbind(newActivity,OKrecords)

newStepsPerDay <- as.vector(aggregate(steps ~ dt, newActivity, sum))
barplot(newStepsPerDay$steps, names.arg=newStepsPerDay$dt, xlab="Date", ylab="total # steps per day")

newActivity$daynr <- newActivity$tmp$wday
table(newActivity$daynr)
weekend <- newActivity[newActivity$daynr==0 | newActivity$daynr==6 ,]
weekendSPI <- as.vector(aggregate(steps ~ interval, weekend, sum))
weekendSPI$avgSteps <- (weekendSPI$steps)/nrDays
weekendSPI$day <- "weekend"

weekday <- newActivity[newActivity$daynr>0 & newActivity$daynr<6 ,]
weekdaySPI <- as.vector(aggregate(steps ~ interval, weekday, sum))
weekdaySPI$avgSteps <- (weekdaySPI$steps)/nrDays
weekdaySPI$day <- "weekday"

ActivityWeek <- rbind(weekendSPI,weekdaySPI)
library(lattice)
xyplot(avgSteps ~ interval| day, data=ActivityWeek, layout = c(1,2))
