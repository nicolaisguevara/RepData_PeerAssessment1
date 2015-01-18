# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
options(scipen = 1, digits = 2)
# Checking the zip file is already there 
if (!file.exists("activity.zip")) {
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
method = "curl", destfile = "activity.zip", quiet = TRUE)
}
# Unzipping the zip file
unzip(zipfile = "activity.zip", exdir = "./")
# Reading the csv file
data <- read.csv("./activity.csv",header = TRUE)
```


## What is mean total number of steps taken per day?


```r
library(plyr)
datanoNA <- na.omit(data)
stepsPerDay <- ddply(datanoNA,~date,summarise,sum(steps))
hist(stepsPerDay[,2],xlab ="Total number of steps taken per day",main = " ")
```

![](PA1_template_files/figure-html/MeanStep-1.png) 

```r
meanStepsPerDay <- mean(stepsPerDay[,2])
medianStepsPerDay <- median(stepsPerDay[,2])
```

- The mean total number of steps taken per day is 10766.19.
- The median total number of steps taken per day is 10765.





## What is the average daily activity pattern?

```r
n <- c(1:53)
stepsPerDay["newcolumn"] <- n
plot(stepsPerDay[,3],stepsPerDay[,2],type='l',ylab="Average Number of Steps per Day",xlab="Number of Days")
```

![](PA1_template_files/figure-html/AveragePattern-1.png) 

```r
stepsPerDaywithNA <- ddply(data,~date,summarise,sum(steps))
totaldays <- nrow(stepsPerDaywithNA) 
sumStepswithNA <- sum(is.na(stepsPerDaywithNA))
```

Note that we do not have the original total of days (i.e, 61) in our plot. Since we removed the missing values, in total 8 days, we ended up only with 53 days.



```r
max <- max(stepsPerDay[,2])
maximum <- stepsPerDay[which(stepsPerDay[,2]==max),]
```
The 5-minute interval number 47, on average across all the days in the dataset, contains the maximum number of steps. This maximum number of steps is 21194 and corresponds to the day 2012-11-23.

## Imputing missing values


```r
# Checking missing values
if (any(is.na(data))) {
sumNA <- sum(is.na(data))
colSums(is.na(data))
# Average of step in the dataset
averageSteps <- round(mean(data[,1],na.rm=TRUE))
# Imputing NA by the average of steps in the dataset
dataImputingNA <- data
dataImputingNA[which(is.na(dataImputingNA[,1])),] [,1] <- averageSteps
}
```
We have in our data 2304 missing values.

Strategy to fill out the missing values using: 
Since the missing values appear all together on a particular day we can not use the mean/median for this day. Here I am imputing the missing values by the average of steps in the dataset. The new dataset with the missing data filled in is called "dataImputingNA".


```r
stepsPerDayImp <- ddply(dataImputingNA,~date,summarise,sum(steps))
hist(stepsPerDayImp[,2],xlab ="Total number of steps taken per day",main = "Histogram Imputing Missing Values ")
```

![](PA1_template_files/figure-html/MeanStepImp-1.png) 

```r
meanStepsPerDayImp <- mean(stepsPerDayImp[,2])
medianStepsPerDayImp <- median(stepsPerDayImp[,2])
```

- The mean total number of steps taken per day after imputing missing values in the original dataset is 10751.74.
- The median total number of steps taken per day after imputing missing values in the original dataset is 10656.

Here we can compare with the mean and median using the original dataset removing the missing values:

- The mean total number of steps taken per day is 10766.19.
- The median total number of steps taken per day is 10765.

After comparing both values we can observe that they are different and for both cases, the mean and the median, imputing the missing value produces a decreasing in these values: the mean going from 10766.19 to 10751.74 and the median going from 10765 to 10656.


## Are there differences in activity patterns between weekdays and weekends?

```r
#Creating a new factor variable with two levels: "weekday" and "weekend"
# First I create a new dataset
dataImputingNAnew <- dataImputingNA
dataImputingNAnew$day <- weekdays(as.Date(dataImputingNAnew$date))
# I replace the weekend days by "weekend" word 
dataImputingNAnew$day[which(dataImputingNAnew$day == "Saturday" | dataImputingNAnew$day == "Sunday" )] <- "weekend"
# I Replace the weekdays by "weekday" word 
dataImputingNAnew$day[which(dataImputingNAnew$day == "Monday" | dataImputingNAnew$day == "Tuesday" | dataImputingNAnew$day == "Wednesday" | dataImputingNAnew$day == "Thursday" | dataImputingNAnew$day == "Friday" ) ] <- "weekday"
dataImputingNAnew$day <- as.factor(dataImputingNAnew$day)
# Generating the data for weekend day and weekday
numberWeekday <- dim(dataImputingNAnew[which(dataImputingNAnew$day =="weekend"),])[1]
numberWeekendDays <- dim(dataImputingNAnew[which(dataImputingNAnew$day =="weekday"),])[1]
dataImputingNAWeekend <- dataImputingNAnew[which(dataImputingNAnew$day =="weekend"),]
dataImputingNAWeekDays <- dataImputingNAnew[which(dataImputingNAnew$day =="weekday"),]
stepsPerDayImpWeekend <- ddply(dataImputingNAWeekend,~date,summarise,sum(steps))
stepsPerDayImpWeekday <- ddply(dataImputingNAWeekDays,~date,summarise,sum(steps))
nWeekend <- c(1:nrow(stepsPerDayImpWeekend))
nWeekday <- c(1:nrow(stepsPerDayImpWeekday))

stepsPerDayImpWeekend["newcolumn"] <- nWeekend
stepsPerDayImpWeekday["newcolumn"] <- nWeekday
# Plotting the data
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(stepsPerDayImpWeekend[,3],stepsPerDayImpWeekend[,2],type='l',ylab="Number of Steps",xlab="Number of Days",main= "weekend")
plot(stepsPerDayImpWeekday[,3],stepsPerDayImpWeekday[,2],type='l',ylab="Number of Steps",xlab="Number of Days",main= "weekday")
```

![](PA1_template_files/figure-html/DifferencesActPattern-1.png) 


 Yes, from these plots we note that there are differences in activity patterns between weekdays and weekends. 

