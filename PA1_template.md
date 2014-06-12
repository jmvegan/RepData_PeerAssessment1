Reproducible Research Assignment 1 
Steps Data from a monitoring device : Jose Maria Veganzones
==============================================================
- The data consists of two months of data from an anonymous individual collected during the months of October and November in 2012 and includes the number of steps taken in 5 minute intervals each day. dataset file: activity.csv

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken


## Loading and preprocessing the data:

I load the data reading the data file activity.csv, and assign a NA value to all missing values, and finally I save it in a rStepData variable:

```r
rowStepData <-  read.csv("./activity.csv", header = TRUE, na.strings="NA")
```

I check the reading watching the first values

```r
head(rowStepData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

I check if the variable 'date' has correct values:

```r
class(rowStepData$date)
```

```
## [1] "factor"
```

But unfortunally the variable 'date' has been read as a Factor, so I have to convert it into a Date class:

```r
rowStepData$date <- as.Date(rowStepData$date, format="%Y-%m-%d")
```

I check the variable 'date':

```r
class(rowStepData$date)
```

```
## [1] "Date"
```
Finally the variable 'date' has a Date class!

I calculate how many missing value are in the dataset:

```r
sum(is.na(rowStepData$step))
```

```
## [1] 2304
```

I create a new dataset removing all missing values:

```r
ignored_NA_StepData <- rowStepData[!is.na(rowStepData$step),]
```
I check again the first values:

```r
head(ignored_NA_StepData)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```


## What is mean total number of steps taken per day?

In order to calculate the mean of steps per day, ignoring missing values values, I have to use the new dataset:
- First I split the dataset into diferent days:

```r
splitDataByDate <- split(ignored_NA_StepData$steps, ignored_NA_StepData$date)
```
- 2nd, I create a vector that contains the days in the dataset:

```r
allDays <- as.Date(sort(unique(ignored_NA_StepData$date)), format = "%Y-%m-%d")
```

- Finally I can calculate the sum, mean and median of all step, splitted by day:


```r
sumStepByDate <- as.data.frame(sapply(splitDataByDate, sum))
names(sumStepByDate) <- "Steps"
mean(sumStepByDate$Steps)
```

```
## [1] 10766
```

```r
median(sumStepByDate$Steps)
```

```
## [1] 10765
```

I make a histogram of the total number of steps taken each day so I sum the steps by day: 

```r
hist(sumStepByDate$Steps, main="Total steps by day frecuency", xlab="Number of steps by day", ylab="Number of days", col="red", breaks = nrow(sumStepByDate))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
Save the histogram 'plot1.png' that contains the Total steps by day frecuency:

```r
png(filename='plot1.png', width=480, height=480, units="px")
hist(sumStepByDate$Steps, main="Total steps by day frecuency", xlab="Number of steps by day", ylab="Number of days", col="red", breaks = nrow(sumStepByDate))
dev.off()
```

```
## pdf 
##   2
```

I calculate and create the file 'figures/plot2.png' which contains the mean and median total number of steps taken per day in the dataset:


```r
plot( allDays, sumStepByDate$Steps, type="l", ylab="Sum of step", xlab="Day")
lines(allDays, rep(median(sumStepByDate$Steps), length(allDays)), type="l", col="blue")
lines(allDays, rep(mean(sumStepByDate$Steps), length(allDays)), type="l", col="red")
legend("topright", c("Mean", "Median"), lty=1, col=c( "red", "blue"), cex=0.95)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


```r
png(filename='plot2.png', width=480, height=480, units="px")
plot( allDays, sumStepByDate$Steps, type="l", ylab="Sum of step", xlab="Day")
lines(allDays, rep(median(sumStepByDate$Steps), length(allDays)), type="l", col="blue")
lines(allDays, rep(mean(sumStepByDate$Steps), length(allDays)), type="l", col="red")
legend("topright", c("Mean", "Median"), lty=1, col=c( "red", "blue"), cex=0.95)
dev.off()
```

```
## pdf 
##   2
```

The median line and the mean have similar values, so the median line it's not easy to be seen:

Mean:

```r
mean(sumStepByDate$Steps)
```

```
## [1] 10766
```
Median:

```r
median(sumStepByDate$Steps)
```

```
## [1] 10765
```

# What is the average daily activity pattern?

I make an Interval segmentation:

```r
allIntervals <- sort(unique(ignored_NA_StepData$interval))
splitDataByInterval <- split(ignored_NA_StepData$steps, ignored_NA_StepData$interval)
```

I calculate the mean for each interval:

```r
meanStepByInterval <- as.data.frame(sapply(splitDataByInterval, mean))
names(meanStepByInterval) <- c("Steps")
```

I draw the solution and save it as 'plot3.png':

```r
plot( allIntervals, meanStepByInterval$Steps, type="l", main="5-Interval step mean", xlab="5-interval", ylab="Mean of steps")
lines(allIntervals, rep( max(meanStepByInterval$Steps), length(allIntervals)), type="l", col="red")
legend("topright", c("Max"), lty=1, col=c( "red"), cex=0.95)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 


```r
png(filename='plot3.png', width=480, height=480, units="px")
plot( allIntervals, meanStepByInterval$Steps, type="l", main="5-Interval step mean", xlab="5-interval", ylab="Mean of steps")
lines(allIntervals, rep( max(meanStepByInterval$Steps), length(allIntervals)), type="l", col="red")
legend("topright", c("Max"), lty=1, col=c( "red"), cex=0.95)
dev.off()
```

```
## pdf 
##   2
```
       
The Maximum average value of steps in the dataset with <b>835</b> 5-minutes interval is: 

```r
max(meanStepByInterval$Steps)
```

```
## [1] 206.2
```


# Imputing missing values

I complete the missing dates in the raw data with our dataset rawData

```r
simStepData <- rowStepData
```

I calculate the means' list splitted by date:

```r
meanSByDate <- sapply(splitDataByDate, mean)
```

I fill in the NA values with the mean of the day:

```r
for(i in 1:nrow(simStepData)){
      if  (is.na(simStepData[i,1])){
              date_i <- simStepData[i,2]
              simStepData[i,1] <- meanSByDate[[as.factor(date_i)]]
      }
}
```

Using the new dataset, I repeat the same process to obtain the graphics:

```r
splitSimulatedDataByDate <- split(simStepData$steps, simStepData$date)

sumSimulaStepByDate <- as.data.frame(sapply(splitSimulatedDataByDate, sum))
names(sumSimulaStepByDate) <- c("Steps")
mean(sumSimulaStepByDate$Steps)
```

```
## [1] 9371
```

```r
median(sumSimulaStepByDate$Steps)
```

```
## [1] 10395
```
The mean and the median decreased compared with the estimates from the first part of the assignmen. The explanation is because the large amount of 0's that there are now and it makes the mean and median decrease.

I draw the solution:

```r
hist(sumSimulaStepByDate$Steps, main="Total steps by day frecuency", xlab="Number of steps by day", ylab="Number of days", col="red", breaks = nrow(sumSimulaStepByDate))
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27.png) 


```r
png(filename='plot4.png', width=480, height=480, units="px")
hist(sumSimulaStepByDate$Steps, main="Total steps by day frecuency", xlab="Number of steps by day", ylab="Number of days", col="red", breaks = nrow(sumSimulaStepByDate))
dev.off()
```

```
## pdf 
##   2
```
 
 

```r
allSimDays <- as.Date(sort(unique(simStepData$date)), format = "%Y-%m-%d")
```


```r
plot( allSimDays, sumSimulaStepByDate$Steps, type="l",ylab="Sum of step", xlab="Day")
lines(allSimDays, rep(median(sumSimulaStepByDate$Steps), length(allSimDays)), type="l", col="blue")
lines(allSimDays, rep(mean(sumSimulaStepByDate$Steps), length(allSimDays)), type="l", col="red")
legend("topright", c("Mean", "Median"), lty=1, col=c( "red", "blue"), cex=0.95)
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30.png) 


```r
png(filename='plot5.png', width=480, height=480, units="px")
plot( allSimDays, sumSimulaStepByDate$Steps, type="l",ylab="Sum of step", xlab="Day")
lines(allSimDays, rep(median(sumSimulaStepByDate$Steps), length(allSimDays)), type="l", col="blue")
lines(allSimDays, rep(mean(sumSimulaStepByDate$Steps), length(allSimDays)), type="l", col="red")
legend("topright", c("Mean", "Median"), lty=1, col=c( "red", "blue"), cex=0.95)
dev.off()
```

```
## pdf 
##   2
```

# Are there differences in activity patterns between weekdays and weekends?
 

I create a new variable in the dataset called 'dateWeek' to put in the day of the week using weekdays() function:

```r
simStepData$dateWeek <- weekdays(simStepData$date)
```
I can compare if dateWeek is 'saturday' or 'sunday' so that I can put the correct factor ('Weekend' or 'Weekday') depending on it.

I set my language to (US) English:

```r
for(i in 1: nrow(simStepData)){
        if(simStepData[i,4]=="domingo" | simStepData[i,4]=="sábado"){
                simStepData[i,4] <- "Weekend"    
        }else{
                simStepData[i,4] <- "Weekday"    
        }
}
```

I split the dataset by dateWeek and then separate intotwo diferent datasets:

```r
weekDaySplit <- split(simStepData, simStepData$dateWeek)
weekendData <- weekDaySplit[["Weekend"]]
weekdayData <- weekDaySplit[["Weekday"]]
```


## First Method: basic system graph

I create a new split in both datasets by Interval:


```r
allweekendIntervals <- sort(unique(weekendData$interval))
splitweekendDataByInterval <- split(weekendData$steps, weekendData$interval)

allweekdayIntervals <- sort(unique(weekdayData$interval))
splitweekdayDataByInterval <- split(weekdayData$steps, weekdayData$interval)
```

I calculate the mean of steps for each interval:

```r
meanWeekendStepByInterval <- as.data.frame(sapply(splitweekendDataByInterval, mean))
names(meanWeekendStepByInterval) <- c("Steps")

meanWeekdayStepByInterval <- as.data.frame(sapply(splitweekdayDataByInterval, mean))
names(meanWeekdayStepByInterval) <- c("Steps")
```

I draw the solution: 

Weekend

```r
plot( allIntervals, meanWeekendStepByInterval$Steps, type="l",col="blue", main="Weekend. steps mean", xlab="5-interval", ylab="Mean of steps")
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37.png) 


```r
png(filename='plot6.png', width=480, height=480, units="px")
plot( allIntervals, meanWeekendStepByInterval$Steps, type="l",col="blue", main="Weekend. steps mean", xlab="5-interval", ylab="Mean of steps")
dev.off()
```

```
## pdf 
##   2
```

Weekday

```r
plot( allIntervals, meanWeekdayStepByInterval$Steps, type="l", col="blue", main="Weekday. steps mean", xlab="5-interval", ylab="Mean of steps")
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-39.png) 


```r
png(filename='plot7.png', width=480, height=480, units="px")
plot( allIntervals, meanWeekdayStepByInterval$Steps, type="l", col="blue", main="Weekend. steps mean", xlab="5-interval", ylab="Mean of steps")
dev.off()
```

```
## pdf 
##   2
```


## Second Method: lattice system graph



I create a dataframe which contains the mean step grouped by Interval and the factor Weekday or Weekend:


```r
weInterval <- unique(sort(weekendData$interval))
splitweekendDataIntervals <- split(weekendData$steps, weInterval)
meanSplitweekendDataIntervals <- as.data.frame(sapply(splitweekendDataIntervals, mean))
names(meanSplitweekendDataIntervals) <- "meanSteps"
wEMatrixValues <- as.data.frame(cbind(meanSplitweekendDataIntervals$meanSteps, weInterval, "Weekend"))
names(wEMatrixValues) <- c("meanSteps", "interval", "weekdate")

wdInterval <- unique(sort(weekdayData$interval))
splitweekdayDataIntervals <- split(weekdayData$steps, wdInterval)
meanSplitweekdayDataIntervals <- as.data.frame(sapply(splitweekdayDataIntervals, mean))
names(meanSplitweekdayDataIntervals) <- "meanSteps"
wDMatrixValues <- as.data.frame(cbind(meanSplitweekdayDataIntervals$meanSteps, wdInterval, "Weekday"))
names(wDMatrixValues) <- c("meanSteps", "interval", "weekdate")

meanStepMatrix <- as.data.frame(rbind(wDMatrixValues,wEMatrixValues))
meanStepMatrix$meanSteps <- as.numeric(as.character(meanStepMatrix$meanSteps))
meanStepMatrix$interval <- as.numeric(as.character(meanStepMatrix$interval))
```

I check the firts values of my final dataframe:

```r
head(meanStepMatrix)
```

```
##   meanSteps interval weekdate
## 1    2.0806        0  Weekday
## 2    0.4583        5  Weekday
## 3    0.2139       10  Weekday
## 4    0.2361       15  Weekday
## 5    0.1472       20  Weekday
## 6    1.3694       25  Weekday
```

I print it with the 'lattice graph system':
 - First I create and print it

```r
library("lattice")
 p <- xyplot(meanStepMatrix$meanSteps ~ meanStepMatrix$interval | meanStepMatrix$weekdate, meanStepMatrix, layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps")
print(p)
```

![plot of chunk unnamed-chunk-43](figure/unnamed-chunk-43.png) 
  - I save it as 'plot8.png':

```r
png(filename = 'plot8.png', width=480, height=480, units="px")
print(p)
dev.off()
```

```
## pdf 
##   2
```
