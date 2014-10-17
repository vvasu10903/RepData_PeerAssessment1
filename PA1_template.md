---
title: "Reproducible Research - Peer Assessment 1"
author: "Vinay Vasudev"
date: "2014-10-16"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Reproducible Research - Peer Assessment 1}
  %\VignetteEngine{knitr::rmarkdown}
  %\usepackage[utf8]{inputenc}
---

## Synopsis

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

## Questions

1. Load the data into a data frame from the provided csv file and explore the structure and various column heading. csv file was already unzipped and available in the source directory.


```r
activityData <- read.csv("activity.csv", header = TRUE)
head(activityData)
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

2. Only transformation of data was done to convert 5-minute time intervals into their corresponding hour/minute times and store in a separate column alongwith corresponding date. This can be used for showing the steps data in a graph on a hour/minute scale instead of 5-minute intervals.


```r
hours <- activityData$interval %/% 100
minutes <- activityData$interval %% 60
times <- sprintf("%02d:%02d", hours, minutes)
dateTimes <- paste(activityData$date, times)
activityData <- cbind(activityData, dateTimes)
```

3. Calculate the mean total number of steps taken per day. (Ignore the missing values). 


```r
totalStepsPerDay <- aggregate(steps ~ date, data=activityData, FUN=sum)
mean(totalStepsPerDay$steps)
```

```
## [1] 10766
```

Mean total number of steps taken per day are 1.0766 &times; 10<sup>4</sup>.


4. Plot a histogram of the total number of steps taken each day where total steps across all the time intervals in a day were calculated for all the days in the data. 


```r
# totalStepsPerDay <- aggregate(steps ~ date, data = activityData, FUN=sum, na.rm=TRUE)
hist(totalStepsPerDay$steps, breaks = 20, col = "red", main = "Total Number of Steps Taken
     Each Day", xlab = "Total Number of Steps Each Day")
```

![ Histogram of the total number of steps taken each day](figure/unnamed-chunk-4.png) 

5. Calculate and report the mean and median total number of steps taken per day. Mean is 1.0766 &times; 10<sup>4</sup> and median is 10765.


```r
mean(totalStepsPerDay$steps)
```

```
## [1] 10766
```

```r
median(totalStepsPerDay$steps)
```

```
## [1] 10765
```

6. In order to look at the average daily activity pattern, We plot the average number of steps for each interval for a day as a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Pattern on the plot shows the highest amount of activity during the mid morning hours.


```r
meanStepsPerInterval <- aggregate(steps ~ interval, data=activityData, FUN=mean)
plot(steps ~ interval, type = "l", col = "red", ann = F, data=meanStepsPerInterval)
title(main="Average Steps for Each Interval of a Day",
      sub = "5-minute Intervals", ylab = "Average Number of Steps")
```

![ Time Series Plot](figure/unnamed-chunk-6.png) 

7. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxStepsInterval <- with(meanStepsPerInterval, interval[steps== max(steps)])
maxSteps <- max(meanStepsPerInterval$steps)

maxStepsInterval
```

```
## [1] 835
```

```r
maxSteps
```

```
## [1] 206.2
```

Maximum number of steps are 206.1698 during the interval 835 (which represents time around 8:35 in the morning).

8. Imputing missing values

* There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) which comes out to be 2304. We also confirm that these missing values are only in the steps column.


```r
sum(is.na(activityData))
```

```
## [1] 2304
```

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

9. A strategy has been devised for filling in all of the missing values in the dataset by calculating the means of all the daily 5-minute intervals across all days and then  filling in the mean values in the respective intervals where values are missing.

* A new dataset, "activityDataNew"", is created that is equal to the original dataset but with the missing data values filled in.


```r
activityDataNew <- merge(activityData, meanStepsPerInterval, by = "interval")
colnames(activityDataNew)[2] <- "steps"
colnames(activityDataNew)[5] <- "averageSteps"

# Remove missing values (NA) and replace them with the average steps for the
# corresponding interval

activityDataNew$steps[is.na(activityDataNew$steps)] <-
    activityDataNew$averageSteps[is.na(activityDataNew$steps)]                                          
```

10. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

* Mean and Median values are still pretty much the same even after imputing the missing value - Mean = 10766 and Median = 10766. The histogramdiffer from the Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsByDay <- aggregate(steps ~ date, data = activityDataNew, FUN=sum)
hist(totalStepsByDay$steps, breaks = 20, col = "blue", main = "Total Number of Steps Taken
     Each Day", xlab = "Total Number of Steps Each Day")
```

![ Histogram of the total number of steps taken each day](figure/unnamed-chunk-10.png) 

```r
mean(totalStepsByDay$steps)
```

```
## [1] 10766
```

```r
median(totalStepsByDay$steps)
```

```
## [1] 10766
```

11. Are there differences in activity patterns between weekdays and weekends?

* A new column is added to the data frame "activityDataNew" based on the date value. Based on the date, we determine the day of the week and add this to a column named "dayName".
* Based on the name of the day in "dayName" column, we determine whether it is a weekday or weekend day and then add "weekend" or "weekday" to the column named "weekDayEnd".


```r
activityDataNew[ , "weekDayEnd"] <- as.factor(ifelse(!weekdays(as.Date(activityDataNew$date,
        '%Y-%m-%d')) %in% c("Saturday", "Sunday"), "weekday","weekend"))

averageStepsByWeekEndDay <- aggregate(averageSteps ~ weekDayEnd * interval, data = activityDataNew,
        FUN=sum)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). We use the ggplot2 to create the multi-panel plot.


```r
library(ggplot2)
myGG <- qplot(interval, averageSteps, data=averageStepsByWeekEndDay,
           facets = .~weekDayEnd, col=weekDayEnd, main =
           "Panel Plot of Average Steps for Each Interval \n by WeekDays and Weekends")
myGG + geom_line(size=1)
```

![Lattice Plot of Average Steps for Each Interval by Weekdays and Weekends](figure/unnamed-chunk-12.png) 