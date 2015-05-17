---
title: "PA1_template"
author: "Ramesh Annam"
date: "Sunday, May 17, 2015"
output: html_document
---

Loading and preprocessing the data
1.Load the data (i.e. read.csv())
2.Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```


What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day


```r
totalstepsperday <- aggregate(steps ~ date, data, sum)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(totalstepsperday$steps)
```

```
## Error in hist(totalstepsperday$steps): object 'totalstepsperday' not found
```

3.Calculate and report the mean and median of the total number of steps taken per day


```r
summary(totalstepsperday$steps)
```

```
## Error in summary(totalstepsperday$steps): object 'totalstepsperday' not found
```

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgsteps_5min <- aggregate(steps ~ interval, data, mean)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

```r
plot(avgsteps_5min$interval, avgsteps_5min$steps, type="l")
```

```
## Error in plot(avgsteps_5min$interval, avgsteps_5min$steps, type = "l"): object 'avgsteps_5min' not found
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgsteps_5min$interval[avgsteps_5min$steps == max(avgsteps_5min$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'avgsteps_5min' not found
```


Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
isna <- is.na(data$steps)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
temp <- data$interval[isna]
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

```r
library(plyr)
temp <- join(data, avgsteps_5min, by = "interval" )
```

```
## Error in as.vector(x): object 'avgsteps_5min' not found
```

```r
temp$steps2 <- data$steps
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
temp$steps <- NULL
```

```
## Error in temp$steps <- NULL: object 'temp' not found
```

```r
temp$steps2[isna] = round(temp$steps[isna]) 
```

```
## Error in eval(expr, envir, enclos): object 'temp' not found
```

```r
temp$steps3 <- data$steps
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newdata <- NULL
newdata$steps_original <- data$steps
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
newdata$steps_filled <- temp$steps2
```

```
## Error in eval(expr, envir, enclos): object 'temp' not found
```

```r
newdata$steps_avg <- temp$steps
```

```
## Error in eval(expr, envir, enclos): object 'temp' not found
```

```r
newdata$interval <- temp$interval
```

```
## Error in eval(expr, envir, enclos): object 'temp' not found
```

```r
newdata$date <- temp$date
```

```
## Error in eval(expr, envir, enclos): object 'temp' not found
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalstepsperdaynew <- aggregate(steps_filled ~ date, newdata, sum)
```

```
## Error in eval(expr, envir, enclos): object 'steps_filled' not found
```

```r
hist(totalstepsperdaynew$steps_filled)
```

```
## Error in hist(totalstepsperdaynew$steps_filled): object 'totalstepsperdaynew' not found
```

```r
summary(totalstepsperdaynew$steps_filled)
```

```
## Error in summary(totalstepsperdaynew$steps_filled): object 'totalstepsperdaynew' not found
```

The mean and median total number of steps taken per day is same as before. The Quartile distribution
slightly changed though...

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newdata$date2 <- as.Date(newdata$date)
```

```
## Error in as.Date.default(newdata$date): do not know how to convert 'newdata$date' to class "Date"
```

```r
newdata$wk_day_flag <- weekdays(newdata$date2) == "Sunday" | 
        weekdays(newdata$date2) == "Saturday"
```

```
## Error in UseMethod("weekdays"): no applicable method for 'weekdays' applied to an object of class "NULL"
```

```r
newdata$wk_day_factor_var[newdata$wk_day_flag == TRUE] <- "Weekend"

newdata$wk_day_factor_var[newdata$wk_day_flag == FALSE] <- "Weekday"
newdata$wk_day_factor_var <- as.factor(newdata$wk_day_factor_var)
```


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow = c(2,1))

newdata <- data.frame(newdata)
plot_data1 <- newdata[newdata$wk_day_factor_var == "Weekday",]
plot_data2 <- newdata[newdata$wk_day_factor_var == "Weekend",]


avgsteps_5min_1 <- aggregate(steps_filled ~ interval, plot_data1, mean)
```

```
## Error in eval(expr, envir, enclos): object 'steps_filled' not found
```

```r
avgsteps_5min_2 <- aggregate(steps_filled ~ interval, plot_data2, mean)
```

```
## Error in eval(expr, envir, enclos): object 'steps_filled' not found
```

```r
plot(avgsteps_5min_1$interval, avgsteps_5min_1$steps, type="l", main = "Weekday")
```

```
## Error in plot(avgsteps_5min_1$interval, avgsteps_5min_1$steps, type = "l", : object 'avgsteps_5min_1' not found
```

```r
plot(avgsteps_5min_2$interval, avgsteps_5min_2$steps, type="l", main = "Weekend")
```

```
## Error in plot(avgsteps_5min_2$interval, avgsteps_5min_2$steps, type = "l", : object 'avgsteps_5min_2' not found
```

From the above plots, the weekends seem more relaxed with delayed / fewer steps.

