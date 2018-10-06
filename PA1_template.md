#Coursera / John Hopkins Data Science Specialization#

##Course 5 - Course Project 1##

###Set working directory###

###Set global parameters###


```r
require(knitr)
opts_chunk$set(echo=TRUE)
```

###Loading and preprocessing the data###

Show any code that is needed to
1) Load the data (i.e. read.csv())
2) Process/transform the data (if necessary) into a format suitable for your 
analysis


```r
raw <- read.csv("./data/activity.csv")
head(raw)
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

###PART 1: What is mean total number of steps taken per day?###

For this part of the assignment, you can ignore the missing values in the 
dataset.

1) Calculate the total number of steps taken per day


```r
steps_per_day <- with(raw, tapply(steps, date, sum, na.rm=TRUE))
steps_per_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

2) If you do not understand the difference between a histogram and a barplot,
research the difference between them. Make a histogram of the total number of 
steps taken each day


```r
hist(steps_per_day, 
     main="Total Steps per Day", 
     xlab="Number of Steps per Day", 
     ylab="Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
dev.copy(png,"./figure/Part1_Q2_hist_steps_per_day.png", bg="transparent")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

3) Calculate and report the mean and median of the total number of steps taken 
per day


```r
steps_per_day_mean <- mean(steps_per_day, na.rm=TRUE)
steps_per_day_median <- median(steps_per_day, na.rm=TRUE)
```
Mean of total number of steps per day = 9354.2295082

Median of total number of steps per day = 10395

###PART 2: What is the average daily activity pattern?###

1) Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)


```r
avg_steps_per_interval <- with(raw, tapply(steps, interval, mean, na.rm=TRUE))
plot(levels(factor(raw$interval)), 
     avg_steps_per_interval,
     type="l",
     main="Time-series Plot of Average Steps Taken",
     xlab="5-min Interval",
     ylab="Average Number of Steps (Averaged Across Days)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
dev.copy(png,"./figure/Part2_Q1_time_steps_per_interval.png", bg="transparent")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

2) Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?


```r
# Find index corresponding to the maximum average number of steps (averaged)
index <- which.max(avg_steps_per_interval)
# Map index back to the interval
levels(factor(raw$interval))[index]
```

```
## [1] "835"
```

###PART 3: Imputing missing values###

Note that there are a number of days/intervals where there are missing values 
(coded as NA). The presence of missing days may introduce bias into some 
calculations or summaries of the data.

1) Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)


```r
# Evaluate number of NA entries down all columns in raw data
colSums(is.na(raw))
```

```
##    steps     date interval 
##     2304        0        0
```

2) Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Strategy 1: Use mean for that particular day to fill in missing values
# Calculate mean of steps for each day
#   mean_steps <- with(raw, tapply(steps, date, mean, na.rm=TRUE))
# Check to see if there are still NAs
#   sum(is.na(mean_steps)) 
# There are still NA values in the resulting mean vector

# Strategy 2: Use mean for that 5-minute interval across all dates
sum(is.na(avg_steps_per_interval))
```

```
## [1] 0
```
Strategy 1 is not good as there are still NA values in the "mean for a 
particular day".

Strategy 2 is good because there are no NA values in the "mean for a particular
5-minute interval".

3) Create a new dataset that is equal to the original dataset but with the 
missing data filled in.


```r
raw_na_filled <- raw
for (i in 1:nrow(raw)) {
    if(is.na(raw[i,1])) {
        if ((i %% length(avg_steps_per_interval)) == 0) {
            j <- length(avg_steps_per_interval)
        } else {
            j <- i %% length(avg_steps_per_interval)
        }
        raw_na_filled[i,1] <- 
            avg_steps_per_interval[[j]]
    }
}
# Check if there are any missing data left
colSums(is.na(raw_na_filled))
```

```
##    steps     date interval 
##        0        0        0
```

4) Make a histogram of the total number of steps taken each day. Calculate 
and report the mean and median total number of steps taken per day. Do these 
values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total 
daily number of steps?


```r
steps_per_day_na_filled <- with(raw_na_filled, 
                                tapply(steps, date, sum, na.rm=TRUE))
hist(steps_per_day_na_filled, 
     main="Total Steps per Day", 
     xlab="Number of Steps per Day", 
     ylab="Frequency")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
dev.copy(png,"./figure/Part3_Q4_hist_steps_per_day.png", bg="transparent")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```


```r
steps_per_day_na_filled_mean <- mean(steps_per_day_na_filled, na.rm=TRUE)
steps_per_day_na_filled_median <- median(steps_per_day_na_filled, na.rm=TRUE)
```
Mean of total number of steps per day = 9354.2295082

Mean of total number of steps per day (NA entries filled) 
= 1.0766189 &times; 10<sup>4</sup>

Median of total number of steps per day = 10395

Median of total number of steps per day (NA entries filled) 
=1.0766189 &times; 10<sup>4</sup>

*Results:* Inputing missing data for steps increases both the mean and median of
the daily number of steps.

###PART 4: Are there differences in activity patterns between weekdays and### 
###weekends?###

For this part the weekdays() function may be of some help here. Use the dataset
with the filled-in missing values for this part.

1) Create a new factor variable in the dataset with two levels - "weekday" and
"weekend" indicating whether a given date is a weekday or weekend day.


```r
for (i in 1:nrow(raw_na_filled)) {
    if ((weekdays(as.Date(as.character(raw_na_filled[i,2]))) == "Saturday") | 
        (weekdays(as.Date(as.character(raw_na_filled[i,2]))) == "Sunday")) {
            raw_na_filled$weekdays[i] <- "weekend"
    }
    else {
        raw_na_filled$weekdays[i] <- "weekday"
    }
}
raw_na_filled$weekdays <- factor(raw_na_filled$weekdays)
head(raw_na_filled)
```

```
##       steps       date interval weekdays
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

2) Make a panel plot containing a time series plot (i.e. type="l") of the 
5-minute interval (x-axis) and the average number of steps taken, averaged 
across all weekday days or weekend days (y-axis). See the README file in the 
GitHub repository to see an example of what this plot should look like using 
simulated data.


```r
library(lattice)
# Hypothetically set y-axis max to be the max of all averages (for each
#   interval) + 50
ymax <- with(raw_na_filled, tapply(steps, interval, mean, na.rm=TRUE)) + 50
xyplot(steps ~ interval | weekdays,
       data=raw_na_filled,
       type="a",
       layout=c(1,2),
       main="Time-series Plot of Average Steps Taken",
       xlab="5-min Interval",
       ylab="Average Number of Steps (Averaged Across Days)",
       ylim=c(0,ymax))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
dev.copy(png,"./figure/Part4_Q2_panel_steps_per_interval.png", bg="transparent")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

<!-- Discarded Analysis -->

<!-- # New var concatenating date and interval columns  -->
<!-- date_time <- with(raw,  -->
<!--                   paste(date, formatC(interval, width=4, flag="0"), sep=" ")) -->
<!-- # Re-format into a POSIXlt object for plotting (x-axis) -->
<!-- date_time1 <- strptime(date_time, "%Y-%m-%d %H%M") -->
<!-- # Generate time-series plot -->
<!-- plot(date_time1, raw$steps,  -->
<!--      type="l", -->
<!--      main="Time Series Plot of Steps vs Date-Time", -->
<!--      xlab="Date (yyyy-mm-dd) & Time (hh:mm)", -->
<!--      ylab="Steps") -->

<!-- # Create "weekend" and "weekday" subsets from "raw_na_filled" -->
<!-- library(dplyr) -->
<!-- weekend_data <- filter(raw_na_filled, weekdays == "weekend") -->
<!-- weekday_data <- filter(raw_na_filled, weekdays == "weekday") -->
<!-- # Create time-series panel plot ("weekend" at the top, "weekday" at the bottom) -->
<!-- avg_steps_per_interval_weekend <- -->
<!--     with(weekend_data, tapply(steps, interval, mean, na.rm=TRUE)) -->
<!-- avg_steps_per_interval_weekday <- -->
<!--     with(weekday_data, tapply(steps, interval, mean, na.rm=TRUE)) -->
<!-- par(mfrow=c(2,1)) -->
<!-- with(weekend_data, -->
<!--      plot(levels(factor(interval)), -->
<!--           avg_steps_per_interval_weekend, -->
<!--           type="l", -->
<!--           main="Time-series Plot of Average Steps Taken (Weekend)", -->
<!--           xlab="5-min Interval", -->
<!--           ylab="Average Number of Steps (Averaged Across Days)")) -->
<!-- with(weekday_data, -->
<!--      plot(levels(factor(interval)), -->
<!--           avg_steps_per_interval_weekday, -->
<!--           type="l", -->
<!--           main="Time-series Plot of Average Steps Taken (Weekday)", -->
<!--           xlab="5-min Interval", -->
<!--           ylab="Average Number of Steps (Averaged Across Days)")) -->
