Reproducible Research: Peer Assessment 1
================

# Loading and preprocessing the data

First we load the data and make a quick data exploration

``` r
setwd("/Users/shameekphukan/Downloads")
activity <- read.csv("./activity.csv")

head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
summary(activity)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

``` r
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
pairs(activity)
```

![](PA1_template_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per
    day

<!-- end list -->

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.3.0     ✔ purrr   0.3.4
    ## ✔ tibble  3.0.1     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.2     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.5.0

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## Warning: package 'purrr' was built under R version 3.6.2

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
stepsPerDay <- activity %>%
        group_by(date) %>%
        summarise(steps = sum(steps, na.rm = T)) %>%
        filter(steps != 0) 
stepsPerDay
```

    ## # A tibble: 53 x 2
    ##    date       steps
    ##    <fct>      <int>
    ##  1 2012-10-02   126
    ##  2 2012-10-03 11352
    ##  3 2012-10-04 12116
    ##  4 2012-10-05 13294
    ##  5 2012-10-06 15420
    ##  6 2012-10-07 11015
    ##  7 2012-10-09 12811
    ##  8 2012-10-10  9900
    ##  9 2012-10-11 10304
    ## 10 2012-10-12 17382
    ## # … with 43 more rows

2.  Make a histogram of the total number of steps taken each day

<!-- end list -->

``` r
hist(stepsPerDay$steps)
```

![](PA1_template_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

3.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- end list -->

``` r
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
```

    ## [1] 10766.19

``` r
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

    ## [1] 10765

# What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = “l”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

<!-- end list -->

``` r
stepsPerInterval <- activity %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = T))

plot(steps~interval,stepsPerInterval, type = "l")
```

![](PA1_template_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of
steps?

<!-- end list -->

``` r
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

    ## [1] 835

# Imputing missing values

1.  Calculate and report the total number of missing values in the
    dataset

<!-- end list -->

``` r
TotalMissingValues <- sum(rowSums(is.na(activity)))
TotalMissingValues
```

    ## [1] 2304

2.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc

<!-- end list -->

``` r
getMeanStepsPerInterval<-function(interval){
        stepsPerInterval[stepsPerInterval$interval==interval,]$steps}
```

3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- end list -->

``` r
activity_noNA <- activity
for(i in 1:nrow(activity_noNA)){
                if(is.na(activity_noNA[i,]$steps)){
                        activity_noNA[i,]$steps <- getMeanStepsPerInterval(activity_noNA[i,]$interval)
                }
        }
head(activity_noNA)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- end list -->

``` r
totalstepsperday <- activity_noNA %>%
        group_by(date) %>%
        summarise(steps = sum(steps))
hist(totalstepsperday$steps)
```

![](PA1_template_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
meantotalsteps <- mean(totalstepsperday$steps)
mediantotalsteps <- median(totalstepsperday$steps)
```

The mean didn’t change after the replacements of NAs, the median changed
about 0.1% of the original
value.

# Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a
    weekday or weekend
day.

<!-- end list -->

``` r
activity_noNA$date <- as.Date(strptime(activity_noNA$date, format="%Y-%m-%d"))
activity_noNA$day <- weekdays(activity_noNA$date)
for (i in 1:nrow(activity_noNA)) {
        if (activity_noNA[i,]$day %in% c("Saturday","Sunday")) {
                activity_noNA[i,]$day<-"weekend"
        }
        else{
                activity_noNA[i,]$day<-"weekday"
        }
}
stepsByDay <- aggregate(activity_noNA$steps ~ activity_noNA$interval + activity_noNA$day, activity_noNA, mean)
```

2.  Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- end list -->

``` r
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
