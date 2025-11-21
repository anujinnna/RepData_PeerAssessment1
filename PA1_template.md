---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

``` r
steps_per_day <- aggregate(steps ~ date,
                           activity,
                           sum)

hist(steps_per_day$steps,
     main = "Total steps per day",
     xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

``` r
median(steps_per_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

``` r
avg_steps_interval <- aggregate(steps ~ interval,
                                activity,
                                mean,
                                na.rm = TRUE)

plot(avg_steps_interval$interval,
     avg_steps_interval$steps,
     type = "l",
     xlab = "5-minute interval",
     ylab = "Average number of steps",
     main = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Imputing missing values

``` r
total_na <- sum(is.na(activity$steps))
total_na
```

```
## [1] 2304
```

``` r
interval_means <- aggregate(steps ~ interval,
                            activity,
                            mean,
                            na.rm = TRUE)

activity_filled <- activity

nas <- is.na(activity_filled$steps)

activity_filled$steps[nas] <- interval_means$steps[
  match(activity_filled$interval[nas], interval_means$interval)
]
```

``` r
steps_per_day_filled <- aggregate(steps ~ date,
                                  activity_filled,
                                  sum)

hist(steps_per_day_filled$steps,
     main = "Total steps per day (missing values imputed)",
     xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

``` r
mean(steps_per_day_filled$steps)
```

```
## [1] 10766.19
```

``` r
median(steps_per_day_filled$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

``` r
day_names <- weekdays(activity_filled$date)

activity_filled$daytype <- ifelse(day_names %in% c("Saturday", "Sunday"),
                                  "weekend",
                                  "weekday")

activity_filled$daytype <- factor(activity_filled$daytype,
                                  levels = c("weekday", "weekend"))
```

``` r
avg_steps_daytype <- aggregate(steps ~ interval + daytype,
                               activity_filled,
                               mean)
```

``` r
library(lattice)

xyplot(steps ~ interval | daytype,
       data = avg_steps_daytype,
       type = "l",
       layout = c(1, 2),
       xlab = "5-minute interval",
       ylab = "Average number of steps",
       main = "Weekday vs weekend activity patterns")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

