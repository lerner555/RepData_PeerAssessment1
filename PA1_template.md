---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library("data.table")
library(ggplot2)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

Act <- data.table::fread(input = "data/activity.csv")
```

## What is mean total number of steps taken per day?


```r
Total_Steps <- Act[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 

ggplot(Total_Steps, aes(x = steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps Median_Steps
## 1:   10766.19        10765
```


## What is the average daily activity pattern?


```r
Interval <- Act[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(Interval, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Interval[steps == max(steps), .(max_interval = interval)]
```

```
##    max_interval
## 1:          835
```

## Imputing missing values


```r
Act[is.na(steps), .N ]
```

```
## [1] 2304
```

```r
Act[is.na(steps), "steps"] <- Act[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]

data.table::fwrite(x = Act, file = "data/tidyData.csv", quote = FALSE)

Total_Steps <- Act[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)]

Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

```
##    Mean_Steps Median_Steps
## 1:    9354.23        10395
```

```r
ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?


```r
Act <- data.table::fread(input = "data/activity.csv")
Act[, date := as.POSIXct(date, format = "%Y-%m-%d")]
Act[, `Day of Week`:= weekdays(x = date)]
Act[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
Act[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
Act[, `weekday or weekend` := as.factor(`weekday or weekend`)]

Act[is.na(steps), "steps"] <- Act[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
Interval <- Act[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(Interval , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

