---
title: "Assignment One"
author: "R. Mark Sharp, Ph.D."
date: "May 18, 2014"
output: html_document
---

### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Assignment

This assignment will be described in multiple parts. 
You will need to write a report that answers the questions detailed below. 
Ultimately, you will need to complete the entire assignment in a single R 
markdown document that can be processed by knitr and be transformed into an 
HTML file.

Throughout your report make sure you always include the code that you used to 
generate the output you present. 
When writing code chunks in the R markdown document, always use echo = TRUE 
so that someone else will be able to read the code. 
This assignment will be evaluated via peer assessment so it is essential that 
your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting 
system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. 
You will submit this assignment by pushing your completed files into your 
forked repository on GitHub. 
The assignment submission will consist of the URL to your GitHub 
repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment 
so you do not have to download the data separately.

#### Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

```r
activity_df <- read.csv(file = "activity.csv", header = TRUE)
require(testthat, quietly = TRUE)
expect_equal(nrow(activity_df), 17568)
names(activity_df)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity_df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
require(lubridate, quietly = TRUE)
activity_df$date <- ymd(activity_df$date)
min_date <- min(activity_df$date)
activity_df$day <- (activity_df$date - min_date)/(24 * 60 * 60)
str(activity_df)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     :Class 'difftime'  atomic [1:17568] 0 0 0 0 0 0 0 0 0 0 ...
##   .. ..- attr(*, "tzone")= chr "UTC"
##   .. ..- attr(*, "units")= chr "secs"
```


Process/transform the data (if necessary) into a format suitable for your analysis

#### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.


```r
steps_per_day <- sapply(min(activity_df$day):max(activity_df$day), function(day) {
    sum(activity_df$steps[activity_df$day == day])
})
steps_df <- data.frame(day = min(activity_df$day):max(activity_df$day), steps = steps_per_day)
mean(steps_df$steps, na.rm = TRUE)
```

```
## [1] 10766
```

1. Make a histogram of the total number of steps taken each day

```r
hist(steps_df$steps, xlab = "Total number of steps per day", main = "Histogram of Step Per Day")
```

![plot of chunk histogram-of-total-steps-each-day](figure/histogram-of-total-steps-each-day.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean_number_of_steps <- round(mean(steps_per_day, na.rm = TRUE), 1)
median_number_of_steps <- round(median(steps_per_day, na.rm = TRUE), 1)
```


The mean number of steps per day is 
1.0766 &times; 10<sup>4</sup> and
the median total number of steps per days is 
1.0765 &times; 10<sup>4</sup>.


#### What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)

```r
activity_ts <- ts(activity_df)
plot.ts(activity_df$interval, activity_df$steps, type = "l")
```

![plot of chunk time-series-plot](figure/time-series-plot.png) 

Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
max_steps <- 0
max_step_interval <- 0
intervals <- sort(unique(activity_df$interval))
for (interval in intervals) {
    steps <- mean(activity_df$steps[activity_df$interval == interval], na.rm = TRUE)
    if (steps > max_steps) {
        max_steps <- steps
        max_step_interval <- interval
    }
}
max_step_interval
```

```
## [1] 835
```

```r
max_steps
```

```
## [1] 206.2
```

The the highest average (mean) number of steps over all days is 
206.1698 that occurs in interval 835.

#### Imputing missing values

Note that there are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some 
calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)

```r
all_rows <- nrow(activity_df)
complete_rows <- nrow(activity_df[complete.cases(activity_df), ])
rows_with_NAs <- all_rows - complete_rows
rows_with_NAs
```

```
## [1] 2304
```

There are 2304 rows with NAs.

2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc.

```r
intervals <- sort(unique(activity_df$interval))
num_intervals <- length(intervals)
means_of_intervals <- numeric(num_intervals)
i <- 0
for (interval in intervals) {
    steps <- mean(activity_df$steps[activity_df$interval == interval], na.rm = TRUE)
    i <- i + 1
    means_of_intervals[i] <- steps
}
```

means_of_intervals has values to be used to replace NAs

2. Create a new dataset that is equal to the original dataset but with the 
missing data filled in.

```r
# for (interval in intervals) { for (step in
# seq_along(activity_df$steps[activity_df$interval == interval])) if
# (is.na(activity_df$step[activity_df$interval == interval][step])) {
# activity_df$step[activity_df$interval == interval][step] <-
# means_of_intervals[interval] } }
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


