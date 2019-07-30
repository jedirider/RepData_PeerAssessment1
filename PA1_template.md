---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
#First load the libraries needed for these studies
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
library(ggplot2)
#Load the data file into a data frame
activity <- read.csv("activity.csv")
#Clean the dataset by removing NA values
cleanData <- activity[ with(activity,{!(is.na(steps))}),]
```


## What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken per day
totalSteps <-aggregate(steps ~ date, cleanData, sum)
```


```r
#Create a Histogram of the total steps taken per day
hist(totalSteps$steps, main = "Histogram of Total Steps per Day", xlab = "Number of Steps per Day")
```

![](PA1_template_files/figure-html/chart1-1.png)<!-- -->


```r
#Calculate and report the mean and median of total steps per day
summary(totalSteps)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```
The mean number of steps taken per day is 10,766, and the median is 10,765


## What is the average daily activity pattern?

```r
#Plot time series of the average number of steps taken across all days
meanInterval <- aggregate(steps ~ interval, data = cleanData, FUN="mean")
plot(meanInterval$interval,meanInterval$steps, type ="l", main = "Average Steps over all Days", xlab ="Interval", ylab = "Avg. Number of Steps") 
```

![](PA1_template_files/figure-html/chart2-1.png)<!-- -->


```r
#Find the max number of steps
maxSteps <- which.max(meanInterval$steps)
meanInterval[maxSteps,]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

```r
#Calculate and report missing values
sum(is.na(activity))
```

```
## [1] 2304
```


```r
#Impute data to original dataframe
imputedActivity <- activity
for (i in 1:nrow(imputedActivity)) {
  if (is.na(imputedActivity$steps[i])) {
    intSteps <- imputedActivity$interval[i]
    newSteps <- meanInterval[
      meanInterval$interval == intSteps,]
    imputedActivity$steps[i] <- newSteps$steps
  }
}
#Create new dataset with missing data filled in and make a histogram
imputedActivity_Day <- aggregate(steps ~ date, imputedActivity, sum)
hist(imputedActivity_Day$steps, main = "Histogram of Total Steps per Day - with Imputed Data", xlab = "Number of Steps per Day")
```

![](PA1_template_files/figure-html/chart3-1.png)<!-- -->


```r
#Calculate the mean and median and compare vs. the original data
summary(imputedActivity_Day)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```
Although the mean and median for the imputed activity dataset are only different by ~1-step vs. the original dataset, the 
dispersion is wider as the 1st and 3rd quartiles are larger by ~500-1000 steps.


## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a factor variable in the dataset for the two day types
weekDay <- function(day) {
	days <- weekdays(as.Date(day, "%Y-%m-%d"))
	if(!(days == "Saturday" || days == "Sunday")) {
		week <- "Weekday"
	} else {
		week <- "Weekend"
	}
	week
}
activity$dayWeek <- as.factor(sapply(activity$date, weekDay))
stepsDay <- aggregate(steps ~ interval+dayWeek, activity, FUN = "mean")
#Create a panel plot
ggplot(stepsDay, aes(interval, steps)) + geom_line(stat = "identity", aes(color = dayWeek))+facet_grid(dayWeek~., scales = "fixed", space = "fixed") + labs(x="Interval", y="Number of Steps") + ggtitle("Number of Steps - Weekday vs. Weekend") + labs(color='Day Type')
```

![](PA1_template_files/figure-html/chart4-1.png)<!-- -->

The test subject is more active earlier in the day on weekdays vs. weekend days, but less active through the middle of the day. My guess is they have a normal 'desk' job during the week while they spend their weekends catching up on housework and chores.
