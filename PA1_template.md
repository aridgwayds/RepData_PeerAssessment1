---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---


## Loading and preprocessing the data


```r
#Read Data
sourcepath<-"C:/Users/aridgway/Documents/Coursera Data Scientist Certification/data/activity.zip" 

library(readr)
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
library(tidyr)

#read data
cols<-list(col_integer(),col_date(format=""),col_integer())
activity<-read_csv(sourcepath, col_types=cols)                 
nrows<-nrow(activity)

if(nrows>0) print(summary(activity))
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```



## What is mean total number of steps taken per day?

```r
#1 Mean steps taken per day
steps_per_day<-plyr::ddply(activity,"date",plyr::summarise,total.steps=sum(steps, na.rm=TRUE))

meanSteps<-round(mean(steps_per_day$total.steps, na.rm=TRUE),0)
medianSteps<-median(steps_per_day$total.steps, na.rm=TRUE)

hist(steps_per_day$total.steps,main="Total Steps per Day",col="gray",xlab="Total Steps",ylab="Frequency")
```

![](PA1_template_files/figure-html/MeanSteps-1.png)<!-- -->
The mean total number of steps taken per day is: 9354 and the median is: 10395.


## What is the average daily activity pattern?

```r
#2 Mean steps per interval
steps_per_intval<-plyr::ddply(activity,"interval",plyr::summarise,avg.steps=mean(steps, na.rm=TRUE)) %>% mutate(avg.steps=round(avg.steps,1))
maxIntval<-steps_per_intval[which.max(steps_per_intval$avg.steps),1]
maxSteps<-round(max(steps_per_intval$avg.steps),0)

plot(steps_per_intval,type="l",main="Average Steps per 5-minute Interval",col="gray",lwd=1,xlab="Interval",ylab="Avg. Steps across All Dates")
abline(v=maxIntval,lwd=1,col="black")
```

![](PA1_template_files/figure-html/AvgDaily-1.png)<!-- -->
The 5-minute interval 835 contains the maximum number of steps 206 on average across all the days in the dataset.

## Imputing missing values

```r
#3 Imputing NAs
#Rows with NA values
rowsNA<-nrow(activity)-sum(complete.cases(activity))
meanStepsInt<-round(mean(steps_per_intval$avg.steps,na.rm=TRUE),0)
activity_full<-mutate(activity,steps=replace_na(steps,meanStepsInt))
rowsNA2<-nrow(activity_full)-sum(complete.cases(activity_full))
steps_per_day2<-plyr::ddply(activity_full,"date",plyr::summarise,total.steps=sum(steps, na.rm=FALSE))

meanSteps2<-round(mean(steps_per_day2$total.steps, na.rm=TRUE),0)
medianSteps2<-median(steps_per_day2$total.steps, na.rm=TRUE)

hist(steps_per_day2$total.steps,main="Total Steps per Day \n(Imputed NAs using mean steps per interval values)",col="gray",xlab="Total Steps",ylab="Frequency")
```

![](PA1_template_files/figure-html/MissingVals-1.png)<!-- -->
The histogram above shows is based on the dataset with NAs replaced with imputed values. The shape shows the data more centered around the mean. 

The total number of rows containing missing values in the dataset is: 2304.
Imputed NAs using mean steps per interval (37.)

The mean total number of steps taken per day is: 1.0752\times 10^{4} and the median is: 1.0656\times 10^{4}. The impact of using the imputed missing data is to increase both the mean and median values.

## Are there differences in activity patterns between weekdays and weekends?

```r
#4 Difference between weekdays and weekends

library(lattice)
activity_full<-mutate(activity_full,
                      dow=(ifelse(weekdays(activity_full$date) %in% list("Saturday","Sunday"), "weekend","weekday")),dow=factor(dow))

steps_per_intval2<-plyr::ddply(activity_full,c("dow","interval"),plyr::summarise,avg.steps=mean(steps, na.rm=TRUE)) %>% mutate(avg.steps=round(avg.steps,1))

p<-xyplot(steps_per_intval2$avg.steps ~ steps_per_intval2$interval | steps_per_intval2$dow, type="l",layout = c(1, 2),xlab="Interval",ylab="Steps") ## Plot with 2 panels

print(p)
```

![](PA1_template_files/figure-html/Patterns-1.png)<!-- -->

The plot above shows the average steps per interval averaged over all days in the datset. This dataset has been altered from the original data whereby null values for the steps variable have been replaced with the steps per interval (37) averaged overall all days.

The main pattern shown in the plot is that on weekends the higher (on average) number of steps for intervals starting around 1000. For intervals less than 100, the higher higher step counts appear for weekdays.



