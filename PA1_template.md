---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading relevant libraries


```r
library(ggplot2)
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## v purrr   0.3.4
```

```
## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(doBy)
```

```
## 
## Attaching package: 'doBy'
```

```
## The following object is masked from 'package:dplyr':
## 
##     order_by
```

## Loading and preprocessing the data


```r
setwd("/Users/jesus/Documents/GitHub/RepData_PeerAssessment1/activity")
RawData<- read.csv("activity.csv",na.strings=("NA"))
```

## What is mean total number of steps taken per day?


```r
NaRemoved<-filter(RawData, (is.na(RawData$steps)==FALSE))
StepsPerDay<-summaryBy(steps~date,data=NaRemoved, FUN = sum)
hist(StepsPerDay$steps.sum)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
MeanSteps<-mean(StepsPerDay$steps.sum)
MedSteps<-median(StepsPerDay$steps.sum)

print(c("Mean steps taken is",MeanSteps))
```

```
## [1] "Mean steps taken is" "10766.1886792453"
```

```r
print(c("Median steps taken is",MedSteps))
```

```
## [1] "Median steps taken is" "10765"
```


## What is the average daily activity pattern?

```r
StepsPerInterval<-summaryBy(steps~interval,data=NaRemoved, FUN = mean)

# Converts from format hhmm to minutes
ConvertedSteps<-mutate(StepsPerInterval,interval = str_pad(interval,

                            width = 4,

                            side = "left",

                            pad = "0")) %>%

  extract(interval,

          into = c("hr", "min"),

          regex = "(\\d{2})(\\d{2})",

          convert = TRUE) %>%

  mutate(interval = (hr*60) + (min))

#graph
ggplot(ConvertedSteps, aes(x=interval, y=steps.mean) )+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Max
StepsPerInterval$interval[which.max(StepsPerInterval$steps.mean)]
```

```
## [1] 835
```

## Imputing missing values


```r
#Calculation of missing values
print(nrow(RawData[is.na(RawData$steps)==TRUE,]))
```

```
## [1] 2304
```

```r
#replacement of values
RawStepsPerInterval <- summaryBy(steps~interval,data=NaRemoved, FUN = mean)

MergedResults<-merge(RawData,RawStepsPerInterval, by.x = "interval", by.y = "interval", all.x=TRUE)
MergedResults$ImputedSteps<-coalesce(MergedResults$steps,MergedResults$steps.mean)

ImputedData<-summaryBy(ImputedSteps~date,data=MergedResults, FUN = sum)
```


## Are there differences in activity patterns between weekdays and weekends?
