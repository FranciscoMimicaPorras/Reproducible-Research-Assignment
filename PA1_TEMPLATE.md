---
title: 'Peer-graded Assignment: Course Project 1'
author: "Francisco Mimica Porras"
date: "2/1/2021"
output:
   md_document: default
 ---

## Setting the data

```{r}
setwd("C:/Users/Famil/OneDrive/COURSERA COURSES/Reproducible Research/Peer-graded Assignment Course Project 1")
library(datasets)
activity <- read.csv("C:/Users/Famil/OneDrive/COURSERA COURSES/Reproducible Research/Peer-graded Assignment Course Project 1/activity.csv")
```

## Formatting date variable


```{r}
activity$date <- as.Date(activity$date)
```

## Including Plots: Histogram without NA values
Histogram of total number of steps taken on each day

```{r}

sum_steps<-aggregate(activity$steps,by=list(activity$date),FUN=sum,na.rm=TRUE) 
png(file = "Plot1.png", width = 480, height = 480, units = "px")
hist(sum_steps$x, 
     breaks=seq(from=0, to=25000, by=2500),
     col="yellow", 
     xlab="Total number of steps", 
     ylim=c(0, 20),  
     main="total number of steps taken each day")
```


## Mean and Median of Steps

Mean and median number of steps taken each day

```{r}
mean(sum_steps$x)
median(sum_steps$x)
```

## Time Series Plot

Time series plot of the average number of steps taken

```{r}
avg_steps<-aggregate(activity$steps,by=list(activity$interval),FUN=mean,na.rm=TRUE) 
colnames(avg_steps)<-c("interval","steps")
library(ggplot2)
png(file = "Plot2.png", width = 480, height = 480, units = "px")
ggplot(aes(x=interval,y=steps),data=avg_steps)+geom_line()

```

## Maximum Average 5 minute interval

```{r}
avg_steps[avg_steps$steps==max(avg_steps$steps),1]

```

## Imputing NA

Calculate and report the total number of missing values in the dataset
```{r}
sum(is.na(activity$steps))

```

Replace NA values with the mean of the steps

```{r}
activity$steps[is.na(activity$steps)]<-mean(activity$steps,na.rm=TRUE)

```

Here are some rows of new data set

```{r}
head(activity)

```

## Histogram with Replaced NA values

Histogram of total number of steps taken on each day (replaced NA values)

```{r}

sum_steps <-aggregate(activity$steps,by=list(activity$date),FUN=sum,na.rm=TRUE) 
png(file = "Plot3.png", width = 480, height = 480, units = "px")
hist(sum_steps$x, 
      breaks=seq(from=0, to=25000, by=2500),
      col="yellow", 
      xlab="Total number of steps", 
      ylim=c(0, 30), 
      main="Total number of steps taken each day\n(NA replaced by mean)")
```

## Mean and median number of steps taken each day after replacing NA values with mean

```{r}
mean(sum_steps$x)
median(sum_steps$x)

```

##  Differences in activity patterns between weekdays and weekends

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}

activity$days=tolower(weekdays(activity$date))

```

# Now categorised days into weekend and weekdays

```{r}

activity$day_type<-ifelse(activity$days=="saturday"|activity$days=="sunday","weekend","weekday")

```

# Take mean steps taken on weekend or weekday in the intervals

```{r}
avg_steps<-aggregate(activity$steps,by=list(activity$interval,activity$day_type),FUN=mean,na.rm=TRUE)

colnames(avg_steps)<-c("interval","day_type","steps")

```


# Create panel plot between average steps and interval seperated by day type

```{r}
png(file = "Plot4.png", width = 480, height = 480, units = "px")
ggplot(aes(x=interval,y=steps),data=avg_steps)+geom_line()+facet_wrap(~avg_steps$day_type)

```