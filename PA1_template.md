# Reproducible Research: Peer Assessment 1

 
## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

  
## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Loading and preprocessing the data

#### 1. Loading the data

Download the dataset from the link above and unzip the file.  
The dataset should be placed in the same folder as your analysis (R-script) files.

Load the data, i.e. read the file into R or RStudio.  
For me, I prefer to use RStudio.


```r
## Load the data
data <- read.csv("activity.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
```

After loading the data, I will view the data first for a sense of how they look like.


```r
## View data
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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

```r
tail(data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

Before I start running my analysis, I will load the packages that I need. I prefer the data to be in non-exponential form, so I will disable the scientific notation too.


```r
## Load packages for use
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra) ## for putting multiple plots into one page

## Disable scientific notation
options(scipen=999)
```


#### 2. Processing/Transforming the data

To facilitate my analysis, I will transform the variables first.  

1. **steps**: Convert the class from *integer* to *numeric*.
2. **date**: Convert the class from *character* to *date* format.
3. **interval**: Transform the data to 4-digit numbers and convert them to resemble the time format (eg. *05:30*, *14:30*)


```r
## Format the variables
data$steps <- as.numeric(data$steps) ## Change to numeric class
data$date <- as.Date(data$date) ## Change to Date format
data$interval <- formatC(data$interval, width = 4, flag = "0") ## Transform to 4-digit numbers (add leading zeros if necessary)
data$interval <- format(strptime(data$interval,"%H%M"), "%H:%M") ## Format to resemble the time format
```

The results will look like this.


```r
## View the data
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: chr  "00:00" "00:05" "00:10" "00:15" ...
```

Use **select** function from dplyr to rearrange the variables in the order of **date**, **interval**, and **steps**.


```r
## Rearrange the variables in the dataset
data1 <- select(data, date, interval, steps)
```

The processed dataset will look like this.


```r
## View the first few rows of the processed data
head(data1)
```

```
##         date interval steps
## 1 2012-10-01    00:00    NA
## 2 2012-10-01    00:05    NA
## 3 2012-10-01    00:10    NA
## 4 2012-10-01    00:15    NA
## 5 2012-10-01    00:20    NA
## 6 2012-10-01    00:25    NA
```


## What is mean total number of steps taken per day?

Select the cases with no missing values. This is necessary to prevent bias being introduced in the computation of mean and median total number of steps.


```r
## Select only cases with no missing values
data2 <- data1[complete.cases(data1),]
```

#### 1. Make a histogram of the total number of steps taken each day

First, compute the total number of steps taken each day.


```r
## Compute total number of steps taken each day
data3 <- ddply(data2, .(date), summarize, total.steps = sum(steps))
```

The results will look like this.


```r
## View the first few rows of data3
head(data3)
```

```
##         date total.steps
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

Next, I will use ggplot2 to plot the histogram and name it as **plot1**.


```r
## Plot the histogram 
g1 <- ggplot(data3, aes(date, total.steps)) ## data to use and define x-axis and y-axis
plot1 <- g1 + geom_bar(fill = "blue", stat="identity") + ## plot bar chart
        ggtitle("Total Number of Steps Taken Each Day\n(1 Oct 2012 - 30 Nov 2012)") + ## create the main title in 2 lines
        theme(plot.title = element_text(lineheight=1, face="bold")) + ## define format for main title
        xlab("Date") + ## name x-axis
        ylab("Total Number of Steps Taken") ## name y-axis
print(plot1)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

#### 2. Calculate and report the mean and median total number of steps taken each day


```r
## Compute the mean total number of steps taken each day
s1mean.steps <- mean(data3$total.steps)

## Compute the median number of steps taken each day
s1median.steps <- median(data3$total.steps)
```

The mean total number of steps taken each day is 10766.  

The median total number of steps taken each day is 10765.


## What is the average daily activity pattern?

#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Firstly, compute the average number of steps taken for each 5 minute interval, averaged across all days.


```r
## Compute the mean steps for each 5-minute interval 
data4 <- ddply(data2, .(interval), summarize, mean.steps = mean(steps))
```

The results will look like this.


```r
## View the first few rows of data4
head(data4)
```

```
##   interval mean.steps
## 1    00:00  1.7169811
## 2    00:05  0.3396226
## 3    00:10  0.1320755
## 4    00:15  0.1509434
## 5    00:20  0.0754717
## 6    00:25  2.0943396
```

Before plotting the histogram, I will first set the x-axis tick-mark labels at 30 minute interval for the time series plot.


```r
## Format x-axis labels for the time series plot
xlabels <- data4$interval[seq(1, nrow(data4), 30)]
```

Next, I will plot the time series using ggplot and name the plot **plot2**.


```r
## Plot time series
g2 <- ggplot(data4, aes(x = interval, y = mean.steps, group=1)) 
plot2 <- g2 + geom_line(colour = "blue", size = 1) + ## plot line chart
        ggtitle("Average Number of Steps Taken Each 5-Minute Interval\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        scale_x_discrete(breaks = xlabels) + ## use the x-axis tick-mark labels set earlier
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps Taken")
print(plot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## Compute the maximum average number of steps (averaged across all days)
s2max.steps <- max(data4$mean.steps)
s2maxsteps.int <- data4$interval[which(data4$mean.steps == s2max.steps)]
```

The 5-minute interval which gives the maximum mean number of steps (averaged across all the days) is 08:35, with 206 steps.


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
total.NA <- sum(is.na(data))
```

There are 2304 missing values in the dataset.

#### 2. Devise a strategy for filling in all of the missing values in the dataset. 

The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to replace the missing values for each 5-minute interval with the corresponding 5-minute interval mean number of steps.


```r
## Select only cases with no missing values from the processed dataset, data1
data2 <- data1[complete.cases(data1),]

## Compute the mean number of steps (averaged across all days) for each 5-minute interval
int.mean <- ddply(data2, .(interval), summarize, mean.steps = mean(steps))
```

The results will look like this.


```r
head(int.mean)
```

```
##   interval mean.steps
## 1    00:00  1.7169811
## 2    00:05  0.3396226
## 3    00:10  0.1320755
## 4    00:15  0.1509434
## 5    00:20  0.0754717
## 6    00:25  2.0943396
```

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

First, create a **for loop** function to replace missing values (NAs) in the new dataset **comp.data** with the corresponding 5-minute interval mean. 


```r
## Create a new dataset with no missing values
comp.data <- data1
        for (i in 1:length(comp.data[,3])) {
        if (is.na(comp.data[i, 3])) {
                comp.data[i, 3] <- int.mean[which(int.mean$interval == comp.data[i,2]), 2]
        } else {
                comp.data[i,3] <- comp.data[i, 3]
        }
}
```

Check that the new dataset, "comp.data", has no missing values.


```r
## Check if the new dataset still have missing values 
sum(is.na(comp.data)) ## zero indicates no missing value
```

```
## [1] 0
```

The new dataset will look like this.


```r
## View the first few rows of the new dataset., comp.data
head(comp.data)
```

```
##         date interval     steps
## 1 2012-10-01    00:00 1.7169811
## 2 2012-10-01    00:05 0.3396226
## 3 2012-10-01    00:10 0.1320755
## 4 2012-10-01    00:15 0.1509434
## 5 2012-10-01    00:20 0.0754717
## 6 2012-10-01    00:25 2.0943396
```


#### 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

First, I will compute the total number of steps taken per day.


```r
## Compute the total number of steps taken per day
comp.data1 <- ddply(comp.data, .(date), summarize, total.steps = sum(steps))
```

The results will look like this.


```r
## View the first few rows of comp.data1
head(comp.data1)
```

```
##         date total.steps
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```

Next, I will plot the histogram and name it **plot3**.


```r
## Plot histogram of total number of steps taken each day
g3 <- ggplot(data = comp.data1, aes(date, total.steps))
plot3 <- g3 + geom_bar(fill = "blue", stat="identity") +
        ggtitle("Total Number of Steps Taken Each Day\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        xlab("Date") +
        ylab("Total Number of Steps Taken")
print(plot3)
```

![](PA1_template_files/figure-html/unnamed-chunk-26-1.png) 

#### 2. Calculate and report the mean and median total number of steps taken each day


```r
## Compute the mean total number of steps taken each day
s3mean.steps <- mean(comp.data1$total.steps)

## Compute the median total number of steps taken each day
s3median.steps <- median(comp.data1$total.steps)
```

The mean total number of steps taken each day is 10766.
The median total number of steps taken each day is 10766.

#### 3. Difference between the values from the estimates from the first part of the assignment and this part of assignment

I will plot a multipanel plot of the two histograms before and after imputing the missing values for easy comparison.


```r
## Retrieve plot 1 and rename it plot1a, adding a line for the mean total number of steps
plot1a <- plot1 + geom_hline(yintercept = s1mean.steps)

## Retrieve plot 3 and rename it plot3a, adding a line for the mean total number of steps
plot3a <- plot3 + geom_hline(yintercept = s3mean.steps)

## Plot the two histogrames together
grid.arrange(plot1a, plot3a, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-28-1.png) 

The values in the two histograms are not much different, other than the 
histogram with imputed missing data having slightly longer tails and more data values (hence a fuller distrbution).  

The mean and median total number of steps are the same as a result of the imputing the missing data.  

#### 4. Impact of imputing missing data on the estimates of the total daily number of steps

The mean total daily number of steps before and after imputing the missing data remain the same.  

The median total daily number of steps before and after imputing the missing data differ by only one step (10765 steps before vs 10766 after imputing the missing values).

## Are there differences in activity patterns between weekdays and weekends?

For this part, the weekdays() function may be of some help here. 
Use the dataset with the filled-in missing values for this part.

#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

First, I will use the **mutate** function from dplyr package to create a new variable, **week**, that has 2 values, weekday or weekend.  

Then I will replace the weekdays (i.e. Monday to Friday) with **weekday** and Saturday and Sunday with **weekend**.


```r
## Create a new variable, week, for the weekdays
comp.data2 <- mutate(comp.data, week = weekdays(date))

## Replace the values under the week variable with weekday and weekend
comp.data2$week <- gsub("Monday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Tuesday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Wednesday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Thursday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Friday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Saturday", "weekend", comp.data2$week)
comp.data2$week <- gsub("Sunday", "weekend", comp.data2$week)
```

The results will look like this.


```r
head(comp.data2)
```

```
##         date interval     steps    week
## 1 2012-10-01    00:00 1.7169811 weekday
## 2 2012-10-01    00:05 0.3396226 weekday
## 3 2012-10-01    00:10 0.1320755 weekday
## 4 2012-10-01    00:15 0.1509434 weekday
## 5 2012-10-01    00:20 0.0754717 weekday
## 6 2012-10-01    00:25 2.0943396 weekday
```

#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

First, I will compute the average number of steps taken per 5-minute interval, averaged across all weekday days or weekend days


```r
## Compute the mean steps taken per 5-min interval, averaged across all weekday days or weekend days
comp.data3 <- ddply(comp.data2, c("interval", "week"), summarize,
                    mean.steps = mean(steps))
```

Next, I will plot the panel plot with two facets for weekday days and weekend days.


```r
## Plot a panel plot with facets for weekday days and weekend days
g4 <- ggplot(comp.data3, aes(x = interval, y = mean.steps, group = week, colour = week)) 
plot4 <- g4 + geom_line(size = 1) + facet_grid(week ~ .) +
        ggtitle("Average Number of Steps Taken Each 5-Minute Interval\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        scale_x_discrete(breaks = xlabels) +
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps Taken")
print(plot4)
```

![](PA1_template_files/figure-html/unnamed-chunk-32-1.png) 


