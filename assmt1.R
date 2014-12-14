## MOOC - John Hopkins University: Data Science Specialisation
## Module 4 - Reproducible Research

## Set working directory

setwd("C:/Users/KLH/coursera/datascience/reproducible research/peerassmt1")

## Read and view the raw data
## =======================================================================

## Read the data
data <- read.csv("activity.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)

## View data
str(data)
head(data)
tail(data)

## Load packages for use
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

## Disable scientific notation
options(scipen=999)

## Process/Transform the data
## =======================================================================

## Format the variables
data$steps <- as.numeric(data$steps)
data$date <- as.Date(data$date)
data$interval <- formatC(data$interval, width = 4, flag = "0")
data$interval <- format(strptime(data$interval,"%H%M"), "%H:%M")

## View the data
str(data)

## Rearrange the variables in dataset
data1 <- select(data, date, interval, steps)

## View the data
head(data1)


## Section 1: What is mean total number of steps taken per day?
## ========================================================================
## For this part of the assignment, you can ignore the missing values in 
## the dataset
data2 <- data1[complete.cases(data1),]

## 1. Make a histogram of the total number of steps taken each day
## ---------------------------------------------------------------
## Compute total number of steps taken per day
data3 <- ddply(data2, .(date), summarize, total.steps = sum(steps))


## Plot the histogram 
g1 <- ggplot(data = data3, aes(date, total.steps))
plot1 <- g1 + geom_bar(fill = "blue", stat="identity") +
        ggtitle("Total Number of Steps Taken Each Day\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        xlab("Date") +
        ylab("Total Number of Steps Taken")
print(plot1)

## 2. Calculate and report the mean and median total number of steps taken 
##    per day

s1mean.steps <- mean(data3$total.steps)
s1median.steps <- median(data3$total.steps)


## Section 2: What is the average daily activity pattern?
## ========================================================================

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval
## (x-axis) and the average number of steps taken, averaged across all days 
## (y-axis)

## Compute average number of steps taken for each 5 min interval, averaged 
## across all days
data4 <- ddply(data2, .(interval), summarize, mean.steps = mean(steps))

## Format x-axis labels for the time series plot
xlabels <- data4$interval[seq(1, nrow(data4), 30)]


## Plot time series
g2 <- ggplot(data4, aes(x = interval, y = mean.steps, group=1)) 
plot2 <- g2 + geom_line(colour = "blue", size = 1) +
        ggtitle("Average Number of Steps Taken Each 5-Minute Interval\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        scale_x_discrete(breaks = xlabels) +
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps Taken")
print(plot2)

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
s2max.steps <- max(data4$mean.steps)
s2maxsteps.int <- data4$interval[which(data4$mean.steps == s2max.steps)]


## Section 3: Imputing missing values
## =======================================================================

## Note that there are a number of days/intervals where there are missing 
## values (coded as NA). The presence of missing days may introduce bias 
## into some calculations or summaries of the data.

## 1. Calculate and report the total number of missing values in the 
## dataset (i.e. the total number of rows with NAs)

total.NA <- sum(is.na(data))

## 2. Devise a strategy for filling in all of the missing values in the 
## dataset. The strategy does not need to be sophisticated. For example, 
## you could use the mean/median for that day, or the mean for that 
## 5-minute interval, etc.

## My strategy: I will replace the missing values for each 5-minute 
## interval with the corresponding 5-minute interval mean.
data2 <- data1[complete.cases(data1),]
int.mean <- ddply(data2, .(interval), summarize, mean.steps = mean(steps))

## 3. Create a new dataset that is equal to the original dataset but with 
## the missing data filled in.

## Create a for loop function to replace missing values (NAs) with ## corresponding 5-minute interval mean
names(data1)

## Create a new dataset with no missing values
comp.data <- data1
        for (i in 1:nrow(comp.data)) {
        if (is.na(comp.data[i, 3])) {
                comp.data[i, 3] <- int.mean[which(int.mean$interval == comp.data[i,2]), 2]
        } else {
                comp.data[i,3] <- comp.data[i, 3]
        }
}

## Check that the new dataset, "comp.data", has no missing values
sum(is.na(comp.data))

## 4. Make a histogram of the total number of steps taken each day and 
## Calculate and report the mean and median total number of steps taken per 
## day. Do these values differ from the estimates from the first part of 
## the assignment? What is the impact of imputing missing data on the 
## estimates of the total daily number of steps?

## Compute total number of steps taken per day
comp.data1 <- ddply(comp.data, .(date), summarize, total.steps = sum(steps))

## Plot histogram of total number of steps taken each day

g3 <- ggplot(data = comp.data1, aes(date, total.steps))
plot3 <- g3 + geom_bar(fill = "blue", stat="identity") +
        ggtitle("Total Number of Steps Taken Each Day\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        xlab("Date") +
        ylab("Total Number of Steps Taken")
print(plot3)

## 2. Calculate and report the mean and median total number of steps taken 
##    per day

s3mean.steps <- mean(comp.data1$total.steps)
s3median.steps <- median(comp.data1$total.steps)

## impact of imputing missing data on the estimates of the total daily 
## number of steps

## mean values remain the same
## median values do not differ much (10,765 in first histogram vs 10766.19 
## in 2nd histogram)
## mean and median values of the 2nd histogram are the same

## Difference between the values from the estimates from the first part of 
## the assignment

## I will put the two histograms together for comparison
plot1a <- plot1 + geom_hline(yintercept = s1mean.steps)
plot3a <- plot3 + geom_hline(yintercept = s3mean.steps)
grid.arrange(plot1a, plot3a, ncol = 2)
## The values in the two histograms are not much different, other than the 
## histogram with imputed missing data having slightly longer tails and a 
## fuller distribution.


## Section 4: Are there differences in activity patterns between weekdays 
## and weekends?

## For this part the weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.

## 1. Create a new factor variable in the dataset with two levels - 
## "weekday" and "weekend" indicating whether a given date is a weekday or ## weekend day.

comp.data2 <- mutate(comp.data, week = weekdays(date))

comp.data2$week <- gsub("Monday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Tuesday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Wednesday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Thursday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Friday", "weekday", comp.data2$week)
comp.data2$week <- gsub("Saturday", "weekend", comp.data2$week)
comp.data2$week <- gsub("Sunday", "weekend", comp.data2$week)
        
## 2. Make a panel plot containing a time series plot (i.e. type = "l") of 
## the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what 
## this plot should look like using simulated data.

## Compute the average number of steps taken per 5-minute interval, 
## averaged across all weekday days or weekend days
comp.data3 <- ddply(comp.data2, c("interval", "week"), summarize,
                    mean.steps = mean(steps))

## Plot a panel plot
g4 <- ggplot(comp.data3, aes(x = interval, y = mean.steps, group = week, colour = week)) 
plot4 <- g4 + geom_line(size = 1) + facet_grid(week ~ .) +
        ggtitle("Average Number of Steps Taken Each 5-Minute Interval\n(1 Oct 2012 - 30 Nov 2012)") +
        theme(plot.title = element_text(lineheight=1, face="bold")) +
        scale_x_discrete(breaks = xlabels) +
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps Taken")
print(plot4)


