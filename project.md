# represearch project 1
Michael Spencer  
April 10, 2016  

## Loading and preprocessing the data
Show any code that is needed to 
  1. Load the data (i.e. read.csv())
  2. Process/transform the data (if necessary) into a format suitable for your analysis
  

```r
setwd("~/Desktop/coursera rprograming/reproducible research/project 1")

library("plyr")
library("lattice")
library("data.table")
library("ggplot2")
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
url <- "d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
data <- "data"

if(!file.exists(data)){
  dir.create(data)
}

graphics <- "graphics" 
if(!file.exists(graphics)){
  dir.create(graphics)
}

zip <- paste(getwd(), "/activity.zip", sep = "")
if(!file.exists(zip)){
  download.file(url, zip, method="curl", mode="wb")
}

archive <- paste(getwd(), "/data/activity.csv", sep = "")
if(!file.exists(archive)){
  unzip(zip, list = FALSE, overwrite = FALSE, exdir = data)
}

activity <- read.table(file = archive, header = TRUE, sep = ",")
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
  1. Make a histogram of the total number of steps taken each day
  2. Calculate and report the mean and median total number of steps taken per day


```r
activity$dateandtime <- as.POSIXct(with(activity, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
                                  format="%Y-%m-%d %H:%M",tz="")

stepsperday <- setNames(aggregate(steps~as.Date(date), activity, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(stepsperday), by = 6)

okscale <- list(x = list(rot = 45, cex = 1.0, labels = format(stepsperday$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = stepsperday, main = "steps per day", ylab = "steps", xlab = "date", scales = okscale, horizontal = F)
```

![](project_files/figure-html/unnamed-chunk-2-1.png)

```r
paste("mean:", mean(stepsperday$steps))
```

```
## [1] "mean: 10766.1886792453"
```

```r
paste("median:", median(stepsperday$steps))
```

```
## [1] "median: 10765"
```

## What is the average daily activity pattern?
  1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
timeseries <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(timeseries, type = "l", axes = F, xlab = "Time of the day", 
    ylab = "Average across all days provided a time", main = "Average number of steps taken", 
    col = "red")
axis(1,at=c(seq(0,2400,100),835), label = paste(c(seq(0,24,1),8),c(rep(":00",25),":40"),sep=""), pos = 0)
axis(2, at=c(seq(0,210,30),206.2), label = c(seq(0,210,30),206.2), pos = 0)
maximum <- which.max(timeseries$steps)
segments(832, 0, 832, 206.2, col = "blue", lty = "dashed")
text(835,200, "max average of steps: (832,206.2)", col = "blue", adj = c(-.1, -.1))
segments(0, 206.2, 832, 206.2, col = "blue", lty = "dashed")
```

![](project_files/figure-html/unnamed-chunk-3-1.png)

```r
timeseries [maximum, ]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
#which.max(timeseries$steps)/12
paste(835, "is equivalent to 8.667 hours, this gives the maximum is reached at 8:40 am")
```

```
## [1] "835 is equivalent to 8.667 hours, this gives the maximum is reached at 8:40 am"
```

## Inputting missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

  1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
  4. Make a histogram of the total number of steps taken each day and Calculate and   report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
paste("missing observations:", sum(is.na(activity$steps)))
```

```
## [1] "missing observations: 2304"
```

```r
"missing observations can be replaced with the sample mean instead of replacing them by zero (the 1st won't bias the estimates while the 2nd will)"
```

```
## [1] "missing observations can be replaced with the sample mean instead of replacing them by zero (the 1st won't bias the estimates while the 2nd will)"
```

```r
newdata <- activity
newdata[is.na(activity$steps), ]$steps <- mean(activity$steps)

newdata$dateandtime <- as.POSIXct(with(newdata, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
                                  format="%Y-%m-%d %H:%M",tz="")

stepsperday2 <- setNames(aggregate(steps~as.Date(date), newdata, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(stepsperday2), by = 6)

okscale2 <- list(x = list(rot = 45, cex = 1.0, labels = format(stepsperday2$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = stepsperday2, main = "steps per day", ylab = "steps", xlab = "date", scales = okscale2, horizontal = F)
```

![](project_files/figure-html/unnamed-chunk-4-1.png)

```r
paste("mean:", mean(stepsperday2$steps))
```

```
## [1] "mean: 10766.1886792453"
```

```r
paste("median:", median(stepsperday2$steps))
```

```
## [1] "median: 10765"
```

```r
paste("means difference:", mean(stepsperday2$steps)-mean(stepsperday$steps))
```

```
## [1] "means difference: 0"
```

```r
paste("medians difference:", median(stepsperday2$steps)-median(stepsperday$steps))
```

```
## [1] "medians difference: 0"
```

## Are there differences in activity patterns between weekdays and weekends?
  For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
  1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
  2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data: Your plot will look different from the one above because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.


```r
str(newdata)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date       : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval   : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ dateandtime: POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
```

```r
newdata$date <- as.Date(newdata$date, "%Y-%m-%d")
newdata$day <- weekdays(newdata$date)
newdata$tipodia <- c("weekday")
for (i in 1:nrow(newdata)){
  if (newdata$day[i] == "Saturday" || newdata$day[i] == "Sunday"){
    newdata$tipodia[i] <- "weekend"
  }
}
newdata$tipodia <- as.factor(newdata$tipodia)
semana_o_finde <- aggregate(steps ~ interval+tipodia, newdata, mean)

qplot(interval, steps, data=semana_o_finde, geom=c("line"), xlab="5-min intervals", 
      ylab="steps mean", main="") + facet_wrap(~ tipodia, ncol=1)
```

![](project_files/figure-html/unnamed-chunk-5-1.png)
