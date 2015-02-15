Activity Monitoring Devices
========================================================




## Loading and preprocessing the data
The entire dataset of steps data is loaded into the variable called steps_data in the following:


```r
steps_data <- read.csv("./repdata-data-activity/activity.csv")
```

```
## Warning: cannot open file './repdata-data-activity/activity.csv': No such
## file or directory
```

```
## Error: cannot open the connection
```

```r
# steps_data$date <- strptime(steps_data$date, '%Y-%m-%d')
```




## Mean total number of steps taken per day

Following code computes "PerDay" calculations for each date given in the dataset. Daywise sums, means and medians for steps are calculated and stored in the variable "datewise". Then, histogram of daywise sums of steps is plotted with ggplot2 library.



```r
StepSumsPerDay = tapply(steps_data$steps, steps_data$date, sum, na.rm = TRUE)  ## Calculating Daily Sums
```

```
## Error: object 'steps_data' not found
```

```r
StepMeansPerDay = tapply(steps_data$steps, steps_data$date, mean, na.rm = TRUE)  ## Calculating Daily Means
```

```
## Error: object 'steps_data' not found
```

```r
StepMediansPerDay = tapply(steps_data$steps, steps_data$date, median, na.rm = TRUE)  ## Calculating Daily Medians
```

```
## Error: object 'steps_data' not found
```

```r

datewise = data.frame(cbind(StepSumsPerDay, StepMeansPerDay, StepMediansPerDay))  ## Merging above three in a single dataframe
```

```
## Error: object 'StepSumsPerDay' not found
```

```r
colnames(datewise) = c("Sums", "Means", "Medians")
```

```
## Error: object 'datewise' not found
```

```r
library(ggplot2)
qplot(Sums, data = datewise, geom = "histogram", fill = "red", xlab = "Daily Total Steps", 
    ylab = "Frequency of occurances ", main = "Histogram of Total Daily Steps")
```

```
## Error: object 'datewise' not found
```

```r
overall_mean_of_sums = mean(datewise$Sums, na.rm = T)
```

```
## Error: object 'datewise' not found
```

```r
overall_median_of_sums = median(datewise$Sums, na.rm = T)
```

```
## Error: object 'datewise' not found
```


The overall mean of the total number of steps taken per day is 

```

Error in eval(expr, envir, enclos) : 
  object 'overall_mean_of_sums' not found

```

.  
Similarly median is 

```

Error in eval(expr, envir, enclos) : 
  object 'overall_median_of_sums' not found

```

 


## Average daily activity pattern


```r
steps_data$interval = as.factor(steps_data$interval)
```

```
## Error: object 'steps_data' not found
```

```r
StepMeansPerInterval = data.frame(tapply(steps_data$steps, steps_data$interval, 
    mean, na.rm = TRUE))
```

```
## Error: object 'steps_data' not found
```

```r
colnames(StepMeansPerInterval) = "IntervalMeans"
```

```
## Error: object 'StepMeansPerInterval' not found
```

```r
StepMeansPerInterval$x = 1:288
```

```
## Error: object 'StepMeansPerInterval' not found
```

```r
ggplot(StepMeansPerInterval, aes(x = x, y = IntervalMeans)) + geom_line() + 
    xlab("Interval") + ylab("Average Steps in the Interval") + ggtitle("Average no. of Steps in all Intervals")
```

```
## Error: object 'StepMeansPerInterval' not found
```

```r

max_interval = names(which.max(StepMeansPerInterval$IntervalMeans))
```

```
## Error: object 'StepMeansPerInterval' not found
```

The above plot shows the average number of steps in an interval across all days.  
Following plot will show average number of steps daily across all intervals.

```r
datewise$Days = 1:61
```

```
## Error: object 'datewise' not found
```

```r
ggplot(datewise, aes(x = Days, y = Means)) + geom_line() + xlab("Days") + ylab("Average Steps daily") + 
    ggtitle("Average no. of Steps Daily for Two Months")
```

```
## Error: object 'datewise' not found
```



The Interval which has the maximum average value of number of steps is 

```

Error in which.max(StepMeansPerInterval$IntervalMeans) : 
  object 'StepMeansPerInterval' not found

```



## Imputing missing values

Replacing missing values in the variable "new_data"" with the average of the whole dataset.

```r
missing = is.na(steps_data$steps)
```

```
## Error: object 'steps_data' not found
```

```r
steps_data$interval = as.numeric(as.character(steps_data$interval))
```

```
## Error: object 'steps_data' not found
```

```r
# Replacing missing values with the average of whole dataset.
new_data = steps_data
```

```
## Error: object 'steps_data' not found
```

```r
new_data$steps[missing] <- mean(new_data$steps, na.rm = T)
```

```
## Error: object 'new_data' not found
```

The total number of missing values before imputing in the dataset are 

```

Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors

```

   

In the following code, calculating the number of steps taken each day.


```r
newSumsPerDay = data.frame(tapply(new_data$steps, new_data$date, sum, na.rm = TRUE))
```

```
## Error: object 'new_data' not found
```

```r
colnames(newSumsPerDay) = "new_Sums"
```

```
## Error: object 'newSumsPerDay' not found
```

```r
qplot(new_Sums, data = newSumsPerDay, geom = "histogram", fill = "red", xlab = "Daily Total Steps", 
    ylab = "Frequency of occurances after imputation ", main = "Histogram of Total Daily Steps After Imputation")
```

```
## Error: object 'newSumsPerDay' not found
```

```r
new_mean = mean(newSumsPerDay$new_Sums, na.rm = T)
```

```
## Error: object 'newSumsPerDay' not found
```

```r
new_median = median(newSumsPerDay$new_Sums, na.rm = T)
```

```
## Error: object 'newSumsPerDay' not found
```


The values of mean and median after imputation are 

```

Error in eval(expr, envir, enclos) : object 'new_mean' not found

```

 and 

```

Error in eval(expr, envir, enclos) : object 'new_median' not found

```

 respectively.  
They are different than the previous value and are slightly higher.
## Are there differences in activity patterns between weekdays and weekends?

In the following code, creating new factor variables - "Weekday" and "weekend." Then, plotting the required graph after doing some computations. 



```r
new_data$weekday = weekdays(as.Date(strptime(new_data$date, "%Y-%m-%d")))
```

```
## Error: object 'new_data' not found
```

```r
new_data$f = ifelse(new_data$weekday == "Sunday" | new_data$weekday == "Saturday", 
    "weekend", "weekday")
```

```
## Error: object 'new_data' not found
```

```r
new_data$f = factor(new_data$f, labels = c("weekend", "weekday"))
```

```
## Error: object 'new_data' not found
```

```r
library(lattice)
new_data$interval = as.factor(new_data$interval)
```

```
## Error: object 'new_data' not found
```

```r
weekend = subset(new_data, f == "weekend")
```

```
## Error: object 'new_data' not found
```

```r
weekend = data.frame(tapply(weekend$steps, weekend$interval, mean, na.rm = T))
```

```
## Error: object 'weekend' not found
```

```r
weekday = subset(new_data, f == "weekday")
```

```
## Error: object 'new_data' not found
```

```r
weekday = data.frame(tapply(weekday$steps, weekday$interval, mean, na.rm = T))
```

```
## Error: object 'weekday' not found
```

```r
weekend$x = rownames(weekend)
```

```
## Error: object 'weekend' not found
```

```r
weekday$x = rownames(weekday)
```

```
## Error: object 'weekday' not found
```

```r
weekend$f = "weekend"
```

```
## Error: object 'weekend' not found
```

```r
weekday$f = "weekday"
```

```
## Error: object 'weekday' not found
```

```r
rownames(weekend) = 1:288
```

```
## Error: object 'weekend' not found
```

```r
colnames(weekend) = c("Means", "x", "f")
```

```
## Error: object 'weekend' not found
```

```r
rownames(weekday) = 1:288
```

```
## Error: object 'weekday' not found
```

```r
colnames(weekday) = c("Means", "x", "f")
```

```
## Error: object 'weekday' not found
```

```r
week = rbind(weekend, weekday)
```

```
## Error: object 'weekend' not found
```

```r
week$f = factor(week$f, labels = c("weekday", "weekend"))
```

```
## Error: object 'week' not found
```

```r
week$Means = as.numeric(week$Means)
```

```
## Error: object 'week' not found
```

```r
week$x = as.numeric(week$x)
```

```
## Error: object 'week' not found
```

```r
xyplot(Means ~ x | f, layout = c(1, 2), data = week, type = "l", xlab = "Interval", 
    ylab = "Average No. of Steps")
```

```
## Error: object 'week' not found
```


