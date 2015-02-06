---
# Reproducible Research: Peer Assessment 1
  

## Loading and preprocessing the data


```r
require("ggplot2")
require("plyr")

## unzip an load the data
unzip("activity.zip")
activity  <- read.csv("activity.csv", header = TRUE, sep = ",")
file.remove("activity.csv")
```

## What is mean total number of steps taken per day?

```r
## total number of steps taken per day
dailysteps <- ddply(activity, .(date), summarise, 
                total_steps = sum(steps, na.rm = TRUE))
```


```r
## mean and median
mean_step = round(mean(dailysteps$total_steps))
median_step = median(dailysteps$total_steps)

## make a vector and a frame for the mean and the median line
ve <- c(mean_step, median_step)
df <- data.frame(ve)
```


```r
## histogram of the total number of steps taken each day
histo <- ggplot(dailysteps, aes(x= total_steps)) +
          geom_histogram(fill="lightblue", colour="black", binwidth=2000) +
          geom_vline(data=df, aes(xintercept=ve), colour=factor(ve),
                     linetype="dashed", size=1, show_guide = TRUE)+
          xlab("Total Number of steps") +
          ggtitle("The total number of steps taken each day")
histo
```

![plot of chunk histogram](figure/histogram-1.png) 

## Mean and median of the total number of steps taken per day

```r
mean_step = round(mean(dailysteps$total_steps))
mean_step
```

```
## [1] 9354
```

```r
median_step = median(dailysteps$total_steps)
median_step
```

```
## [1] 10395
```
the **mean** (red line on histogram) is **9 354** and the **median** (green line on histogram) is **10 395**. 

## What is the average daily activity pattern?

```r
## average number of steps by interval
average_steps <- ddply(activity, .(interval), summarise, 
                  mean_steps = mean(steps, na.rm = TRUE))
```


```r
## the maximum number of steps
max_steps = max(average_steps$mean_steps)
cross_interval = average_steps[average_steps$mean_steps == max_steps,]$interval
```


```r
## Time series plot
series_plot <- ggplot( average_steps, aes(x=interval, y=mean_steps)) + 
                geom_line(colour = "red") +                                
                geom_vline(xintercept = cross_interval, colour="blue", linetype = "longdash") + 
                xlab("Time interval") +
                ylab("Average steps")+
                ggtitle("Average steps by time interval of a day")
series_plot
```

![plot of chunk time_series_plot](figure/time_series_plot-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## the maximum number of steps
max_steps = max(average_steps$mean_steps)
max_steps
```

```
## [1] 206.1698
```

```r
## the average cross interval
cross_interval = average_steps[average_steps$mean_steps == max_steps,]$interval
cross_interval
```

```
## [1] 835
```
the average across interval (the blue line on the time series plot) is **835** for **206** maximum number of steps

## Imputing missing values

```r
## the total number of missing values in the dataset
total_na <- nrow(activity[is.na(activity$steps),])
total_na
```

```
## [1] 2304
```
there is **2304**  missing values in the data set.

### Filling the missing values in the data set


```r
## calculate the mean steps for each interval
mean_interval_steps <- ddply(activity, .(interval), summarise, 
                    mean_steps = mean(steps, na.rm = TRUE))

## merge activity and interval mean steps
m <- join(x = activity, y = mean_interval_steps, by = "interval")
m[is.na(m$steps),]$steps = m[is.na(m$steps),]$mean_steps

activity2 <- m[, c(1:3)]
```


```r
## total number of steps taken by day
dailysteps2 <- ddply(activity2, .(date), summarise, 
                    total_steps = sum(steps))

## Mean and median of the total number of steps taken per day
mean_step2 = round(mean(dailysteps2$total_steps))
median_step2 = round(median(dailysteps2$total_steps))

ve2 <- c(mean_step2, median_step2)
df2 <- data.frame(ve2)

## histogram of the total number of steps taken each day
histo2 <- ggplot(dailysteps2, aes(x= total_steps)) +
  geom_histogram(fill="lightblue", colour="black", binwidth=2000) +  
  geom_vline(aes(xintercept = mean_step2), color="red", linetype="dashed", size=1) +          
  geom_vline(aes(xintercept = median_step2), color="blue", linetype="dashed", size=1) +
  xlab("Total Number of steps") +
  ggtitle("The total number of steps taken each day")
histo2
```

![plot of chunk histogramm2](figure/histogramm2-1.png) 

### Mean and median

```r
## Mean and median of the total number of steps taken per day
mean_step2 = round(mean(dailysteps2$total_steps))
mean_step2
```

```
## [1] 10766
```

```r
median_step2 = round(median(dailysteps2$total_steps))
median_step2
```

```
## [1] 10766
```
the **mean** `10 766` and the **median**  `10 766` differs from the first estimated without empty values, they have the same value because of the filling of the NA values 

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels "weekday" and "weekend"



```r
days<- weekdays(as.Date(activity$date, ))
activity$day = days

weekend_flag = days=="Sunday" | days=="Saturday"

activity$type[weekend_flag] = "weekend"
activity$type[!weekend_flag] = "weekday"


activity$day = as.factor(activity$type)
activity <- activity[, c(1:4)]
```


### Plot in a time series

```r
## average number of steps by interval
new_average_steps <- ddply(activity, .(day, interval), summarise, 
                       mean_steps = mean(steps, na.rm = TRUE))

## Time series plot
series_plot2 <- ggplot( new_average_steps, aes(x=interval, y=mean_steps)) + 
                  geom_line(colour = "deepskyblue") +    
                  facet_wrap(~ day, ncol=1) +
                  theme_bw() +
                  theme(                  
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")) +                                     
                  xlab("Interval") +
                  ylab("Number of steps")

series_plot2
```

![plot of chunk time_series_plot2](figure/time_series_plot2-1.png) 
