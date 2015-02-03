require("ggplot2")
require("plyr")

## unzip an load the data
unzip("activity.zip")
activity  <- read.csv("activity.csv", header = TRUE, sep = ",")

## total number of steps taken by day
dailysteps <- ddply(activity, .(date), summarise, 
                total_steps = sum(steps, na.rm = TRUE))

## histogram of the total number of steps taken each day
histo <- ggplot(dailysteps, aes(x= total_steps)) +
          geom_histogram(fill="lightblue", colour="black") +
          xlab("Total Number of steps") +
          ggtitle("The total number of steps taken each day")
histo

## Mean and median of the total number of steps taken per day
round(mean(dailysteps$total_steps))
median(dailysteps$total_steps)

## average number of steps by interval
average_steps <- ddply(activity, .(interval), summarise, 
                  mean_steps = mean(steps, na.rm = TRUE))

## the maximum number of steps
max_steps = max(average_steps$mean_steps)
cross_interval = average_steps[average_steps$mean_steps == max_steps,]$interval

## Time series plot
series_plot <- ggplot( average_steps, aes(x=interval, y=mean_steps)) + 
                geom_line(colour = "red") +                                
                geom_vline(xintercept = cross_interval, colour="blue", linetype = "longdash") + 
                xlab("Time interval") +
                ylab("Average steps")+
                ggtitle("Average steps by time interval of a day")
series_plot



