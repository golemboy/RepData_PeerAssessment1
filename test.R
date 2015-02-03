require("ggplot2")
require("plyr")

## unzip an load the data
unzip("activity.zip")
activity  <- read.csv("activity.csv", header = TRUE, sep = ",")
file.remove("activity.csv")

## total number of steps taken by day
dailysteps <- ddply(activity, .(date), summarise, 
                total_steps = sum(steps, na.rm = TRUE))

## Mean and median of the total number of steps taken per day
mean_step = round(mean(dailysteps$total_steps))
median_step = median(dailysteps$total_steps)
mean_step
median_step
ve <- c(mean_step, median_step)
df <- data.frame(ve)

## histogram of the total number of steps taken each day
histo <- ggplot(dailysteps, aes(x= total_steps)) +
          geom_histogram(fill="lightblue", colour="black", binwidth=1000) +
          #geom_vline(aes(xintercept = mean_step), color="red", linetype="dashed", size=1) +          
          #geom_vline(aes(xintercept = median_step), color="blue", linetype="dashed", size=1) +
          geom_vline(data=df, aes(xintercept=ve), colour=factor(ve), linetype="dashed", size=1, show_guide = TRUE)+
          xlab("Total Number of steps") +
          ggtitle("The total number of steps taken each day")
histo


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

## the total number of missing values in the dataset
nrow(activity[is.na(activity$steps),])

## calcuate the mean steps for each interval (include NA)
mean_interval_steps <- ddply(activity, .(interval), summarise, 
                    mean_steps = mean(steps, na.rm = TRUE))


## merge activity and interval mean steps
m <- join(x = activity, y = mean_interval_steps, by = "interval")
m[is.na(m$steps),]$steps = m[is.na(m$steps),]$mean_steps

activity2 <- m[, c(1:3)]

## total number of steps taken by day
dailysteps2 <- ddply(activity2, .(date), summarise, 
                    total_steps = sum(steps))

## Mean and median of the total number of steps taken per day
mean_step2 = round(mean(dailysteps2$total_steps))
median_step2 = round(median(dailysteps2$total_steps))
mean_step2
median_step2
ve2 <- c(mean_step2, median_step2)
df2 <- data.frame(ve2)

## histogram of the total number of steps taken each day
histo2 <- ggplot(dailysteps2, aes(x= total_steps)) +
  geom_histogram(fill="lightblue", colour="black", binwidth=1000) +  
  geom_vline(data=df2, aes(xintercept=ve2), colour=factor(ve2), linetype="dashed", size=1, show_guide = TRUE)+
  xlab("Total Number of steps") +
  ggtitle("The total number of steps taken each day")
histo2

## Set Locale English to avoid French days
Sys.setlocale("LC_TIME", "English")
days<- weekdays(as.Date(activity$date, ))
activity$day = days

weekend_flag = days=="Sunday" | days=="Saturday"

activity$type[weekend_flag] = "weekend"
activity$type[!weekend_flag] = "weekday"
#days[!weekend_flag]$type = "weekday"

activity$day = as.factor(activity$type)
activity <- activity[, c(1:4)]

## average number of steps by interval
new_average_steps <- ddply(activity, .(day, interval), summarise, 
                       mean_steps = mean(steps, na.rm = TRUE))

## Time series plot
series_plot2 <- ggplot( new_average_steps, aes(x=interval, y=mean_steps)) + 
                  geom_line(colour = "blue") +    
                  #theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
                  facet_grid(day ~ .) +
                  theme_bw() +
                  theme(
                    #panel.border = element_blank(), 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    axis.line = element_line(colour = "black")
                    )+
                                     
                  xlab("Time interval") +
                  ylab("Average steps") +
                  ggtitle("Average steps by time interval of a day")
series_plot2


