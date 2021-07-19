# Load in libraries
library(dplyr)

# Set working directory
setwd("C:/Users/ctil9/OneDrive/Documents/R/Reproducible Research")

# Download files
download.file(fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              "./repdata.zip", 
              method = "curl")

unzip("./repdata.zip")

# Read in data
activity <- read.table("./activity.csv", 
                       header = TRUE,
                       sep = ",")

# Remove any rows with NAs
complete_activity <- activity[complete.cases(activity),]


## Total Daily Steps

# Group data by date
activity_grouped <- complete_activity %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps), n = n())

# Plot histogram of total daily steps
hist(activity_grouped$sum,
     xlab = "Total Steps per Day",
     ylab = "Frequency",
     main = "Histogram of Total Steps Per Day",
     col = "pink")

# Calculate mean and median
mean_total_steps <- mean(activity_grouped$sum)
median_total_steps <- median(activity_grouped$sum)


## Average Daily Pattern

# Group data by intervals
activity_grouped_v2 <- complete_activity %>% 
  group_by(interval) %>% 
  summarise(mean = mean(steps))

# Plot graph
plot(activity_grouped_v2$interval,
     activity_grouped_v2$mean,
     type = "l",
     xlab = "5 Minute Intervals During 24 Hour Period",
     ylab = "Average Steps",
     main = "Average Steps Per 5 Minute Interval")

# Find interval with the max mean steps
max_interval <- activity_grouped_v2[which.max(activity_grouped_v2$mean), 'interval']



## Missing Values

# Calculate the number of rows with missing values
nrow_all <- nrow(activity)
nrow_complete <- nrow(complete_activity)

nNAs <- nrow_all - nrow_complete


# Set all missing values as the mean of the interval rounded to the nearest whole number
activity_noNA <- merge(activity, activity_grouped_v2, by = "interval")

for (i in 1:nrow(activity_noNA)){
  if (is.na(activity_noNA$steps[i])) activity_noNA$steps[i] <- ceiling(activity_noNA$mean[i])
}

# Group data in new data set by date
activity_noNA_groupedbydate <- activity_noNA %>% 
  group_by(date) %>% 
  summarise(sum = sum(steps), n = n())

# Plot histogram of the new data set with no NA values
hist(activity_noNA_groupedbydate$sum,
     xlab = "Total Steps per Day",
     ylab = "Frequency",
     main = "Histogram of Total Steps Per Day",
     col = "pink")


# Calculate mean and median of new data set
mean_total_steps_noNA <- mean(activity_noNA_groupedbydate$sum)
median_total_steps_noNA <- median(activity_noNA_groupedbydate$sum)

# Find the difference between the original mean and median, and the new mean and median

meandiff <- mean_total_steps - mean_total_steps_noNA
mediandiff <- median_total_steps - median_total_steps_noNA


# The mean has decreased by 18.73 
# The median has decreased by 144


## Differences between weekends and weekdays

# Add in the days of week
activity_noNA$weekday <- weekdays(as.Date(activity_noNA$date))

# Create weekday/weekend flag
weekend <- c("Saturday", "Sunday")
activity_noNA$weekday_weekend <- NA

for(i in 1:nrow(activity_noNA)){
  ifelse(activity_noNA$weekday[i] %in% weekend, 
         activity_noNA$weekday_weekend[i] <- "weekend", 
         activity_noNA$weekday_weekend[i] <- "weekday")
} 


# Group data by weekends/weekdays
activity_weekend <- activity_noNA[activity_noNA$weekday_weekend == "weekend",]

# Group data by intervals
activity_weekend_grpd <- activity_weekend %>% 
  group_by(interval) %>% 
  summarise(mean = mean(steps))

# Plot weekends graph
plot(activity_weekend_grpd$interval,
     activity_weekend_grpd$mean,
     type = "l",
     xlab = "5 Minute Intervals During 24 Hour Period",
     ylab = "Average Steps",
     main = "Average Steps Per 5 Minute Interval During the Weekend")












