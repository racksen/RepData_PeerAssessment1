library(ggplot2)
library(xtable)
# Loading and preprocessing the data
zipFile <- unzip("activity.zip")
activityDF <- read.csv(zipFile, colClasses = c("numeric", "Date", "numeric"))
#1. What is mean total number of steps taken per day?

##1.1 Calculating total number of steps taken per day and draw histogram
activityDFByDate <- aggregate(steps ~ date, activityDF, sum, na.rm=TRUE)
hist(activityDFByDate$steps, col = "skyblue",  main = "#Steps Taken Each Day", xlab="Steps",  breaks = 20)

##1.2 Calculating mean and median of the total number of steps taken per day
median(activityDFByDate$steps)
mean(activityDFByDate$steps)

#2. What is the average daily activity pattern?

##2.1 Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
activityDFByInterval <- aggregate(steps ~ interval, activityDF, mean, na.rm = TRUE)
ggplot(activityDFByInterval, aes(x=interval,y=steps)) + geom_line(color="blue",size=1) +  labs(x="Interval",y="Average Number of Steps") 

##2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
activityDFByInterval[which.max(activityDFByInterval$steps),]$interval

#3. Imputing missing values

##3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activityDF))

##3.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Missing step values (NA) were replaced by the mean of 5 minute interval

##3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityDFByInterval <- aggregate(steps ~ interval, activityDF, mean, na.rm = TRUE)
activityDFImpute <- merge(activityDF, activityDFByInterval, by=c("interval"))
activityDFImpute <- transform(activityDFImpute, steps.x = ifelse(is.na(steps.x),steps.y,steps.x))
activityDFImpute <- data.frame(activityDFImpute[,1:4])
activityDFImpute$steps.x <- round(activityDFImpute$steps.x, digits = 0)
names(activityDFImpute) <- c("interval", "steps","date", "steps.y")
activityDFImpute <- activityDFImpute[,c("steps","date","interval")]
activityDFImpute <- activityDFImpute[order(activityDFImpute$date),]


##3.4.1 Make a histogram of the total number of steps taken each day. 
activityDFImputeAgg <- aggregate(steps ~ date, activityDFImpute, sum, na.rm = TRUE)
hist(activityDFImputeAgg$steps, col = "skyblue",  main = "#Steps Taken Each Day of Imputes dataset", xlab="Steps",  breaks = 20)

##3.4.2 Mean total number of steps taken per day.
mean(activityDFImputeAgg$steps)

##3.4.3 Median total number of steps taken per day.
median(activityDFImputeAgg$steps)

##3.4.4 Do these values differ from the estimates from the first part of the assignment?
## The mean is differing slightly and the median is changed down to 3 steps 
##mean(activityDFByDate$steps) = 10766.19; mean(activityDFImputeAgg$steps) = 10765.64. 
##median(activityDFByDate$steps) = 10765; median(activityDFImputeAgg$steps) = 10762 . 

activityDFByDate_Summary <- c(mean(activityDFByDate$steps),median(activityDFByDate$steps))
activityDFImputeAgg_Summary<-c(mean(activityDFImputeAgg$steps),median(activityDFImputeAgg$steps))
acitvitySummary <- rbind(activityDFByDate_Summary,activityDFImputeAgg_Summary)
colnames(acitvitySummary) <-c("Mean","Median")
xt <- xtable(acitvitySummary)
print(xt, type="html")

##3.4.5 What is the impact of imputing missing data on the estimates of the total daily number of steps?
# The imputed estimate reduce the actual mean and median as per the expectation because adding missing value instances with average value will increase the frequency thereby decrease in mean an median. As we replace the NA with average steps, the values are differing slightly. 

#4. Are there differences in activity patterns between weekdays and weekends?
##4.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activityDFImpute$day <- weekdays(activityDFImpute$date)
activityDFImpute$dayType <- ifelse(activityDFImpute$day %in% c("Saturday", "Sunday"),"weekend", "weekday")
activityDFImputeDayAgg <- aggregate(steps ~ interval + dayType, activityDFImpute, mean)
##4.2 Make a panel plot containing a time series plot
ggplot(activityDFImputeDayAgg, aes(x=interval,y=steps)) +   geom_line(color="skyblue",size=1) +   facet_wrap(~dayType, nrow=2, ncol=1) +   labs(x="Interval",y="Number of Steps") + theme_bw()
