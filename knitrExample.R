# This assignment makes use of data from a personal activity monitoring device. This device collects data 
# at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual 
# collected during the months of October and November, 2012 and include the number of steps taken in 5 minute 
# intervals each day.

# Downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip at "2016-05-10 18:06:01 CDT"
# to working directory.

#The variables included in this dataset are:

    #steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
    #date: The date on which the measurement was taken in YYYY-MM-DD format
    #interval: Identifier for the 5-minute interval in which measurement was taken

#The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# 1.Code for reading in the dataset and/or processing the data
data = read.csv("./repdata_data_activity/activity.csv", na.strings = "NA")
data$steps = as.numeric(data$steps)

# 2.Histogram of the total number of steps taken each day
dailySteps = with(data, tapply(steps, date, sum, na.rm = TRUE))
hist(dailySteps, breaks = 10, col = "blue", xlab = "steps per day", ylab = "frequency: days", main = "")

# 3.Mean and median number of steps taken each day
mean(dailySteps)
median(dailySteps)

# 4.Time series plot of the average number of steps taken
plot(dailySteps, type = "l", ylab = "total steps", xlab = "daily counts: Oct - Nov 2012", axes = FALSE, col = "blue", lwd = 2)

# 5.The 5-minute interval that, on average, contains the maximum number of steps
meanInt = with(data, tapply(steps, interval, mean, na.rm = TRUE))
meanInt[which(meanInt == max(meanInt))]

# How many records are NA for step count?
table(is.na(data$steps))
# 6.Code to describe and show a strategy for imputing missing data
# Prepare to impute NA values using mean for interval
meanInt = data.frame(unique(data$interval),meanInt)
colnames(meanInt) = c("interval", "avgsteps")
meanInt$avgsteps = as.numeric(meanInt$avgsteps)
# Merge mean interval data with original dataframe
library(plyr)
dataImpute = join(data, meanInt, by = "interval", type = "left", match = "all")

# Add column for imputed values, replacing NAs with interval average steps
dataImpute$newSteps = dataImpute$steps
stepsNA = is.na(dataImpute$newSteps)
dataImpute$newSteps[stepsNA] = dataImpute$avgsteps[stepsNA]

# 7.Histogram of the total number of steps taken each day after missing values are imputed
newSteps = with(dataImpute, tapply(newSteps, date, sum, na.rm = TRUE))
hist(newSteps, breaks = 10, col = "blue", xlab = "steps per day w/ imputed values", ylab = "frequency: days", main = "")

# 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# Adding weekday/weekend varaible...
dataImpute$date = as.Date(dataImpute$date)
weekpart = weekdays(dataImpute$date)
dataImpute = data.frame(dataImpute, weekpart)
dataImpute$weekpart = sub("[MTWF].*", "weekday", dataImpute$weekpart)
dataImpute$weekpart = sub("S.*", "weekend",dataImpute$weekpart)
#Panel plot: weekday v. weekend...
wkdy = which(dataImpute$weekpart == "weekday")
wknd = which(dataImpute$weekpart == "weekend")
par(mfcol = c(2,1))
plot1 = plot(dataImpute$interval[wkdy], dataImpute$newSteps[wkdy], type = "l", ylab = "total steps", xlab = "weekdays: Oct - Nov 2012 ", axes = FALSE, col = "blue")
plot2 = plot(dataImpute$interval[wknd], dataImpute$newSteps[wknd], type = "l", ylab = "total steps", xlab = "weekends: Oct - Nov 2012 ", axes = FALSE, col = "blue")
