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
steps = read.csv("./repdata_data_activity/activity.csv", na.strings = "NA")
str(steps)

# 2.Histogram of the total number of steps taken each day

st <- hist(as.numeric(steps$steps)), col = "blue", main = "Total Steps Per Day", xlab = "Steps"

# 3.Mean and median number of steps taken each day
# 4.Time series plot of the average number of steps taken
# 5.The 5-minute interval that, on average, contains the maximum number of steps
# 6.Code to describe and show a strategy for imputing missing data
table(is.na(steps$steps))
# 7.Histogram of the total number of steps taken each day after missing values are imputed
# 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# 9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

