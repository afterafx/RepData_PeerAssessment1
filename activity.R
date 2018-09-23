library(ggplot2)
activityData <- read.csv(file = "activity.csv")

## Question 1 ########################################

# dataframe containing the sum of the steps per date
stepsYear <- aggregate(steps ~ date, activityData, sum)

# plotting the dataframe
ggplot(stepsYear, aes(x = steps)) +
  geom_histogram() +
  labs(title = "Total Number of Steps Per Day") +
  theme_minimal()

# Mean of the sum of total steps
mean(stepsYear$steps)

# Median of the sum of total steps
median(stepsYear$steps)

## Question 2 ########################################

# dataframe containing the average steps per 5 min interval
intervalMean <- aggregate(activityData$steps, by=list(activityData$interval), FUN = mean, na.rm=TRUE)
names(intervalMean) <- c("interval", "mean")

# plotting the dataframe
plot(intervalMean$interval, intervalMean$mean, type = "l",
     xlab = "Intervals", ylab = "Mean", main = "Average Number of Steps Per Interval")

# Find the interval with the max mean
intervalMean[which.max(intervalMean$mean), ]$interval

## Question 3 ########################################

## Number of NA values in Steps
sum(is.na(activityData$steps))

# Creating dataframe to fill NA values
filledNAData <- activityData

# logical vector containing just the rows with NA values
naData <- is.na(filledNAData$steps)

# dataframe containing no NA values
missingNAData <- activityData[!is.na(activityData$steps), ]

# Vector of the interval mean data
intervalAvg <- tapply(missingNAData$steps, missingNAData$interval, mean, na.rm=TRUE, simplify=T)

# fill the dataframe with interval data for the NA values
filledNAData$steps[naData] <- intervalAvg[as.character(filledNAData$interval[naData])]

# dataframe containing sum of the steps per day with NA values filled
stepsYearFilled <- aggregate(steps ~ date, filledNAData, sum)

# plotting of the dataframe
ggplot(stepsYearFilled, aes(x = steps)) +
  geom_histogram() +
  labs(title = "Total Number of Steps Per Day with no NA values") +
  theme_minimal()

# mean total number of steps.
mean(stepsYearFilled$steps)

# median total number of steps
median(stepsYearFilled$steps)

## Question 4 ########################################

# Changed Date variable to Date format
filledNAData$date <- as.Date(filledNAData$date)

# Create weekday variable to hold the days of a weekday
weekday <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', "Friday")

# Add a column to differeniate the days
filledNAData$wDay <- factor((weekdays(filledNAData$date) %in% weekday),
                            levels = c(FALSE, TRUE), labels = c('weekend', 'weekday'))

# Average the number of steps per interval
stepsDay <- aggregate(steps ~ interval + wDay, filledNAData, mean, na.rm = TRUE)

# Plot the data based on weekday
ggplot(stepsDay, aes(x = interval, y = steps, color = wDay)) +
  geom_line() +
  facet_grid(~wDay) +
  labs(title = "Average Steps by Intervals (Type of Weekday)", x = "Interval", y = "Average Steps")
