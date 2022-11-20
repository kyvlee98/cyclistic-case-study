### Cyclistic Rider Analysis ###

# The purpose of this script is to combine downloaded monthly trip data into a
# consolidated data frame and perform calculations and analysis in order to 
# answer the key question of the project: In what ways do members and casual 
# riders use bikes differently?

# Install required packages
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)

getwd()
setwd("/Users/kyven/Desktop/google data analytics/capstone project/data")

#=================================
# Step 1: COLLECT AND IMPORT DATA
#=================================
data1 <- read_csv("202111-divvy-tripdata.csv")
data2 <- read_csv("202112-divvy-tripdata.csv")
data3 <- read_csv("202201-divvy-tripdata.csv")
data4 <- read_csv("202202-divvy-tripdata.csv")
data5 <- read_csv("202203-divvy-tripdata.csv")
data6 <- read_csv("202204-divvy-tripdata.csv")
data7 <- read_csv("202205-divvy-tripdata.csv")
data8 <- read_csv("202206-divvy-tripdata.csv")
data9 <- read_csv("202207-divvy-tripdata.csv")
data10 <- read_csv("202208-divvy-tripdata.csv")
data11 <- read_csv("202209-divvy-publictripdata.csv")
data12 <- read_csv("202210-divvy-tripdata.csv")

#=====================================================
# Step 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#=====================================================

# Compare column names each of the files to make sure all column names match
# perfectly before combining them into a single data frame
colnames(data1)
colnames(data2)
colnames(data3)
colnames(data4)
colnames(data5)
colnames(data6)
colnames(data7)
colnames(data8)
colnames(data9)
colnames(data10)
colnames(data11)
colnames(data12)

# Stack individual month's data frames into one big data frame
tripdata <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, 
                      data9, data10, data11, data12)

#=======================================================
# Step 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#=======================================================

# Inspect the new table that has been created
head(tripdata)
colnames(tripdata)
str(tripdata)
summary(tripdata)

# To see how many observations fall under each user type
table(tripdata$member_casual)

# Add columns that list the date, month, day, and year of each ride
tripdata$date <- as.Date(tripdata$started_at)
tripdata$month <- format(as.Date(tripdata$date), "%m")
tripdata$day <- format(as.Date(tripdata$date), "%d")
tripdata$year <- format(as.Date(tripdata$date), "%y")
tripdata$day_of_week <- format(as.Date(tripdata$date), "%A")
#or tripdata$day_of_week <- weekdays(tripdata$date)

# Add column/calculation called "ride_length" 
tripdata$ride_length <- difftime(tripdata$ended_at, tripdata$started_at)

# View and check structure of new columns
colnames(tripdata)
str(tripdata)

# Convert ride_length to numeric so we can run calculations
tripdata$ride_length <- as.numeric(tripdata$ride_length)

# Confirm it has been properly converted to numeric *returns TRUE*
is.numeric(tripdata$ride_length)

# Remove bad data
# where ride_length is negative (not possible)
# Guide: https://www.datasciencemadesimple.com/
# delete-or-drop-rows-in-r-with-conditions-2/
tripdata_v2 <- tripdata[!(tripdata$ride_length<0),]

#==========================
# Step 4: CONDUCT ANALYSIS
#==========================

# Statistical summary of ride_length
max(tripdata_v2$ride_length)
min(tripdata_v2$ride_length)
median(tripdata_v2$ride_length)
mean(tripdata_v2$ride_length)

# OR
summary(tripdata_v2$ride_length)

# Compare members and casual users
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = mean)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = median)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = max)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = min)

# Average ride time by each day for members vs casual users
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual + 
            tripdata_v2$day_of_week, FUN = mean)

# Reorder the days of the week
tripdata_v2$day_of_week <- ordered(tripdata_v2$day_of_week, 
                                   levels=c("Sunday", "Monday", "Tuesday", 
                                            "Wednesday", "Thursday", "Friday", 
                                            "Saturday"))

# Analyze ridership data by type and weekday

# Visualize the number of rides by rider type
tripdata_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) + 
  geom_col(position="dodge")

# Visualize average duration
tripdata_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) + 
  geom_col(position="dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================

counts <- aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual + 
                      tripdata_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/google data analytics/
          capstone project/data/avg_ride_length.csv')

