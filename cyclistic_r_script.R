library(tidyverse)
library(conflicted)

# Set dplyr as the default choices
conflict_prefer("filter","dplyr")
conflict_prefer("lag", "dplyr")

#
# Collect Data
#

# Upload 2023 Divvy data sets
jan_2023 <- read.csv("202301-divvy-tripdata.csv")
feb_2023 <- read.csv("202302-divvy-tripdata.csv")
mar_2023 <- read.csv("202303-divvy-tripdata.csv")
apr_2023 <- read.csv("202304-divvy-tripdata.csv")
may_2023 <- read.csv("202305-divvy-tripdata.csv")
jun_2023 <- read.csv("202306-divvy-tripdata.csv")
jul_2023 <- read.csv("202307-divvy-tripdata.csv")
aug_2023 <- read.csv("202308-divvy-tripdata.csv")
sep_2023 <- read.csv("202309-divvy-tripdata.csv")
oct_2023 <- read.csv("202310-divvy-tripdata.csv")
nov_2023 <- read.csv("202311-divvy-tripdata.csv")
dec_2023 <- read.csv("202312-divvy-tripdata.csv")

#
# Create one big data frame
#

all_trips <- bind_rows(jan_2023, feb_2023, mar_2023, apr_2023, may_2023, jun_2023, jul_2023,
                       aug_2023, sep_2023, oct_2023, nov_2023, dec_2023)

#
# Clean up and add data to prepare for analysis
#

# Convert 'started_at' and 'ended_at' to consistent date format
all_trips$started_at <- parse_date_time(all_trips$started_at, orders = c("mdy HM", "%Y-%m-%d %H:%M:%S"), tz = "UTC")
all_trips$ended_at <- parse_date_time(all_trips$ended_at, orders = c("mdy HM", "%Y-%m-%d %H:%M:%S"), tz = "UTC")

# Inspect new table
colnames(all_trips) # list column names
nrow(all_trips) # how many rows are in data frame
dim(all_trips) # dimensions of the data frame
head(all_trips) # see the first 6 rows of data frame
summary(all_trips) # statistical summary of data for numerics


# Add columns that list the date, month, day, and year of each ride
# This will allow the ability to aggregate ride data for each
#   month, day, and year

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <-difftime(all_trips$ended_at, all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The "all_trips" data frame contains a few hundred entries where
  #"trip_length" is less than 0
# Will create a new version of the data frame (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$ride_length < 0),]

#
# Conduct Descriptive Analysis
#
# Analysis on ride_length in seconds
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# Fix the days of the week being out of order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday",
                                              "Tuesday", "Wednesday", 
                                              "Thursday", "Friday", "Saturday"))

# Average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
arrange(member_casual, weekday) # sorts

# Visualize the number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(
    aes(x = weekday, y = number_of_rides, fill = member_casual)
    ) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Number of Rides each Weekday",
    subtitle = "Rides in 2023",
    caption = "Created using ggplot",
    x = "Weekday", y = "Number of Rides",
    fill = "Rider Type"
  )

# Visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)/60) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#
# Export Summary File for further analysis
#
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')