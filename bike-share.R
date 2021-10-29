# Install packages
install.packages('here')
install.packages('skimr')
install.packages('janitor')
install.packages('dplyr')

# Load Packages
library(tidyverse) 
library(lubridate)  
library(ggplot2)  
library(here)
library(skimr)
library(janitor)
library(dplyr)

# Load files
jan_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202101-divvy-tripdata.csv")
feb_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202102-divvy-tripdata.csv")
mar_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202103-divvy-tripdata.csv")
apr_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202104-divvy-tripdata.csv")
may_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202105-divvy-tripdata.csv")
jun_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202106-divvy-tripdata.csv")
jul_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202107-divvy-tripdata.csv")
aug_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202108-divvy-tripdata.csv")
sep_21 <- read_csv("Documents/Data_Projects/Projects/Case Study First/Redo/Dataset/202109-divvy-tripdata.csv")

# Review Datasets
# Check coloumn names
colnames(jan_21)
colnames(feb_21)
colnames(mar_21)
colnames(apr_21)
colnames(may_21)
colnames(jun_21)
colnames(jul_21)
colnames(aug_21)
colnames(sep_21)

# Check datatypes
str(jan_21)
str(feb_21)
str(mar_21)
str(apr_21)
str(may_21)
str(jun_21)
str(jul_21)
str(aug_21)
str(sep_21)

# Combine all data into one
all_trips <- bind_rows(jan_21, feb_21, mar_21, apr_21, may_21, jun_21, jul_21, aug_21, sep_21)

# Review cobined data
colnames(all_trips)
str(all_trips)
View(all_trips)
n_distinct(all_trips$member_casual)
n_distinct(all_trips$rideable_type)
unique(all_trips$member_casual)
unique(all_trips$rideable_type)
skim_without_charts(all_trips)

# Reformat all date 
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Clean names
clean_names(all_trips)

# Find duratiion
all_trips$ride_length_sec <- difftime(all_trips$ended_at,all_trips$started_at)
# Convert to minutes and hour
all_trips$ride_length_min <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")
all_trips$ride_length_hour <- difftime(all_trips$ended_at,all_trips$started_at, units = "hours")

# Compare members and casual users
aggregate(all_trips$ride_length_min ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length_min ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length_min ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length_min ~ all_trips$member_casual, FUN = min)

# Drop negative duration rows
all_trips<-all_trips[!(all_trips$ride_length_min < 0),]
# Check minumum values again
aggregate(all_trips$ride_length_min ~ all_trips$member_casual, FUN = min)

# Check mean of duration by usertype and day of week
aggregate(all_trips$ride_length_min ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
aggregate(all_trips$ride_length_min ~ all_trips$member_casual + all_trips$day_of_week, FUN = sum)

all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  drop_na() %>% 
  summarise(mean_ride_length_min = mean(ride_length_min))

# Reorder day of week
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_min)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sort


# Visualization of rides by rider type
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Visualization for average duration
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips$ride_length_min ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/all_trips_Second.csv')

#Export dataset for further analysis in SQL & Visualisation in Tableau
write.csv(all_trips, file = '~/Desktop/all_trips_Second.csv')

