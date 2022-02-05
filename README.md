# Cyclistic-Bike-Share

library(tidyverse)
library(ggplot2)
library(lubridate)
getwd()

Jan_2021 <- read_csv("202101-divvy-tripdata.csv")
Feb_2021 <- read_csv("202102-divvy-tripdata.csv")
Mar_2021 <- read_csv("202103-divvy-tripdata.csv")
Apr_2021 <- read_csv("202104-divvy-tripdata.csv")
May_2021 <- read_csv("202105-divvy-tripdata.csv")
Jun_2021 <- read_csv("202106-divvy-tripdata.csv")
Jul_2021 <- read_csv("202107-divvy-tripdata.csv")
Aug_2021 <- read_csv("202108-divvy-tripdata.csv")
Sep_2021 <- read_csv("202109-divvy-tripdata.csv")
Oct_2021 <- read_csv("202110-divvy-tripdata.csv")
Nov_2021 <- read_csv("202111-divvy-tripdata.csv")
Dec_2021 <- read_csv("202112-divvy-tripdata.csv")

colnames(Jan_2021)
colnames(Feb_2021)
colnames(Mar_2021)
colnames(Apr_2021)
colnames(May_2021)
colnames(Jun_2021)
colnames(Jul_2021)
colnames(Aug_2021)
colnames(Sep_2021)
colnames(Oct_2021)
colnames(Nov_2021)
colnames(Dec_2021)

#incosistent data type check
sapply(Jan_2021, class)
sapply(Feb_2021, class)
sapply(Mar_2021, class)
sapply(Apr_2021, class)
sapply(May_2021, class)
sapply(Jun_2021, class)
sapply(Jul_2021, class)
sapply(Aug_2021, class)
sapply(Sep_2021, class)
sapply(Oct_2021, class)
sapply(Nov_2021, class)
sapply(Dec_2021, class)

Jan_2021 <- Jan_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Feb_2021 <- Feb_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Mar_2021 <- Mar_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Apr_2021 <- Apr_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
May_2021 <- May_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Jun_2021 <- Jun_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Jul_2021 <- Jul_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Aug_2021 <- Aug_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Sep_2021 <- Sep_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Oct_2021 <- Oct_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Nov_2021 <- Nov_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))
Dec_2021 <- Dec_2021 %>% select(-c(start_station_name, start_station_id, end_station_name, end_station_id))

all_rides <- bind_rows(Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021, Jun_2021, Jul_2021, Aug_2021, Sep_2021, Oct_2021, Nov_2021, Dec_2021)

#=================================#
# table(all_rides$member_casual) #results in either "member" or "casual" 
# table(all_rides$rideable_type) #results in either "classic_bike", "docked_bike", or "electric_bike"

all_rides$date <- as.Date(all_rides$started_at)
all_rides$month <- format(as.Date(all_rides$date), "%m")
all_rides$day <- format(as.Date(all_rides$date), "%d")
all_rides$year <- format(as.Date(all_rides$date), "%Y")
all_rides$day_of_week <- format(as.Date(all_rides$date), "%A")

all_rides$ride_length <- difftime(all_rides$ended_at, all_rides$started_at)

all_rides_v2 <- all_rides[!(all_rides$rideable_type == "docked_bike" | all_rides$ride_length<0),]

#======================================#
# Average Number of Rides by Day: Members vs. Casual Riders
all_rides_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders")

# Average Riding Duration by Day: Members vs. Casual Riders
all_rides_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders")

#write.csv(all_rides_v2, file = "C:/Users/sgdra/Documents/R/Cyclistic Bike Share/all_rides_data.csv")


