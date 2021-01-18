#Libraries & Dataset-------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(readr)
library(forcats)
library(sp)
library(sf)
library(leaflet)
library(zoo)
library(stringr)
library(chron)
library(waffle)

# Data Source: https://www.divvybikes.com/system-data

df <- read.csv("Divvy_Trips_Small.csv")

names(df)
head(df)
dim(df)

#Cleaning the dataset------------------------------------------
#1. Rename columns

df <- df %>%
  rename(trip_id = ï..TRIP.ID,
         start_time = START.TIME,
         stop_time = STOP.TIME,
         bike_id = BIKE.ID,
         trip_duration = TRIP.DURATION,
         from_station_id = FROM.STATION.ID,
         from_station_name = FROM.STATION.NAME,
         to_station_id = TO.STATION.ID,
         to_station_name = TO.STATION.NAME,
         user_type = USER.TYPE,
         gender = GENDER,
         birth_year = BIRTH.YEAR,
         from_latitude = FROM.LATITUDE,
         from_longitude = FROM.LONGITUDE,
         from_location = FROM.LOCATION,
         to_latitude = TO.LATITUDE,
         to_longitude = TO.LONGITUDE,
         to_location = TO.LOCATION)

#2. Change date columns into month, year, day

#   - NOTE: Minute, hour, day, month, and year columns will be determined
#     by the ride start date. However, trip_duration will be kept to make
#     calculations regarding trip lengths that may start and end on diff days.

df$start_time <- as.POSIXct(df$start_time, format="%m/%d/%Y %H:%M", tz="America/Chicago")
df$stop_time <- as.POSIXct(df$stop_time, format="%m/%d/%Y %H:%M", tz="America/Chicago")
df$year <- format(df$start_time, "%Y")
df$month <- format(df$start_time, "%m")
df$dayofweek <- as.factor(as.POSIXlt(df$start_time)$wday)
df$dayofyear <- yday(df$start_time)
df$day <- format(df$start_time, "%d")
df$hour <- format(df$start_time, "%H")
df$minute <- format(df$start_time, "%M")
df$weekend <- is.weekend(df$start_time)

#3. Column additions (age, age group, trip duration group)

df$age <- 2020 - as.numeric(df$birth_year)

age_grp_labs <- c("21-30", "31-40", "41-50", "51-60", "61+")
df$age_group <- cut(df$age, breaks=c(20, 30, 40, 50, 60, 100), labels=age_grp_labs, right=FALSE)

trip_dur_labs <- c("0-250", "251-500", "501-750", "751-1000", "1001-1250", "1251-1500", "1501-1750", "1751-2000", "2000-2250", "2251-2500", "2500+")
df$trip_dur_group <- cut(df$trip_duration,
                         breaks=c(0,250,500,750,1000,1250,
                                  1500,1750,2000,2250,2500,86400),
                         labels=trip_dur_labs, right=FALSE)

#4. Only 2014 data

df <- filter(df, year==2014)

#Exploratory Data Analysis

#Riding To and From----------------------------------------------------

#  Function which calculations number of rides to or from (or two and from)
#  a particular station.
num_rides_total <- function(station_id) {
  x <- filter(df, from_station_id==station_id | to_station_id==station_id)
  y <- filter(df, to_station_id==station_id & from_station_id==station_id)
  data.frame(station_id, count(rbind(x, y)))
}

#  Full list of stations alongside number of rides to/from that station
num_rides_stations <- do.call(rbind.data.frame,
                              lapply(unique(df$from_station_id),
                                     num_rides_total))

#  List ordered by decreasing number of rides
popular <- num_rides_stations[order(num_rides_stations$station_id, decreasing=TRUE),]

station_id_name <- select(df, from_station_id, from_station_name)

station_id_name <- station_id_name[!duplicated(station_id_name$from_station_id),]

station_id_name <- station_id_name[order(station_id_name$from_station_id, decreasing=TRUE),]

station_id_name <- station_id_name %>%
  select(from_station_name)

popular <- cbind(popular, station_id_name)

popular <- popular[order(popular$n, decreasing=TRUE),] %>%
  rename(num_total_rides=n, station_name=from_station_name) %>%
  select(-station_id)

popular <- popular[, c("station_name", "num_total_rides")]

head(popular)
tail(popular)

#Net Rides----------------------------------------

num_rides_diff <- function(station_id)
{
  out_rides <- filter(df, from_station_id==station_id)
  in_rides <- filter(df, to_station_id==station_id)
  data.frame(station_id, count(in_rides)-count(out_rides))
}

net_rides <- do.call(rbind.data.frame, lapply(unique(df$from_station_id),
                                              num_rides_diff))

net_rides <- net_rides[order(net_rides$station_id, decreasing=TRUE),]

net_rides <- cbind(net_rides, station_id_name)

net_rides <- net_rides[order(net_rides$n, decreasing=TRUE),] %>%
  rename(ride_diff=n, station_name=from_station_name) %>%
  select(-station_id)

net_rides <- net_rides[, c("station_name", "ride_diff")]

net_rides <- rbind(head(net_rides), tail(net_rides))

mutate(net_rides, station_name=fct_reorder(station_name, desc(ride_diff))) %>%
  ggplot(aes(station_name, ride_diff, fill=ride_diff<0)) +
  geom_bar(stat="identity") +
  labs(title="Net Rides by Divvy Station", caption="Data Source: Divvy 2014",
       x="", y="Net Rides") +
  theme(legend.position="none", axis.ticks=element_blank(),
        axis.text.x=element_text(angle=270)) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("green4", "firebrick"))

#Basic Demographic Barplots-----------------------------------------------

# Divvy Rides by User Type
ride_user_type <- table(df$user_type)

waffle(ride_user_type/1000, rows=24, xlab="1 square = 1000 rides",
       colors=c("#F8766D", "#00BFC4"))

# Divvy Rides by Gender
ride_gender <- table(filter(df, !(gender==""))$gender)

waffle(ride_gender/1000, rows=20, xlab="1 square = 1000 rides",
       colors=c("white", "#F8766D", "#00BFC4")) +
  theme(legend.title=element_blank())

# Divvy Rides by Age Group
ride_age <- table(df$age_group)

waffle(ride_age/1000, rows=20, xlab="1 square = 1000 rides") +
  theme(legend.title=element_text("Age Group"))

#Rides by Hour and Category---------------

# Rides by Hour and Type of Weekday

wday_hour <- df %>%
  group_by(hour, weekend) %>%
  summarize(total=n())

ggplot(wday_hour, aes(hour, total, color=weekend, group=weekend)) +
  geom_line() +
  labs(title="Divvy Rides Every Hour", subtitle="Grouped by Weekday vs Weekend",
       caption="Data Source: Divvy 2014", x="Hour", y="Total Rides") +
  scale_y_continuous(labels=comma) +
  scale_color_manual(labels=c("Weekday", "Weekend"),
                     values=c("#00BFC4", "#F8766D")) +
  theme(legend.title=element_blank())

# Rides by Hour and Day of Week

hour_wday_rides <- df %>%
  group_by(hour, dayofweek) %>%
  summarize(total=n())

ggplot(hour_wday_rides, aes(hour, total,
                            color=dayofweek, group=dayofweek)) +
  geom_line() +
  labs(title="Divvy Rides Every Hour", subtitle="Grouped by Day of Week",
       caption="Data Source: Divvy 2014", x="Hour",
       y="Total Rides") +
  guides(color=guide_legend("Day of Week")) +
  scale_y_continuous(labels=comma) +
  scale_color_discrete(labels=c("Sunday", "Monday",
                                "Tuesday", "Wednesday",
                                "Thursday", "Friday",
                                "Saturday"))

nrow(filter(df, ((hour>=7 & hour<=9) | (hour>=16 & hour<=18))))/nrow(df)

# Rides by Hour and Usertype

user_hour <- df %>%
  group_by(hour, user_type) %>%
  summarize(total=n())

ggplot(user_hour, aes(hour, total, color=user_type, group=user_type)) +
  geom_line() +
  labs(title="Divvy Rides Every Hour", subtitle="Grouped by User Type",
       caption="Data Source: Divvy 2014", x="Hour", y="Total Rides") +
  scale_y_continuous(labels=comma) +
  theme(legend.title=element_blank())

# Rides by User Type Grouped by Day of Week

user_wday <- df %>%
  group_by(user_type, dayofweek) %>%
  summarize(total=n())

ggplot(user_wday, aes(dayofweek, total, fill=dayofweek)) +
  geom_bar(stat="identity") +
  labs(title="Divvy Rides Grouped by Day of Week",
       subtitle="Faceted by User Type", x="", y="Total Rides") +
  facet_grid(cols=vars(user_type)) +
  scale_y_continuous(labels=comma) +
  scale_x_discrete(labels=c("Sun", "Mon", "Tue", "Wed",
                            "Thu", "Fri", "Sat")) +
  theme(legend.position="none")

# Rides by Hour and Gender

gender_hour <- df %>% filter(!(gender=="")) %>%
  group_by(hour, gender) %>%
  summarize(total=n())

ggplot(gender_hour, aes(hour, total, fill=gender)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Divvy Rides Every Hour", subtitle="Grouped by Gender",
       caption="Data Source: Divvy 2014",
       x="Hour", y="Total Rides") +
  scale_y_continuous(labels=comma) +
  theme(legend.title=element_blank())

# Rides by Hour and Month

month_hour <- df %>%
  group_by(hour, month) %>%
  summarize(total=n())

ggplot(month_hour, aes(hour, total, fill=month)) +
  geom_bar(stat="identity") +
  labs(title="Divvy Rides Every Hour", subtitle="Grouped by Month",
       caption="Data Source: Divvy 2014", x="Hour", y="Total Rides") +
  scale_y_continuous(labels=comma) +
  scale_fill_brewer(palette="Blues", name="",
                    labels=c("January", "February",
                             "March", "April",
                             "May", "June"))

# Divvy Rides by Day of Year

rides_day <- df %>%
  group_by(dayofyear) %>%
  summarize(total=n())

ggplot(rides_day, aes(dayofyear, total)) +
  geom_line(color="darkblue") +
  labs(title="Divvy Rides per Day", caption="Data Source: Divvy 2014",
       x="Day", y="Rides per Day") +
  scale_x_continuous(breaks=c(0, 32, 60, 91, 121, 152),
                     labels=c("January 1", "February 1", "March 1",
                              "April 1", "May 1", "June 1")) +
  scale_y_continuous(labels=comma)

#Ride Duration-------------------------------------------

# Ride Duration by Day of Week

trip_dur_dayofweek <- df %>%
  group_by(trip_dur_group, dayofweek) %>%
  summarize(total=n())

ggplot(trip_dur_dayofweek, aes(dayofweek, total, color=trip_dur_group, group=trip_dur_group)) +
  geom_line() +
  labs(title="Ride Duration by Day of Week", caption="Data Source: Divvy 2014",
       x="", y="Total Rides", color="Ride Duration (seconds)") +
  scale_x_discrete(labels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday")) +
  scale_y_continuous(labels=comma) +
  theme(axis.ticks=element_blank())

nrow(filter(df, trip_duration<=750, user_type=="Subscriber",
            ((hour>=7 & hour<=9) | (hour>=16 & hour<=18))))/nrow(filter(df, trip_duration<=750)) * 100

# Median Ride Duration by Month

median_ride <- function(m)
{
  x <- filter(df, month==m)
  data.frame(m, median(x$trip_duration))
}

median_trip_dur_month <- do.call(rbind.data.frame, lapply(unique(df$month),
                                                          median_ride))

median_trip_dur_month <- rename(median_trip_dur_month, median_trip_dur="median.x.trip_duration.")

ggplot(median_trip_dur_month, aes(m, median_trip_dur, group=1)) +
  geom_line() +
  labs(title="Median Ride Duration by Month", caption="Data Source: Divvy 2014",
       x="", y="Median Ride Duration (seconds)") +
  scale_x_discrete(labels=c("January", "February", "March", "April",
                            "May", "June")) +
  scale_y_continuous(labels=comma) +
  geom_hline(yintercept=median(df$trip_duration), linetype="dashed", color="red", size=1.5) +
  geom_label(aes(x=2, y=median(df$trip_duration), label="Overall Median: 756"), fill="white") +
  theme(axis.ticks=element_blank())

# Trip Duration by Gender Boxplot

filter(df, trip_duration<=quantile(trip_duration, 0.99), !(gender=="")) %>%
  ggplot(aes(gender, trip_duration, color=gender)) + geom_boxplot() +
  labs(title="Ride Duration by Gender", caption="Data Source: Divvy 2014",
       x="", y="Ride Duration (seconds)") +
  theme(axis.ticks=element_blank(), legend.position="none") +
  scale_y_continuous(labels=comma)

# Ride Duration by Gender Density Plot

filter(df, trip_duration<=quantile(trip_duration, 0.99), !(gender=="")) %>%
  ggplot(aes(trip_duration, fill=gender)) +
  geom_density(alpha=0.2) +
  labs(title="Density Plot of Ride Duration by Gender", caption="Data Source: Divvy 2014",
       x="Ride Duration (seconds)", y="Density") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels=comma)

# Trip Duration by User Type Boxplot

filter(df, trip_duration<=quantile(trip_duration, 0.99)) %>%
  ggplot(aes(user_type, trip_duration, color=user_type)) + geom_boxplot() +
  labs(title="Ride Duration by User Type", caption="Data Source: Divvy 2014",
       x="", y="Ride Duration (seconds)") +
  theme(axis.ticks=element_blank(), legend.position="none") +
  scale_y_continuous(labels=comma)

# Ride Duration by User Type Density Plot

filter(df, trip_duration<=quantile(trip_duration, 0.99)) %>%
  ggplot(aes(trip_duration, fill=user_type)) +
  geom_density(alpha=0.2) +
  labs(title="Density Plot of Ride Duration by User Type", caption="Data Source: Divvy 2014",
       x="Ride Duration (seconds)", y="Density") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels=comma)

#Heatmaps--------------------------------------------------------

# Heatmap by hour and day of week

wday_and_hour <- df %>%
  group_by(dayofweek, hour) %>%
  summarize(total=n())

ggplot(wday_and_hour, aes(dayofweek, hour, fill=total)) +
  geom_tile(color="black") +
  labs(title="Heatmap by Hour and Day of Week",
       x="", y="Hour", fill="Total Rides",
       caption="Data Source: Divvy 2014") +
  scale_x_discrete(labels=c("Sunday", "Monday",
                            "Tuesday", "Wednesday",
                            "Thursday", "Friday",
                            "Saturday")) +
  theme(axis.ticks=element_blank())

# Heatmap by month and dayofweek

wday_and_month <- df %>%
  group_by(dayofweek, month) %>%
  summarize(total=n())

ggplot(wday_and_month, aes(dayofweek, month, fill=total)) +
  geom_tile(color="black") +
  labs(title="Heatmap by Month and Day of Week", x="", y="",
       fill="Total Rides", caption="Data Source: Divvy 2014") +
  scale_x_discrete(labels=c("Sunday", "Monday",
                            "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday")) +
  scale_y_discrete(labels=c("January", "February", "March",
                            "April", "May", "June")) +
  theme(axis.ticks=element_blank())

#Distance------------------------------------------

#  Distance Function 1:
dist_pure <- function(flat, flon, tlat, tlon)
{
  rad <- pi/180
  a1 <- flat * rad
  a2 <- flon * rad
  b1 <- tlat * rad
  b2 <- tlon * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  R <- 6378.145
  d <- R * c
  return(d)
}

combos <- function(f_loc)
{
  data.frame(f_loc, unique(df[c("to_location")]))
}

all_combos <- do.call(rbind.data.frame, lapply(unique(df$from_location),
                                               combos))

numextract <- function(string)
{
  data.frame(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))[1],
             unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))[2])
}

from_locs <- do.call(rbind.data.frame, lapply(all_combos$f_loc,
                                              numextract))
from_locs <- rename(from_locs,
                    f_lon=unlist.regmatches.string..gregexpr.....digit...........digit.......,
                    f_lat=unlist.regmatches.string..gregexpr.....digit...........digit........1)

to_locs <- do.call(rbind.data.frame, lapply(all_combos$to_location,
                                            numextract))
to_locs <- rename(to_locs,
                  t_lon=unlist.regmatches.string..gregexpr.....digit...........digit.......,
                  t_lat=unlist.regmatches.string..gregexpr.....digit...........digit........1)

all_combos <- cbind(from_locs, to_locs)
all_combos <- all_combos[, c(2, 1, 4, 3)]

fac_to_num <- function(factor)
{
  as.numeric(as.character(factor))
}

all_combos[] <- lapply(all_combos, fac_to_num)

all_combos$f_lon <- all_combos$f_lon*-1
all_combos$t_lon <- all_combos$t_lon*-1

all_distances <- mapply(dist_pure, all_combos$f_lat, all_combos$f_lon, all_combos$t_lat, all_combos$t_lon)

all_combos <- cbind(all_combos, all_distances) %>% filter(!(all_distances==0)) %>% rename(distance=all_distances)

furthest_combos <- all_combos[order(all_combos$distance, decreasing = TRUE),] %>% distinct(distance, .keep_all = TRUE)

head(furthest_combos)

tail(furthest_combos)

# Distance Function 2:
dist <- function(x) {
  rad <- pi/180
  a1 <- x$from_latitude * rad
  a2 <- x$from_longitude * rad
  b1 <- x$to_latitude * rad
  b2 <- x$to_longitude * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  R <- 6378.145
  d <- R * c
  return(d)
}

distances <- dist(df)

max(distances)

which.max(distances)

df[which.max(distances),] %>% select(from_station_name, to_station_name)

mean(distances)

#Speed--------------------------------------------------------------------

#  Speed Function:
speed <- function(d, t)
{
  s <- d/t
  s * 3600
}

# Average speed of a ride: ~ 8.675 km/hour = ~5.4 miles/hour
speeds <- speed(distances, df$trip_duration)
mean(speeds)

# The max speed of a ride was ~62.267 km/hour !!!
# But its only because the two stations were right across from each other
# and the trip was very short and quick.
max(speeds)
df[which.max(speeds),] %>% select(from_station_name, to_station_name)

# Distribution of speeds
dfs <- df
dfs$speed <- speed(distances, dfs$trip_duration)

filter(dfs, trip_duration<=quantile(trip_duration, 0.99)) %>%
  ggplot(aes(speed, trip_duration)) +
  geom_point()

#Ages---------------------------------------------------------------------

# Scatterplot of age vs trip duration

age_ride_time_plot <- df %>% drop_na(age_group) %>%
  filter(!(gender==""), trip_duration<=quantile(trip_duration, 0.995)) %>%
  ggplot(aes(age_group, trip_duration)) +
  geom_jitter(width=0.3) +
  labs(title="Ride Time by Age Group",
       x="Age Group", y="Trip Duration (seconds)",
       caption="Data Source: Divvy 2014") +
  scale_y_continuous(labels=comma) +
  theme(axis.ticks=element_blank(), legend.title=element_blank())

age_ride_time_plot

age_ride_time_gender_plot <- df %>% drop_na(age_group) %>%
  filter(!(gender==""), trip_duration<=quantile(trip_duration, 0.995)) %>%
  ggplot(aes(age_group, trip_duration, color=gender)) +
  geom_jitter(width=0.3) +
  labs(title="Ride Time by Age Group", subtitle="Segmented by Gender",
       x="Age Group", y="Trip Duration (seconds)",
       caption="Data Source: Divvy 2014") +
  scale_y_continuous(labels=comma) +
  theme(axis.ticks=element_blank(), legend.title=element_blank()) +
  geom_hline(yintercept=mean(df$trip_duration), color="red") +
  geom_label(aes(x=85, y=mean(df$trip_duration),
                 label="Mean: 1069"), fill="white")

age_ride_time_gender_plot

# Connected scatterplot of age vs mean trip duration

calc_median_ride_time <- function(a) {
  x <- filter(df, age==a)
  data.frame(a, nrow(x), round(median(x$trip_duration),digits=2))
}

median_ride_times <- do.call(rbind.data.frame,
                           lapply(unique(df$age),
                                  calc_median_ride_time))

median_ride_times <- rename(median_ride_times,
                          ride_time=round.median.x.trip_duration...digits...2.,
                          pop_size=nrow.x.) %>%
  filter(!is.nan(ride_time)) %>%
  filter(a<=90)

median_ride_times <- median_ride_times[order(median_ride_times$a, decreasing=FALSE),]

#  **REALLY COOL Mean trip Duration by Age**

ggplot(median_ride_times, aes(a, ride_time)) +
  geom_point(aes(size=pop_size)) +
  labs(title="Median Ride Duration by Age", subtitle="Circle size corresponding to number of riders",
       caption="Data Source: Divvy 2014", y="Median Ride Duration (seconds)", x="Age") +
  geom_hline(yintercept = median(filter(df, !is.na(age), age<=90)$trip_duration), linetype="dashed",
             color="red", size=1.5) +
  geom_label(aes(x=85, y=median(filter(df, !is.na(age), age<=90)$trip_duration),
                 label="Median: 578"), fill="white") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80, 90))

#Map Visualizations--------------------------------------------------------

# Map 1: Circles

dloc <- data.frame(unique(df[c("from_station_name", "from_latitude",
            "from_longitude")]))

station_id <- do.call(rbind.data.frame,
                      lapply(unique(df$from_station_id), num_rides_total))

dloc <- cbind(dloc, station_id) %>% rename(total_rides=n)

dloc <- dloc %>% mutate(total_rides=total_rides/100)

map <- leaflet(dloc) %>% setView(lng = -87.65, lat = 41.85, zoom=11)
map <- map %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(~from_longitude, ~from_latitude,
             color="steelblue", radius = ~total_rides)
map

# Map 2: Lines

#  For Subscribers:
station_to_station_s <- df %>%
  filter(user_type=="Subscriber") %>%
  group_by(from_station_name, to_station_name) %>%
  summarize(total=n())

station_to_station_s <- station_to_station_s[order(station_to_station_s$total, decreasing=TRUE),]

head(station_to_station_s)

#  For Customers:
station_to_station_c <- df %>%
  filter(user_type=="Customer") %>%
  group_by(from_station_name, to_station_name) %>%
  summarize(total=n())

station_to_station_c <- station_to_station_c[order(station_to_station_c$total, decreasing=TRUE),]

head(station_to_station_c)

#  Mapping Process:

s1 <- dloc[(dloc$from_station_name=="LaSalle St & Jackson Blvd" |
              dloc$from_station_name=="Canal St & Madison St"),]
s2 <- dloc[(dloc$from_station_name=="Dearborn St & Monroe St" |
              dloc$from_station_name=="Canal St & Madison St"),]
s3 <- dloc[(dloc$from_station_name=="Columbus Dr & Randolph St" |
              dloc$from_station_name=="Clinton St & Washington Blvd"),]
s4 <- dloc[(dloc$from_station_name=="Columbus Dr & Randolph St" |
              dloc$from_station_name=="Canal St & Madison St"),]
s5 <- dloc[(dloc$from_station_name=="Michigan Ave & Lake St" |
              dloc$from_station_name=="Clinton St & Washington Blvd"),]

c1 <- dloc[(dloc$from_station_name=="Lake Shore Dr & Monroe St" |
              dloc$from_station_name=="Streeter Dr & Illinois St"),]
c2 <- dloc[(dloc$from_station_name=="Theater on the Lake" |
              dloc$from_station_name=="Streeter Dr & Illinois St"),]
c3 <- dloc[(dloc$from_station_name=="Streeter Dr & Illinois St" |
              dloc$from_station_name=="Millennium Park"),]
c4 <- dloc[(dloc$from_station_name=="Lake Shore Dr & Monroe St" |
              dloc$from_station_name=="Museum Campus"),]
c5 <- dloc[(dloc$from_station_name=="Michigan Ave & Oak St" |
              dloc$from_station_name=="Theater on the Lake"),]

map2 <- leaflet() %>% setView(lng = -87.62, lat = 41.89, zoom=13)
map2 <- map2 %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data=s1, lat=~from_latitude, lng=~from_longitude, color="#00BFC4", group=) %>%
  addPolylines(data=s2, lat=~from_latitude, lng=~from_longitude, color="#00BFC4", group=) %>%
  addPolylines(data=s3, lat=~from_latitude, lng=~from_longitude, color="#00BFC4", group=) %>%
  addPolylines(data=s4, lat=~from_latitude, lng=~from_longitude, color="#00BFC4", group=) %>%
  addPolylines(data=s5, lat=~from_latitude, lng=~from_longitude, color="#00BFC4", group=) %>%
  addPolylines(data=c1, lat=~from_latitude, lng=~from_longitude, color="#F8766D", group=) %>%
  addPolylines(data=c2, lat=~from_latitude, lng=~from_longitude, color="#F8766D", group=) %>%
  addPolylines(data=c3, lat=~from_latitude, lng=~from_longitude, color="#F8766D", group=) %>%
  addPolylines(data=c4, lat=~from_latitude, lng=~from_longitude, color="#F8766D", group=) %>%
  addPolylines(data=c5, lat=~from_latitude, lng=~from_longitude, color="#F8766D", group=) %>%
  addLegend("topright", title="User Type", colors=c("#00BFC4", "#F8766D"),
            labels=c("Subscriber", "Customer"))
map2

#Misc---------------------------------------------------

# % of Return Rides
nrow(filter(df, from_station_id==to_station_id))
nrow(filter(df, from_station_id==to_station_id))/nrow(df)*100

# The median return ride lasts 1286 seconds, which is 540 seconds longer
# than the average non-return ride.

return_trip_median <- median(filter(df, to_station_id==from_station_id)$trip_duration)
return_trip_median

non_return_trip_median <- median(filter(df, !(to_station_id==from_station_id))$trip_duration)
non_return_trip_median

return_trip_median - non_return_trip_median

# The most popular day of the year for riding:
# June 14 had 16,562 Divvy rides

rides_by_dayofyear <- df %>%
  group_by(dayofyear) %>%
  dplyr::summarize(total=n())

rides_by_dayofyear[which.max(rides_by_dayofyear$total),]

# The most popular day of the year and hour for riding:
# May 25 @ 2P.M. had 1630 Divvy rides

rides_by_dayofyearhour <- df %>%
  group_by(month, day, hour) %>%
  dplyr::summarize(total=n())

rides_by_dayofyearhour[which.max(rides_by_dayofyearhour$total),]

# Number of unique bikes

nrow(distinct(df, bike_id))

# Most popular bikes to use

bike_use <- function(b)
{
  x <- filter(df, bike_id==b)
  data.frame(b, count(x))
}

bikes <- do.call(rbind.data.frame, lapply(unique(df$bike_id),
                                          bike_use))

pop_bikes <- bikes[order(bikes$n, decreasing=TRUE),]

max(pop_bikes$n)
min(pop_bikes$n)

#Fin.--------------------------------------------------------