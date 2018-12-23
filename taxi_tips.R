library(dplyr)
library(ggplot2)
library(lubridate)

# Read in raw dataset
taxi_df <- read.csv("./data/yellow_tripdata_2018-06.csv")

# For computational simplicity, take a random sample without replacement of 100,000 trips from the above datasets
taxi_df <- sample_n(taxi_df, 100000)

# Tips vs. Fares
g1 <- ggplot(taxi_df, aes(x = fare_amount, y = tip_amount)) +
geom_point(alpha = 0.1, color = 'blue', size = 1) +
xlab("Fare") + ylab("Tip") +
ggtitle('Tip vs. Fare') +
theme(plot.title = element_text(hjust = 0.5, size = 18),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16)) +
scale_x_continuous(limits = c(0, 100)) +
scale_y_continuous(limits = c(0, 20))

# Tips vs. Distance
g2 <- ggplot(taxi_df, aes(x = trip_distance, y = tip_amount)) +
geom_point(alpha = 0.1, color = 'darkgreen', size = 1) +
xlab("Distance") + ylab("Tip") +
ggtitle('Tip vs. Distance') +
theme(plot.title = element_text(hjust = 0.5, size = 18),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16)) +
scale_x_continuous(limits = c(0, 25)) +
scale_y_continuous(limits = c(0, 20))

gridExtra::grid.arrange(g1, g2, nrow = 1)

# Get total travel times
taxi_df$tpep_dropoff_datetime = as.POSIXct(taxi_df$tpep_dropoff_datetime,
format='%m/%d/%Y %H:%M')
taxi_df$tpep_pickup_datetime = as.POSIXct(taxi_df$tpep_pickup_datetime,
format='%m/%d/%Y %H:%M')
taxi_df$total_time = taxi_df$tpep_dropoff_datetime - taxi_df$tpep_pickup_datetime
taxi_df$total_time = as.double(taxi_df$total_time)

# Convert total_time to minutes and create speed variable (= trip_distance / total_time)
taxi_df1 <- taxi_df %>%
filter(total_time < 10000) %>%
mutate(total_time = total_time / 3600) %>%
mutate(speed = trip_distance / total_time) %>%
filter(speed > 0.5) %>%
filter(tip_amount > 0)

# Tips vs. Speed
ggplot(taxi_df1, aes(x = speed, y = tip_amount)) +
geom_point(alpha = 0.1, color = 'purple', size = 1) +
xlab("Speed (Miles per Hour)") + ylab("Tip") +
ggtitle('June 2018 Yellow Taxi Ride Tips vs. Speed') +
theme(plot.title = element_text(hjust = 0.5, size = 18),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16)) +
scale_x_continuous(limits = c(0, 50)) +
scale_y_continuous(limits = c(0, 20)) +
geom_smooth(method = "lm", color = "black")

taxi_df2 <- taxi_df %>%
filter(passenger_count > 0) %>%
mutate(tip_percentage = 100 * (tip_amount / fare_amount)) %>%
filter(tip_percentage < 30) %>%
filter(tip_percentage > 0)

ggplot(taxi_df2, aes(x = factor(passenger_count), y = tip_percentage)) +
geom_boxplot(color = "darkgreen", fill = "green") +
xlab("Number of Passengers") + ylab("Tip Percentage") +
ggtitle("Tip Percentage vs. Number of Passengers") +
theme(plot.title = element_text(hjust = 0.5, size = 18),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16))

