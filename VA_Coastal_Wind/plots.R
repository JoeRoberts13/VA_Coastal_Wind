
# All the plotting functions, call the calculation script to get global variables

library(httr)
library(suncalc)
library(lubridate)
library("tidyverse")

# Source the data creation script
source("VA_Coastal_Wind/main.R")

#### Exploratory Data Analysis ####

# Some basic plots 
plot(coastal_data_df$date_time, coastal_data_df$temperature_2m)
plot(coastal_data_df$date_time, coastal_data_df$relative_humidity_2m)
plot(coastal_data_df$date_time, coastal_data_df$pressure_msl)
plot(coastal_data_df$date_time, coastal_data_df$wind_speed_100m)

###### Tiled weather plot #####
## Make some tiled scatterplots 
selected_y_vars <- c("temperature_2m", "air_density", "wind_speed_100m","power_kW")

scatter_min_date <- as.POSIXct("01-01-20", format = "%m-%d-%y", tz = timezone)
scatter_max_date <- as.POSIXct("01-01-21", format = "%m-%d-%y", tz = timezone)

selected_x_dates <- (coastal_data_df$date_time >= scatter_min_date & 
                       coastal_data_df$date_time < scatter_max_date)

data_selected <- coastal_data_df[selected_x_dates, c("date_time", selected_y_vars)]
data_long <- tidyr::gather(data_selected, key = "variable", value = "value", -date_time)

data_long$variable <- factor(data_long$variable, 
                       levels = c("temperature_2m", "air_density", "wind_speed_100m","power_kW"),
                       labels = c("Temperature (C)", "Air Density (kg/m^3)", "Wind Speed (m/s)", "Power (kW)"))


p_scatterTile <- ggplot(data_long, aes(x = date_time, y = value)) +
  geom_point(color = "#126180", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Date", y = element_blank()) +
  theme_gray()

print(p_scatterTile)
ggsave("images/tile_weather.png", 
       plot = p_scatterTile, 
       width = 8, height = 6)

# Hist of windspeed, it should look like a Weibull distribution,
# with a longer right tail and no negative values
hist(coastal_data_df$wind_speed_100m)

#### Wind Speed Temporal Analysis ####

###### Boxplots #####
# Create some boxplots with all the weather observations
# Don't include pressure because it's much larger (000's)
# Reshaping the dataframe to long format

coastal_data_long <- pivot_longer(coastal_data_df,
                                  cols = c("temperature_2m", 
                                           "wind_speed_100m",
                                           "relative_humidity_2m"),
                                  names_to = "variable",
                                  values_to = "value")

# Creating a boxplot of RH, Temp, WS
ggplot(coastal_data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Relative Humidity, Temperature, and Wind Speed", 
       x = "Variable", 
       y = "Value") +
  theme(legend.title = element_blank()) # Remove the legend title 

# Plot wind speed vs. time-of-day w/ boxplots
ggplot(coastal_data_df, aes(x = time_of_day, y = wind_speed_100m)) +
  geom_boxplot() +
  labs(title = "Wind Speed vs. Time of Day",
       x = "Time of Day",
       y = "Wind Speed (100m)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability
# looks like wind speed tends to lower during the middle of the day

# Day vs. night wind behavior
ggplot(coastal_data_df, aes(x =wind_speed_100m, y = day_night)) +
  facet_wrap(~season) +
  geom_boxplot()

###### Histograms #####
####### Plot wind speeds vs. time-of-day by season #######
wind_vs_time_by_season <- ggplot(coastal_data_df, aes(x = time_of_day, y = wind_speed_100m)) +
  geom_boxplot() +  
  facet_wrap(~season, scales = "free_x") +  # Create a panel for each season
  labs(title = "Wind Speed vs. Time of Day by Season",
       x = "Time of Day",
       y = "Wind Speed (@ 100m elevation)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "italic", size = 12))

print(wind_vs_time_by_season)
ggsave("images/wind_vs_time_by_season.png", 
       plot = wind_vs_time_by_season, 
       width = 8, height = 6)

# Summary: Wind seems to be generally higher, and a little bit more variable in 
# the Winter and Spring.  Also, wind seems to be slightly higher at night.

###### Wind Scatterplot #####
# Just a regular scatterplot of wind over time
min_date = as.POSIXct("01-01-20", format = "%m-%d-%y", tz = timezone)
max_date = as.POSIXct("01-01-21", format = "%m-%d-%y", tz = timezone)
selected_range <- (coastal_data_df$date_time >= min_date & coastal_data_df$date_time < max_date)

ggplot(coastal_data_df[selected_range,], aes(x = date_time, y = wind_speed_100m)) +
  geom_point()

###### Variance by date, week, month #####
selected_range <- (variance_by_date$date >= min_date & variance_by_date$date < max_date)
ggplot(variance_by_date[selected_range,], aes(x = date, y = var_wind_speed))+
  geom_line() +
  labs(title = "Variance of wind speed by date",
       x = "Date",
       y = "Variance of Wind Speed (m/s)") +
  theme_minimal()

selected_range <- (variance_by_week$week >= min_date & variance_by_week$week < max_date)
ggplot(variance_by_week[selected_range,], aes(x = week, y = var_wind_speed))+
  geom_line() +
  labs(title = "Variance of wind speed by week",
       x = "Week",
       y = "Variance of Wind Speed (m/s)") +
  theme_minimal()

selected_range <- (variance_by_month$month >= min_date & variance_by_month$month < max_date)
ggplot(variance_by_month, aes(x = month, y = var_wind_speed))+
  geom_line() +
  geom_smooth() +
  labs(title = "Variance of wind speed by month",
       x = "Month",
       y = "Variance of Wind Speed (m/s)") +
  theme_minimal()

###### Wind Histogram #####
# Create wind bins to aggregate the hours and power of wind data
bin_width <- .5
coastal_data_df$wind_bins <- cut(coastal_data_df$wind_speed_100m, 
                                 breaks = seq(0, max(coastal_data_df$wind_speed_100m) + bin_width, 
                                              bin_width), 
                                 include.lowest = TRUE)

## Convert factor to corrresponding numeric vector of midpoints
bin_edges <- seq(0, max(coastal_data_df$wind_speed_100m) + bin_width, bin_width)
bin_midpoints <- (bin_edges[-length(bin_edges)] + bin_edges[-1]) / 2

# Convert wind_bins to numeric indices
coastal_data_df$bin_index <- as.numeric(coastal_data_df$wind_bins)

# Map indices to bin midpoints
coastal_data_df$wind_bin_midpoint <- bin_midpoints[coastal_data_df$bin_index]

wind_hist <- ggplot(coastal_data_df, aes(x = wind_bin_midpoint, y = time_differences)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(x = "Wind Speed (m/s)", 
       y = "Total Hours Observed", 
       title = "Bar Plot: Wind Speed vs. Total Hours Observed") +
  theme_minimal()

print(wind_hist)

#### Turbine Power Curve #####

###### Flat Curve #####
# We'll plot the turbine power curve to see how it looks
# This one is flat, because air density isn't factored in above rated speed
ggplot(data = coastal_data_df, aes(x = wind_speed_100m, y = power_kW_flat)) +
  geom_point() + 
  labs(title = "Turbine Power Curve",
       x = "Wind Speed (m/s)",
       y = "Power (kW)") +
  theme_minimal()

# We'll try to visualize and capture the distribution of air density to add
# fuzziness to the rated power
# hist(coastal_data_df$air_density) # Not all that normal
# plot(coastal_data_df$wind_speed_100m, coastal_data_df$air_density)

# see if there's a relationship between pressure and wind_speed
# lm <- lm(air_density ~ wind_speed_100m, data= coastal_data_df)
# summary(lm)
# Can't really say much of value about that, not that'll hold up
# I'll say, it appears the variance is different at higher wind speeds, but
# The statistic relationship can't be modeled linearly using only wind speed


# Plot that relationship
# ggplot(coastal_data_df, aes(x = wind_speed_100m, y = air_density)) +
#   geom_point() +  # Plot the original data points
#   geom_smooth(method = "lm", formula = y ~ x, 
#               color = "red", se = FALSE) +
#   labs(title = "Air Density vs. Wind Speed ",
#        x = "Wind Speed (m/s)",
#        y = "Air Density (kg / m^3)") +
#   theme_minimal()

###### Turbine Power Curve ####
# We'll plot the turbine power curve to see how it looks
turbine_power_curve <- ggplot(data = coastal_data_df, aes(x = wind_speed_100m, y = power_kW)) +
  geom_point() + 
  geom_hline(yintercept = rated_power, linetype = "dashed", color = "red") +
  geom_text(aes(x = 0, y = rated_power, 
                label = paste("Rated Power:",rated_power,"kW")), 
            hjust = 0, vjust = -.5, color = "red") +
  labs(title = "Turbine Power Curve",
       x = "Wind Speed (m/s)",
       y = "Power (kW)") +
  theme(plot.title = element_text(face = "italic"))

print(turbine_power_curve)
ggsave("images/turbine_power_curve.png", plot=turbine_power_curve, 
       width = 8, height = 6)

# See what the average power output greater than rated speed is
print(paste("Average power when > rated speed:", 
            round(mean(coastal_data_df$power_kW[coastal_data_df$wind_speed_100m>rated_speed]),0),
            "kW"))

#### Energy Plots ####
# Energy produced vs. Time-of-day
plot(coastal_data_df$time_of_day, coastal_data_df$power_kW, pch = 16,
     xlab = "Time of Day", ylab = "Power (kW)",
     main = "Scatterplot: Power vs. Time of Day")

# Energy produced vs. Time-of-day by season 
ggplot(coastal_data_df, aes(x = time_of_day, y = power_kW)) +
  geom_boxplot() +  
  facet_wrap(~season, scales = "free_x") +  # Create a panel for each season
  labs(title = "Power Produced vs. Time of Day by Season",
       x = "Time of Day",
       y = "Power (kW)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Improve readability of x-axis labels
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "italic", size = 16))


###### Combined Weighted Average Power Plot #####

# Calculate total hours observed and total energy produced for each wind speed
summary_data <- aggregate(cbind(time_differences, energy_kWh) ~ wind_bin_midpoint, 
                          data = coastal_data_df, sum)


# Create a bar plot of total hours observed
hours_plot <- ggplot(summary_data, aes(x = wind_bin_midpoint, y = time_differences)) +
  geom_col(fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Wind Speed (m/s)", 
       y = "Total Hours Observed", 
       title = "Bar Plot: Wind Speed vs. Total Hours Observed") +
  theme_minimal()

print(hours_plot)

# Create a line plot of total energy produced
scale_factor = 0.8 * max(summary_data$energy_kWh) / max(summary_data$time_differences)
convert_MWh = 1000
energy_plot <- ggplot(summary_data, aes(x = wind_bin_midpoint, y = energy_kWh/scale_factor)) +
  geom_col(fill = "maroon", color = "black", alpha = 0.7) +
  labs(x = "Wind Speed Bins", 
       y = "Total Energy Produced", 
       title = "Bar Plot: Wind Speed vs. Total Energy Produced") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale_factor/convert_MWh, 
                                         name = "Energy (MWh) (scaled)"))+
  theme()
print(energy_plot)

# Plot both on the same axis
annotate_x <- max(coastal_data_df$wind_speed_100m) 
annotate_y <- .05 * sum(coastal_data_df$time_differences)
date_range_pretty <- paste(format(min(coastal_data_df$date_time),"%b %Y"), "-",
                           format(max(coastal_data_df$date_time),"%b %Y"))

combined_plot <- hours_plot +
  geom_col(data = summary_data, 
           aes(x = wind_bin_midpoint, y = energy_kWh/scale_factor),
           fill = "maroon",
           alpha = 0.7) +
  labs(title = paste0("VA Coastal Wind (Single Turbine): Total Hours of Wind Observed and Total Energy Produced",
                      " (", date_range_pretty, ")")) +
  annotate("text", 
           x = annotate_x, y = annotate_y, 
           label = paste("Total Power Produced:",format(round(total_MWh,0), big.mark = ","),"MWh"), 
           hjust = 1, vjust = 0, 
           color = "black") +
  annotate("text", 
           x = annotate_x, y = annotate_y, 
           label = paste("Avg. Annual Power Produced:",format(round(annual_MWh,0), big.mark = ","),"MWh"), 
           hjust = 1, vjust = 1.5, 
           color = "black") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale_factor/convert_MWh, name = "Energy (MWh) (scaled)",
                                         breaks = seq(0, max(summary_data$energy_kWh) * scale_factor / convert_MWh,
                                                      by = round(max(summary_data$energy_kWh/(7*convert_MWh)), -3) )),
                     breaks = seq(0, max(summary_data$time_differences)*1.25, 
                                  by = round(max(summary_data$time_differences/5), -2) )) +
  scale_x_continuous(breaks = seq(0, max(summary_data$wind_bin_midpoint)*1.25, by = 2)) + 
  theme_gray()

# Print the combined plot
print(combined_plot)
ggsave("images/weighted_average_power_and_wind.png", plot = combined_plot, 
       width = 12, height = 8)



