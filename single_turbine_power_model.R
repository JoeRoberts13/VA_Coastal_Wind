library(httr)
library(lubridate)
library("tidyverse")


##### API Calls ####
# Fetch Hourly weather data using open-meteo.com
# We'll model the Dominion Energy Coastal Virginia Offshore Wind Project
# Located 27 miles offshore, east of Virginia Beach.
# The coordinates are approximately: 36.8317, -75.4680
# It's unclear how open-meteo.com gets their historical data.  They say
# it's aggregated from reputable national sensors, but I think some of it is
# modeled and interpolated.  We'll see how it looks.

# Parameters for the request
latitude <- 36.8317
longitude <- -75.4680
start_date <- "2022-01-01"
end_date <- "2022-12-31"
hourly <- "temperature_2m,relative_humidity_2m,pressure_msl,wind_speed_100m"
wind_speed_unit <- "ms"
timezone <- "EST"
url = "https://archive-api.open-meteo.com/v1/archive"

## API Call to get the weather data
response <-  GET(url,
                 query = list(latitude = latitude,
                              longitude = longitude,
                              start_date = start_date,
                              end_date = end_date,
                              hourly = hourly,
                              wind_speed_unit = wind_speed_unit,
                              timezone = timezone),
)
if(status_code(response) == 200) print("Success!") else {
  print(paste("Failure. Code:", status_code(response))) }

coastal_data_list<- content(response, "parsed")

# Check the units of the data
print(coastal_data_list$hourly_units)
print(coastal_data_list$timezone)

# Length of the df
obs_length <- length(coastal_data_list$hourly$time)

# Repeat the variables that won't change by row
data_lat <- rep(coastal_data_list$latitude, obs_length)
data_lon <- rep(coastal_data_list$longitude, obs_length)
data_timezone <- rep(coastal_data_list$timezone_abbreviation, obs_length)
data_temp_units <- rep(coastal_data_list$hourly_units$temperature_2m, obs_length)
data_pressure_units <- rep(coastal_data_list$hourly_units$pressure_msl, obs_length)
data_wind_units <- rep(coastal_data_list$hourly_units$wind_speed_100m, obs_length)

# Format the time correctly in POSIX
time_vector <- unlist(coastal_data_list$hourly$time)
data_time <- as.POSIXct(time_vector, format="%Y-%m-%dT%H:%M",
                        tz = coastal_data_list$timezone_abbreviation)

# Put it all into a dataframe
coastal_data_df <- data.frame(
  lat = data_lat,
  lon = data_lon,
  timezone = data_timezone,
  time = data_time,
  temperature_2m = unlist(coastal_data_list$hourly$temperature_2m),
  relative_humidity_2m = unlist(coastal_data_list$hourly$relative_humidity_2m),
  pressure_msl = unlist(coastal_data_list$hourly$pressure_msl),
  wind_speed_100m = unlist(coastal_data_list$hourly$wind_speed_100m),
  temp_units = data_temp_units,
  pressure_units = data_pressure_units,
  wind_units = data_wind_units
  
)
view(coastal_data_df)
str(coastal_data_df)

#### Exploratory Data Analysis ####

# Add a time-of-day and season variable to see how things change
coastal_data_df$time_of_day <- format(coastal_data_df$time, "%H:%M:%S")
coastal_data_df$time_of_day <- factor(coastal_data_df$time_of_day, 
                                      levels = unique(coastal_data_df$time_of_day),
                                      ordered = TRUE)

spring = c(3,4,5)
summer = c(6,7,8)
fall = c(9,10,11)

coastal_data_df$season <- sapply(month(coastal_data_df$time),function(x){
  if (x %in% spring) return("Spring")
  else if (x %in% summer) return("Summer")
  else if (x %in% fall) return("Fall")
  else return("Winter")
})
coastal_data_df$season <- factor(coastal_data_df$season, levels = c("Winter",
                                                                    "Spring",
                                                                    "Summer",
                                                                    "Fall"))

# Add observation length and check for completeness
coastal_data_df$time_differences = c(NA, diff(coastal_data_df$time))
if(var(coastal_data_df$time_differences, na.rm = TRUE) == 0 && 
   sum(is.na(coastal_data_df$time_differences) == 1)){
  coastal_data_df$time_differences[1] = 1
} else print("Observations aren't all the same, or are missing data.")

# Do some generic data validation and scatterplots vs. time
print(paste0("Complete cases: ",
             sum(complete.cases(coastal_data_df))*100/nrow(coastal_data_df),
             "%"))
plot(coastal_data_df$time, coastal_data_df$temperature_2m)
plot(coastal_data_df$time, coastal_data_df$relative_humidity_2m)
plot(coastal_data_df$time, coastal_data_df$pressure_msl)
plot(coastal_data_df$time, coastal_data_df$wind_speed_100m)

# Hist of windspeed, it should look like a Weibull distribution,
# with a longer right tail and no negative values
hist(coastal_data_df$wind_speed_100m)

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

# Plot wind speeds vs. time-of-day by season
wind_vs_time_by_season <- ggplot(coastal_data_df, aes(x = time_of_day, y = wind_speed_100m)) +
  geom_boxplot() +  
  facet_wrap(~season, scales = "free_x") +  # Create a panel for each season
  labs(title = "Wind Speed vs. Time of Day by Season",
       x = "Time of Day",
       y = "Wind Speed (@ 100m elevation)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "italic", size = 12))

ggsave("wind_vs_time_by_season.png", 
       plot = wind_vs_time_by_season, 
       width = 8, height = 6)

# Summary: Wind seems to be generally higher, and a little bit more variable in 
# the Winter and Spring.  Also, wind seems to be slightly higher at night.
# It also could be less gusty at night because there are less high outliers, 
# but it's tough to tell. I could do it again with like 5 years of data

#####Air Density Calculation ####

air_density_calc <- function(temp_celsius, relative_humidity, p_air_hPa){
  # Input validation
  if (!is.numeric(temp_celsius) || !is.numeric(relative_humidity) || !is.numeric(p_air_hPa)) {
    stop("All inputs must be numeric.")
  }
  
  if (any(relative_humidity < 1 | relative_humidity > 100)) {
    warning("Relative humidity should be between 0 and 100, and shouldn't be decimal percent. Check the units")
  }
  
  if (any(p_air_hPa < 100 | p_air_hPa > 1200)) {
    warning("Your pressures are outside normal ranges on earth in hPa. Check the units")
  }
  
  # Constants
  R_dry = 287.052874 # individual gas constant of dry air (Joules/(kg*K))
  R_vapor = 461.522789 # individual gas constant of vapor (Joules/(kg*K))
  
  # Calculate vapor pressure in the air
  p_saturation = 6.1078 * 10^((7.5 * temp_celsius)/(temp_celsius + 237.3)) # in hPa
  p_vapor_hPa = p_saturation * relative_humidity/100 # in hPa
  p_vapor = p_vapor_hPa *100 # convert to Pascals
  
  # Calculate pressure of dry air
  p_air = p_air_hPa * 100 #convert to Pascals
  p_dry = p_air - p_vapor
  
  # Calculate air density as sum of dry air and vapor density
  temp_K = temp_celsius + 273.15
  rho = (p_dry / (R_dry * temp_K)) + (p_vapor/ (R_vapor * temp_K))
  return(rho)
}

# Calculate air density (in kg / m^3)
coastal_data_df$air_density <- air_density_calc(coastal_data_df$temperature_2m,
                                                coastal_data_df$relative_humidity_2m,
                                                coastal_data_df$pressure_msl)

#### Power Modeling ####


###### Assumptions ####
# The Virginia Coastal Offshore Wind pilot project consisted of 2x 6MW
# Siemens-Gamesa SWT 6.0-154 turbines.

# I'll be modeling the proposed commercial project, which is projected to consist
# of 176x 14.7MW Siemens-Gamesa turbines per the Dominion Energy Website
# https://coastalvawind.com/about-offshore-wind/frequently-asked-questions/general.aspx

# For this model, I will assume they're referring to the SG 14-222 DD, set to 
# begin production in 2024. Source for the turbine data, and manufacturer
# website are below.
# https://www.siemensgamesa.com/products-and-services/offshore/wind-turbine-sg-14-222-dd
# https://en.wind-turbine-models.com/turbines/2266-siemens-gamesa-sg-14-222-dd


# Turbine Stats
radius <- 222/2 # meters

cut_in_speed <- 3 # meters/sec
rated_speed <- 12 # meters/sec
cut_out_speed <- 32 # meters/sec
survival_speed <- 57 # meters/sec

rated_power <- 14000 # kW

# Efficiency Stats
# Betz's Law states that max theoretical turbine efficiency is ~59% to account
# for required leaving losses. But I don't have data available on this turbine


# Make a calculation for combined efficiency to make the power curve work
n_combined <- (rated_power * 1000) /
  (0.5 * median(coastal_data_df$air_density) * pi * radius^2 * rated_speed^3)

# Make some assumptions
n_mechanical <- 0.96
n_electrical <- 0.94
n_blade <- n_combined / (n_mechanical*n_electrical)


###### Power Calculation #####
# Power @ 100% efficiency (kiloWatts)
coastal_data_df$power_available <- 0.5 * coastal_data_df$air_density * pi * 
  radius^2 * coastal_data_df$wind_speed_100m^3 / 1000

# Actual power output based on rated turbine speeds and wind speeds
coastal_data_df <- coastal_data_df %>%
  mutate(power_kW_flat = case_when(
    wind_speed_100m < cut_in_speed ~ 0,
    wind_speed_100m < rated_speed ~ n_combined * power_available,
    wind_speed_100m < cut_out_speed ~ rated_power,
    TRUE ~ 0
  ))


###### Turbine Power Curve (Flat) #####
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
hist(coastal_data_df$air_density) # Not all that normal
plot(coastal_data_df$wind_speed_100m, coastal_data_df$air_density)

# see if there's a relationship between pressure and wind_speed
lm <- lm(air_density ~ wind_speed_100m, data= coastal_data_df)
summary(lm)
# Can't really say much of value about that, not that'll hold up
# I'll say, it appears the variance is different at higher wind speeds, but
# The statistic relationship can't be modeled linearly using only wind speed


# Plot that relationship
ggplot(coastal_data_df, aes(x = wind_speed_100m, y = air_density)) +
  geom_point() +  # Plot the original data points
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "red", se = FALSE) +
  labs(title = "Air Density vs. Wind Speed ",
       x = "Wind Speed (m/s)",
       y = "Air Density (kg / m^3)") +
  theme_minimal()

## I'll just try to use the actual air density for wind > rated speed, but use 
# rated speed instead of wind speed. It'll be a little more realistic, but not perfect
# I think it's a reasonable assumption because the turbine will yaw the blades
# to keep the most efficient tip speed of the rotor.
coastal_data_df <- coastal_data_df %>%
  mutate(power_kW = case_when(
    wind_speed_100m < cut_in_speed ~ 0,
    wind_speed_100m < rated_speed ~ n_combined * power_available,
    wind_speed_100m < cut_out_speed ~ n_combined * power_available / wind_speed_100m^3 * rated_speed^3,
    TRUE ~ 0
  ))

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

ggsave("turbine_power_curve.png", plot=turbine_power_curve, 
       width = 8, height = 6)

# See what the average power output greater than rated speed is
print(paste("Average power when > rated speed:", 
            round(mean(coastal_data_df$power_kW[coastal_data_df$wind_speed_100m>rated_speed]),0),
            "kW"))


####### Weighted Average Power ####
# Calculate energy per period
coastal_data_df$energy_kWh <- coastal_data_df$power_kW * coastal_data_df$time_differences
total_MWh <- sum(coastal_data_df$energy_kWh/1000)
print(paste("Total Enery Produced:",round(total_MWh,0),"MWh"))

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


### Make a weighted histogram of wind speed
# Define bins for wind speed
wind_breaks <- seq(0, max(coastal_data_df$wind_speed_100m)+.1, by = .2)

# Create weighted wind histogram
hist_wind <- hist(coastal_data_df$wind_speed_100m, 
                  breaks = wind_breaks, 
                  plot = FALSE, 
                  weights = coastal_data_df$time_of_day)

# Plot the weighted histogram
barplot(hist_wind$counts, 
        names.arg = hist_wind$breaks[-length(hist_wind$breaks)], 
        col = "lightblue", border = "black", 
        xlab = "Wind Speed (m/s)", ylab = "Total Hours", 
        main = "Distribution of Wind Speeds (Weighted by Duration)")

### Make a weighted histogram of power 
# Define bins for power
power_breaks <- seq(0, max(coastal_data_df$power_kW)+50, by = 50)

# Create weighted power histogram
hist_power <- hist(coastal_data_df$power_kW, 
                   breaks = power_breaks, 
                   plot = FALSE, 
                   weights = coastal_data_df$time_differences)

# Plot the weighted histogram
barplot(hist_power$counts, 
        names.arg = hist_power$breaks[-length(hist_power$breaks)], 
        col = "lightblue", border = "black", 
        xlab = "Power (kW)", ylab = "Total Hours", 
        main = "Distribution of Power (Weighted by Duration)")


###### Combined Weighted Average Power Plot #####
bin_width <- .5
coastal_data_df$wind_bins <- cut(coastal_data_df$wind_speed_100m, 
                                 breaks = seq(0, max(coastal_data_df$wind_speed_100m) + bin_width, 
                                              bin_width), 
                                 include.lowest = TRUE)

# Calculate total hours observed and total energy produced for each wind speed bin
summary_data <- aggregate(cbind(time_differences, energy_kWh) ~ wind_bins, 
                          data = coastal_data_df, sum)

# Create a bar plot of total hours observed
hours_plot <- ggplot(summary_data, aes(x = wind_bins, y = time_differences)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Wind Speed (m/s)", 
       y = "Total Hours Observed", 
       title = "Bar Plot: Wind Speed Bins vs. Total Hours Observed") +
  scale_x_discrete(labels = function(x) gsub("\\[|\\)", "", x))

# Create a line plot of total energy produced
scale_factor = 7000
energy_plot <- ggplot(summary_data, aes(x = wind_bins, y = energy_kWh/scale_factor)) +
  geom_bar(stat = "identity", fill = "maroon", color = "black", alpha = 0.7) +
  labs(x = "Wind Speed Bins", 
       y = "Total Energy Produced", 
       title = "Bar Plot: Wind Speed Bins vs. Total Energy Produced") +
  scale_x_discrete(labels = function(x) gsub("\\[|\\)", "", x)) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale_factor/1000, name = "Energy (MWh) (scaled)"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot both on the same axis
annotate_x <- max(coastal_data_df$wind_speed_100m) 
annotate_y <- .05 * sum(coastal_data_df$time_differences)
combined_plot <- hours_plot +
  geom_bar(data = summary_data, 
           aes(x = wind_bins, y = energy_kWh/scale_factor),
           fill = "maroon", 
           stat = "identity", 
           alpha = 0.7) +
  labs(title = "VA Coastal Wind (Single Turbine): Total Hours of Wind Observed and Total Energy Produced (Jan-Dec 2022)") +
  annotate("text", 
           x = annotate_x, y = annotate_y, 
           label = paste("Total Power Produced:",round(total_MWh,0),"MWh"), 
           hjust = -1, vjust = 0, 
           color = "black") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale_factor/1000, name = "Energy (MWh) (scaled)"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the combined plot
print(combined_plot)
ggsave("weighted_average_power_and_wind.png", plot = combined_plot, 
       width = 8, height = 8)

