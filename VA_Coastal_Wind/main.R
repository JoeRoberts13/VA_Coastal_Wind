
# Main script that contains API calls, variable creation and calculations
# for air density and turbine power.


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
start_date <- "2019-01-01"
end_date <- "2023-12-31"
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
if(status_code(response) == 200) print("API call successful!") else {
  print(paste("Failure. Code:", status_code(response))) }

coastal_data_list<- content(response, "parsed")

# Print the units of the data
print(paste("Time Units:", coastal_data_list$hourly_units$time))
print(paste("Temp Units:", coastal_data_list$hourly_units$temperature_2m))
print(paste("RH Units:", coastal_data_list$hourly_units$relative_humidity_2m))
print(paste("Pressure Units:", coastal_data_list$hourly_units$pressure_msl))
print(paste("Wind Units:", coastal_data_list$hourly_units$wind_speed_100m))
print(paste("Timezone:", coastal_data_list$timezone))



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
  date_time = data_time,
  temperature_2m = unlist(coastal_data_list$hourly$temperature_2m),
  relative_humidity_2m = unlist(coastal_data_list$hourly$relative_humidity_2m),
  pressure_msl = unlist(coastal_data_list$hourly$pressure_msl),
  wind_speed_100m = unlist(coastal_data_list$hourly$wind_speed_100m),
  temp_units = data_temp_units,
  pressure_units = data_pressure_units,
  wind_units = data_wind_units
  
)
# view(coastal_data_df)
# str(coastal_data_df)

print(paste("Date Range:", format(min(coastal_data_df$date_time),"%b %Y"), "-",
            format(max(coastal_data_df$date_time),"%b %Y")))

###### Variable Creation ####

# Add a time-of-day and season variable to see how things change
coastal_data_df$time_of_day <- format(coastal_data_df$date_time, "%H:%M:%S")
coastal_data_df$time_of_day <- factor(coastal_data_df$time_of_day, 
                                      levels = unique(coastal_data_df$time_of_day),
                                      ordered = TRUE)


spring = c(3,4,5)
summer = c(6,7,8)
fall = c(9,10,11)

coastal_data_df$season <- sapply(month(coastal_data_df$date_time),function(x){
  if (x %in% spring) return("Spring")
  else if (x %in% summer) return("Summer")
  else if (x %in% fall) return("Fall")
  else return("Winter")
})
coastal_data_df$season <- factor(coastal_data_df$season, levels = c("Winter",
                                                                    "Spring",
                                                                    "Summer",
                                                                    "Fall"))
# Create a new factor that groups seasons into two categories
coastal_data_df$season_group <- factor(
  ifelse(coastal_data_df$season %in% c("Winter", "Spring"), "Winter/Spring", 
         "Summer/Fall"))

# Add a day/night indicator based using suncalc library
# Create a data frame of unique dates
unique_dates <- data.frame(date = unique(as.Date(coastal_data_df$date_time)))

# Calculate sunrise and sunset times
sun_times <- suncalc::getSunlightTimes(date = unique_dates$date, 
                                       lat = latitude, 
                                       lon = longitude, 
                                       tz =  timezone)

# Merge sunrise and sunset times back with the unique_dates
unique_dates <- merge(unique_dates, sun_times, 
                      by.x = "date", by.y = "date")

unique_dates <- unique_dates[,c("date","sunrise","sunset")]

# Add date variable to the main df
coastal_data_df$date <- as.Date(coastal_data_df$date_time)

# Merge the sunrise and sunset into the original dataset
coastal_data_df <- merge(coastal_data_df, unique_dates, 
                         by = "date", all.x = TRUE)

# Add Day/Night variable
coastal_data_df$day_night <- with(coastal_data_df, 
                                  ifelse(date_time >= sunrise & date_time < sunset, 
                                         "Day", "Night"))


# Calculate variance by week
variance_by_week <- coastal_data_df %>%
  group_by(floor_date(date_time, unit = "week")) %>%
  summarise(var_wind_speed = var(wind_speed_100m))

colnames(variance_by_week) <- c("week", "var_wind_speed")

# Calculate variance by day
variance_by_date <- coastal_data_df %>%
  group_by(date) %>%
  summarise(var_wind_speed = var(wind_speed_100m))

# Calculate variance by month
variance_by_month <- coastal_data_df %>%
  group_by(floor_date(date_time, unit = "month")) %>%
  summarise(var_wind_speed = var(wind_speed_100m))

colnames(variance_by_month) <- c("month", "var_wind_speed")


# Add observation length and check for completeness
coastal_data_df$time_differences = c(NA, diff(coastal_data_df$date_time))
if(var(coastal_data_df$time_differences, na.rm = TRUE) == 0){
  coastal_data_df$time_differences[1] = 1
} else print("Observations aren't all the same")

# Do some generic data validation and scatterplots vs. time
print(paste0("Complete cases: ",
             sum(complete.cases(coastal_data_df))*100/nrow(coastal_data_df),
             "%"))


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


## I'll use the actual air density when wind > rated speed, but use 
# rated speed instead of wind speed. It'll be a little more realistic, but not 
# perfect. I think it's a reasonable assumption because the turbine will yaw the
# blades to keep the most efficient tip speed of the rotor. 
coastal_data_df <- coastal_data_df %>%
  mutate(power_kW = case_when(
    wind_speed_100m < cut_in_speed ~ 0,
    wind_speed_100m < rated_speed ~ n_combined * power_available,
    wind_speed_100m < cut_out_speed ~ n_combined * power_available / wind_speed_100m^3 * rated_speed^3,
    TRUE ~ 0
  ))

# Cap max power at rated power
coastal_data_df <- coastal_data_df %>%
  mutate(power_kW = case_when(
    power_kW > rated_power ~ rated_power,
    TRUE ~ power_kW
  ))


####### Weighted Average Power ####
# Calculate energy per observation, total and annual MWh
coastal_data_df$energy_kWh <- coastal_data_df$power_kW * coastal_data_df$time_differences
total_MWh <- sum(coastal_data_df$energy_kWh/1000)
print(paste("Total Energy Produced:", 
            format(round(total_MWh,0), big.mark = ","), 
            "MWh"))

years <- as.numeric(max(coastal_data_df$date_time) - min(coastal_data_df$date_time)) / 365.25
annual_MWh <- total_MWh / years
print(paste("Average Annual Energy Produced:",
            format(round(annual_MWh,0), big.mark = ","),
            "MWh"))

##### Capacity Factors ####
coastal_data_df$capacity_f <- coastal_data_df$power_kW / rated_power

