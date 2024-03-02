
# We'll do some theoretically modeling of wind using the Weibull distribution

library(lubridate)
library(fitdistrplus)
library("tidyverse")

# # Source the data creation script
# source("VA_Coastal_Wind/main.R")

# Make sure all the values are > 0
sum(coastal_data_df$wind_speed_100m <= 0) # there's 1 value equal to zero

coastal_data_df$wind_speed_100m[coastal_data_df$wind_speed_100m <= 0] = 1e-4

# Fit all the wind to a Weibull distribution
all_dates_fit <- fitdistr(coastal_data_df$wind_speed_100m, densfun = "weibull")

# Shape and scale values
shape_all <- all_dates_fit$estimate[1]
scale_all <- all_dates_fit$estimate[2]

# make a sequence and fit the distribution
x_values <- seq(0,32, length.out = 100)
weibull_densities <- dweibull(x_values, shape = shape_all, scale = scale_all)

# Put it in a df
weibull_all_df <- data.frame(x = x_values, density = weibull_densities)


# Plot the line over the histogram
ggplot(coastal_data_df, aes(x = wind_speed_100m)) +
  geom_histogram(aes(y = ..density..),
                 fill = pal[1], 
                 alpha = 0.7,
                 binwidth = .2) +
  geom_line(data = weibull_all_df, aes(x = x, y = density), color = "red") +
  labs(x = "Wind Speed (m/s)", 
       y = "Density", 
       title = "Wind Speed Density Plot with fitted Weibull Distribution") +
  theme_minimal()


# Fit seasonal Weibulls
winter_fit <- fitdistr(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Winter/Spring"],
                       densfun = "weibull")

summer_fit <- fitdistr(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Summer/Fall"],
                    densfun = "weibull")

# Shape an size
shape_winter <- winter_fit$estimate[1]
scale_winter <- winter_fit$estimate[2]

shape_summer <- summer_fit$estimate[1]
scale_summer <- summer_fit$estimate[2]

# Make the densities
winter_densities <- dweibull(x_values, shape = shape_winter, scale = scale_winter)
summer_densities <- dweibull(x_values, shape = shape_summer, scale = scale_summer)

# Put it in a df
weibull_seasonal_df <- data.frame(x = x_values, 
                                  winter = winter_densities, 
                                  summer = summer_densities)


# Plot them bad boys
ggplot(coastal_data_df, aes(x = wind_speed_100m)) +
  geom_histogram(data = subset(coastal_data_df, season_group == "Winter/Spring"), 
                 mapping = aes(y = ..density.., fill = season_group),
                 alpha = 0.7, 
                 binwidth = 0.3, 
                 position = "identity") +
  geom_line(data = weibull_seasonal_df, 
            aes(x = x, y = winter), 
            color = "dodgerblue",
            size = 1) +
  geom_histogram(data = subset(coastal_data_df, season_group == "Summer/Fall"), 
                 mapping = aes(y = ..density.., fill = season_group),
                 alpha = 0.7, 
                 binwidth = 0.3, 
                 position = "identity") +
  geom_line(data = weibull_seasonal_df, 
            aes(x = x, y = summer), 
            color = "red",
            size = 1) +
  labs(title = "Wind Speed Distribution by Season",
       subtitle = "Summer/Fall = Jun-Nov; Winter/Spring = Dec-May",
       x = "Wind Speed", 
       y = "Frequency", 
       fill = "Season") +
  scale_x_continuous(breaks = seq(0, max(coastal_data_df$wind_speed_100m)*1.25, by = 4)) +
  scale_fill_manual(values = c("Winter/Spring" = pal[1], "Summer/Fall" = pal[2]),
                      name = "Season",
                      labels = c("Winter/Spring" = "Winter/Spring", "Summer/Fall" = "Summer/Fall")) +
  theme_gray()






  

