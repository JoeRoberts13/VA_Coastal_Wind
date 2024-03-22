
# We'll do some theoretically modeling of wind using the Weibull distribution

library(lubridate)
library(fitdistrplus)
library("tidyverse")
library("gganimate")

##### Front Matter ####
# Set seed for reproducibility
set.seed(123)

# # Source the data creation script
# source("VA_Coastal_Wind/main.R")

# Make sure all the values are > 0
sum(coastal_data_df$wind_speed_100m <= 0) # there's 1 value equal to zero

coastal_data_df$wind_speed_100m[coastal_data_df$wind_speed_100m <= 0] = 1e-2

###### QQ Function #####
qq_weibull = function(vector, shape = NULL, scale = NULL, type = c("theoretical","sample")) {
  type = match.arg(type)
  if (!is.numeric(vector)) {
    stop("Vector must be numeric.")
  }
  if(type == "theoretical"){
    if(missing(shape) | missing(scale)){
      stop("Shape and scale values must be provided for the theoretical distribution.")
    }
    if(!is.numeric(shape) | !is.numeric(scale)){
      stop("Shape and scale must be numeric.")
    }
    n <- length(vector)
    p <- ((1:n)-.5)/n 
    quantiles <- qweibull(p, shape = shape, scale = scale)
    return(quantiles)
  }
  if(type == "sample"){
    vector <- sort(vector)
    return(vector)
  }
}

##### Unfiltered Weibull #####

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

###### Plot ######
# Plot the line over the histogram
p_all_fitted_over_hist <- ggplot(coastal_data_df, aes(x = wind_speed_100m)) +
  geom_histogram(aes(y = ..density..),
                 fill = pal[1], 
                 alpha = 0.7,
                 binwidth = .2) +
  geom_line(data = weibull_all_df, aes(x = x, y = density), 
            color = "lightseagreen",
            linewidth = .8) +
  scale_x_continuous(breaks = seq(0,max(coastal_data_df$wind_speed_100m)*1.25,2)) +
  labs(x = "Wind Speed (m/s)", 
       y = "Density", 
       title = "Wind Speed Density Plot with fitted Weibull Distribution") +
  theme_gray()

print(p_all_fitted_over_hist)

ggsave("images/all_fitted_over_hist.png", p_all_fitted_over_hist,
       height = 4, width = 6)

###### QQ Fit #####
qq_all <- data.frame(
  theoretical = qq_weibull(coastal_data_df$wind_speed_100m, 
                            shape = shape_all,
                            scale = scale_all,
                            type = "theoretical"),
  sample = qq_weibull(coastal_data_df$wind_speed_100m, type = "sample")
)

p_qq_all <- ggplot(qq_all, aes(x = theoretical, y = sample)) +
  geom_point() +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  labs(title = "Q-Q Plot: Actual vs. Theoretical Wind Speed (All observations)", x = "Theoretical Wind Speed Quantiles", y = "Actual Wind Speed Quantiles") +
  theme_minimal()

print(p_qq_all)
ggsave("images/qq_all.png", p_qq_all,
       height = 4, width = 6)

print(all_dates_fit$loglik)
fitdistr(coastal_data_df$wind_speed_100m[coastal_data_df$season_group=="Summer/Fall"], 
         densfun = "weibull")$loglik


##### Season Grouping ####

# Generate estimates for season_groups
estimates_season_group <- coastal_data_df %>%
  group_by(season_group) %>%
  summarize(shape = fitdistr(wind_speed_100m, densfun = "weibull")$estimate[1],
            scale = fitdistr(wind_speed_100m, densfun = "weibull")$estimate[2]) %>%
  ungroup()

# Store in individual variables
shape_winter_spring <- estimates_season_group$shape[estimates_season_group$season_group == "Winter/Spring"]
shape_summer_fall <- estimates_season_group$shape[estimates_season_group$season_group == "Summer/Fall"]
scale_winter_spring <- estimates_season_group$scale[estimates_season_group$season_group == "Winter/Spring"]
scale_summer_fall <- estimates_season_group$scale[estimates_season_group$season_group == "Summer/Fall"]


# Calculate Weibull densities for each group
densities_season <- map2(estimates_season_group$shape, 
                  estimates_season_group$scale, 
                  ~dweibull(x_values, shape = .x, scale = .y))

# Create a data frame of the expanded results
density_season_df <- map2_df(densities_season, 
                             estimates_season_group$season_group, 
                             ~data.frame(x = x_values, density = .x, group = .y))


# Fitted distribution and seasonal histograms (Saved version)
p_seasonal_distribution <- ggplot(coastal_data_df, aes(x = wind_speed_100m)) +
  geom_histogram(aes(y = ..density..,
                     fill = season_group),
                 alpha = 0.7, 
                 binwidth = 0.3, 
                 position = "identity") +
  geom_line(data = density_season_df, 
            aes(x = x, 
                y = density,
                color = group), 
            linewidth = .8) +
  labs(title = "Wind Speed Distribution by Season",
       subtitle = "Summer/Fall = Jun-Nov; Winter/Spring = Dec-May",
       x = "Wind Speed", 
       y = "Frequency", 
       fill = "Season") +
  guides(color = guide_legend("Fitted Distribution"),
         fill = guide_legend("Observed")) +
  scale_x_continuous(breaks = seq(0, max(coastal_data_df$wind_speed_100m)*1.25, by = 2)) +
  theme_gray()

print(p_seasonal_distribution)

ggsave("images/season_distribution_hist.png", plot = p_seasonal_distribution,
       height = 4, width = 7)

# Less clean version, but it will plot the lines behind the fill of hist's nicely
ggplot(coastal_data_df, aes(x = wind_speed_100m)) +
  geom_histogram(data = subset(coastal_data_df, season_group == "Winter/Spring"), 
                 mapping = aes(y = ..density.., fill = season_group),
                 alpha = 0.7, 
                 binwidth = 0.3, 
                 position = "identity") +
  geom_line(data = density_season_df, 
            aes(x = x, 
                y = density,
                color = group), 
            size = 1) +
  geom_histogram(data = subset(coastal_data_df, season_group == "Summer/Fall"), 
                 mapping = aes(y = ..density.., fill = season_group),
                 alpha = 0.7, 
                 binwidth = 0.3, 
                 position = "identity") +
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

###### QQ Fit #####
# Summer/Fall theoretical and sample quantiles
qq_summer_fall <- data.frame(
  group = rep("Summer/Fall", length(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Summer/Fall"])),
  theoretical = qq_weibull(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Summer/Fall"], 
                           shape = shape_summer_fall,
                           scale = scale_summer_fall,
                           type = "theoretical"),
  sample = qq_weibull(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Summer/Fall"], type = "sample")
)

# Winter/Spring theoretical and sample quantiles
qq_winter_spring <- data.frame(
  group = rep("Winter/Spring", length(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Winter/Spring"])),
  theoretical = qq_weibull(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Winter/Spring"], 
                           shape = shape_winter_spring,
                           scale = scale_winter_spring,
                           type = "theoretical"),
  sample = qq_weibull(coastal_data_df$wind_speed_100m[coastal_data_df$season_group == "Winter/Spring"], type = "sample")
)

# Put them together in a long format
# There's probably a better way to do all this
qq_season_group <- rbind(qq_winter_spring, qq_summer_fall)


# Graph both
p_qq_season_group <- ggplot(qq_season_group, aes(x = theoretical, y = sample, color = group)) +
  geom_point() +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  facet_grid(cols = vars(group)) +
  guides(color = FALSE) +
  labs(title = "Q-Q Plot: Actual vs. Fitted Distribution Wind Speed (By Season)", 
       subtitle = paste0("Summer/Fall = Jun-Nov (n = ",
                         format(nrow(qq_summer_fall), big.mark = ","),
                         "); Winter/Spring = Dec-May (n = ",
                         format(nrow(qq_winter_spring), big.mark = ","),
                         ")"),
       x = "Theoretical Wind Speed Quantiles", 
       y = "Actual Wind Speed Quantiles") +
  theme_gray()

print(p_qq_season_group)
ggsave("images/qq_season_group.png", p_qq_season_group,
       height = 5, width = 8, bg = "white")


##### Monthly Grouping ####

# Generate estimates for season_groups
estimates_monthly <- coastal_data_df %>%
  group_by(month) %>%
  summarize(shape = fitdistr(wind_speed_100m, densfun = "weibull")$estimate[1],
            scale = fitdistr(wind_speed_100m, densfun = "weibull")$estimate[2]) %>%
  ungroup()


# Calculate Weibull densities for each group
densities_month <- map2(estimates_monthly$shape, 
                         estimates_monthly$scale, 
                         ~dweibull(x_values, shape = .x, scale = .y))

# Create a data frame of the expanded results
density_month_df <- map2_df(densities_month, 
                             estimates_monthly$month, 
                             ~data.frame(x = x_values, 
                                         density = .x, 
                                         group = .y))

# Make the group a date object for animation
# animation looks better if it starts w/ December
levels <- c("December", month.name[-12])

# Reformat the group date to later pull out the month names
density_month_df$group_date <- as.Date(paste("2021", 
                                             density_month_df$group, 
                                             "1", sep = "-"), "%Y-%b-%d")

# Plot with all the monthly predicted wind lines
ggplot(density_month_df, aes(x = x, 
                             y = density,
                             color = factor(format(group_date,"%b"),
                                            levels = c("Jan", "Feb",
                                                       "Mar", "Apr",
                                                       "May", "Jun",
                                                       "Jul", "Aug",
                                                       "Sep", "Oct",
                                                       "Nov", "Dec"),
                                            ordered = TRUE))) +
  geom_line(linewidth = .8) +
  labs(title = "Wind Speed Distribution by Month",
       x = "Wind Speed", 
       y = "Frequency", 
       color = "Month") +
  scale_x_continuous(breaks = seq(0, max(coastal_data_df$wind_speed_100m)*1.25, by = 4)) +
  # scale_color_manual(values = colorRampPalette(brewer.pal(12, "Blues"))) +
  scale_color_manual(values = colorRampPalette(c("lightskyblue", "navyblue"))(12)) +
  theme_gray()

###### Monthly Distribution animation #####
# Add animation code
animated_monthly <- ggplot(density_month_df, aes(x = x, 
                                                 y = density,
                                                 color = factor(format(group_date,"%B"),
                                                                levels = levels,
                                                                ordered = TRUE))) +
  geom_line(linewidth = .8) +
  labs(title = "Wind Speed Distribution by Month: {closest_state}",
       x = "Wind Speed", 
       y = "Frequency", 
       color = "Month") +
  scale_x_continuous(breaks = seq(0, max(coastal_data_df$wind_speed_100m)*1.25, by = 4)) +
  scale_color_manual(values = colorRampPalette(c("lightskyblue", "navyblue"))(12)) +
  theme_gray() +
  # Animation variables
  transition_states(factor(format(group_date,"%B"),
                           levels = levels,
                           ordered = TRUE), transition_length = 2, state_length = 1) +
  enter_fade() + exit_fade() +
  labs(title = 'Month: {closest_state}', subtitle = 'Wind Speed Distribution by Season: 2019-2024')

# Render the animation
anim <- animate(animated_monthly, renderer = gifski_renderer(), height = 600, width = 800, 
        duration = 10, fps = 10, end_pause = 10)

print(anim)

# Save the animation
anim_save("images/wind_speed_distribution_by_month.gif", animation = anim)


##### Theoretical capacity factor ####

# Function to output a capacity factor based on speeds
capacity_factor <- function(wind_speed, cut_in, cut_out, rated){
  if(!is.numeric(wind_speed) | !is.numeric(cut_in) | !is.numeric(cut_out) | !is.numeric(rated)){
    stop("All inputs must be numeric.")
  }
  capacity = ifelse(wind_speed < cut_in, 0,
                    ifelse(wind_speed < rated, wind_speed^3 / rated^3,
                           ifelse(wind_speed < cut_out, 1, 0)))
  return(capacity)
}

# Calculate a nominal capacity factor
speed_nominal <- seq(0, 35, by = .1)
capacity_nominal <- capacity_factor(speed_nominal, cut_in_speed, 
                                    cut_out_speed, rated_speed)

# Data frame that thang
nominal_capacity_df <- data.frame(speed = speed_nominal,
                                  capacity = capacity_nominal)

# Calculate density function over the same range
nominal_capacity_df$d_speed <- dweibull(nominal_capacity_df$speed, 
                                        shape = shape_all, scale = scale_all)

# Left-hand probability of wind being greater than each speed 
nominal_capacity_df$p_speed <- pweibull(speed_nominal, 
                                        shape = shape_all, 
                                        scale = scale_all, 
                                        lower.tail = FALSE)

###### Theoretical power curve ######
ggplot(nominal_capacity_df, aes(x = speed, y = capacity)) +
  geom_line() +
  labs(title = "Nominal Turbine Power Curve (without Air Density)",
       x = "Wind Speed (m/s)",
       y = "Capacity Factor") +
  theme_gray()


###### Expected value (includes air density error) #####
# Calculate expected capacity factor over the whole period
expected_cf <- 
  sum(nominal_capacity_df$capacity * nominal_capacity_df$d_speed) / 
  sum(nominal_capacity_df$d_speed)

print(paste("Expected capacity factor (whole year):", expected_cf))

# Actual capacity factor, assumes all periods are equal length
print(paste("Actual capacity factor (whole year):", mean(coastal_data_df$capacity_f)))


###### CDF Plot ####

# CDF with colors indicating corresponding capacity factor
ggplot(nominal_capacity_df, aes(x = speed, y = p_speed, color = capacity)) +
  geom_point(alpha = 0.7) +
  labs(title = "CDF of Wind Speed (w/ Capacity Factor)",
       x = "Wind Speed (m/s)",
       y = "Probability(Wind Speed is < x)") +
  scale_color_gradient(low = "lightskyblue", high = "red") +
  theme_gray()

# (Better) Plot both CDR and Capacity Factor as lines
ggplot(nominal_capacity_df, aes(x = speed, y = p_speed)) +
  geom_line(nominal_capacity_df, mapping = aes(x = speed, y = capacity),
           color = pal[2], 
           alpha = 0.8, 
           linewidth = 2) +
  geom_line(alpha = 0.8, 
            color = pal[1],
            linewidth = 2) +
  geom_line(data = data.frame(x = c(0, 0),
                               y = c(0, 0),
                               Key = c("Capacity Factor", "Probability Wind Speed > x")),
             mapping = aes(x = x, 
                           y = y,
                           color = Key),
            size = 2) +
  scale_y_continuous(breaks = seq(0, 1, by = .1),
                     sec.axis = sec_axis(~., name = "Capacity Factor",
                                         breaks = seq(0, 1, by = .1))) +
  scale_x_continuous(breaks = seq(0, max(nominal_capacity_df$speed)*1.25, by = 2)) +
  labs(title = "Relationship between Wind Speed Probability and corresponding Capacity Factor",
       subtitle = paste0("Fitted from all data (n = ", 
                        format(nrow(coastal_data_df), big.mark = ","),")"),
       x = "Wind Speed (m/s)",
       y = "Probability(Wind Speed is > x)") +
  theme_gray()


#### Seasonally Fitted Capacity Factor #####
speed_nominal <- seq(0, 35, by = .1)
capacity_seasonal <- capacity_factor(speed_nominal, cut_in_speed, 
                                    cut_out_speed, rated_speed)

# Data frame that thang
nominal_capacity_df <- data.frame(speed = speed_nominal,
                                  capacity = capacity_nominal)

# Calculate density function over the same range
nominal_capacity_df$d_speed_winter <- dweibull(nominal_capacity_df$speed, 
                                        shape = shape_winter_spring, 
                                        scale = scale_winter_spring)

nominal_capacity_df$d_speed_summer <- dweibull(nominal_capacity_df$speed, 
                                               shape = shape_summer_fall, 
                                               scale = scale_summer_fall)

# Left-hand probability of wind being greater than each speed 
nominal_capacity_df$p_speed_winter <- pweibull(speed_nominal, 
                                        shape = shape_winter_spring, 
                                        scale = scale_winter_spring, 
                                        lower.tail = FALSE)

nominal_capacity_df$p_speed_summer <- pweibull(speed_nominal, 
                                               shape = shape_summer_fall, 
                                               scale = scale_summer_fall, 
                                               lower.tail = FALSE)

###### Expected Value (Includes air density error) ######
# Winter/Spring
expected_cf_winter <- 
  sum(nominal_capacity_df$capacity * nominal_capacity_df$d_speed_winter) / 
  sum(nominal_capacity_df$d_speed_winter)

print(paste("Expected CF (Winter):", expected_cf_winter))

print(paste("Actual CF (Winter):", 
            mean(coastal_data_df$capacity_f[coastal_data_df$season_group == "Winter/Spring"])))

# Summer/Fall
expected_cf_summer <- 
  sum(nominal_capacity_df$capacity * nominal_capacity_df$d_speed_summer) / 
  sum(nominal_capacity_df$d_speed_summer)

print(paste("Expected CF (Summer):", expected_cf_summer))

print(paste("Actual CF (Summer):", 
            mean(coastal_data_df$capacity_f[coastal_data_df$season_group == "Summer/Fall"])))



###### Plot with winter/summer CDFs ######
# (Better) Plot both CDR and Capacity Factor as lines
ggplot(nominal_capacity_df, aes(x = speed, y = capacity)) +
  geom_line(color = pal[2], 
            alpha = 0.8, 
            linewidth = 2) +
  geom_line(nominal_capacity_df,
            mapping = aes(x = speed, y = p_speed_summer),
            alpha = 0.8, 
            color = pal[3],
            linewidth = 2) +
  geom_line(nominal_capacity_df,
            mapping = aes(x = speed, y = p_speed_winter),
            alpha = 0.8, 
            color = pal[4],
            linewidth = 2) +
  geom_line(data = data.frame(x = c(0, 0, 0),
                              y = c(0, 0, 0),
                              Key = c("Capacity Factor", 
                                      "Summer/Fall Probability",
                                      "Winter/Spring Probability")),
            mapping = aes(x = x, 
                          y = y,
                          color = Key),
            size = 2) +
  scale_y_continuous(breaks = seq(0, 1, by = .1),
                     sec.axis = sec_axis(~., name = "Capacity Factor",
                                         breaks = seq(0, 1, by = .1))) +
  scale_x_continuous(breaks = seq(0, max(nominal_capacity_df$speed)*1.25, by = 2)) +
  scale_color_manual(values = c(pal[2], pal[3], pal[4])) +
  labs(title = "Relationship between Wind Speed Probability and corresponding Capacity Factor",
       subtitle = paste0("Fitted from all data points (n = ", 
                         format(nrow(coastal_data_df), big.mark = ","),")"),
       x = "Wind Speed (m/s)",
       y = "Probability(Wind Speed is > x)") +
  theme_gray()


#### Monthly median Capacity Factor ####
capacity_factor_monthly <- coastal_data_df %>%
  group_by(month) %>%
  summarise(mean_cf = mean(capacity_f),
            median_cf = median(capacity_f)) %>%
  ungroup()

overall_median_cf <- median(coastal_data_df$capacity_f)

p_capacity_factor_monthly <- ggplot(capacity_factor_monthly, aes(x = month, y = median_cf, group =1)) +
  geom_line(color = pal[2],
            size = 0.8) +
  geom_hline(yintercept = overall_median_cf,
             linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  coord_cartesian(ylim = c(0,1)) +
  labs(title = "Median Capacity Factor by Month",
       x = "Month",
       y = "Capacity Factor") +
  theme_gray()

print(p_capacity_factor_monthly)
ggsave("images/capacity_factor_montly.png", plot = p_capacity_factor_monthly, 
       height = 4, width = 6)

