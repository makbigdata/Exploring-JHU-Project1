library(tidyverse)
# I like the tidy way of doing things using tidyverse.

# Import data using read_delim with handling for missing values and column types
power_data <- read_delim("household_power_consumption.txt", 
                         delim = ";", 
                         na = "?", 
                         col_types = cols(
                           Date = col_character(),
                           Time = col_character(),
                           Global_active_power = col_double(),
                           Global_reactive_power = col_double(),
                           Voltage = col_double(),
                           Global_intensity = col_double(),
                           Sub_metering_1 = col_double(),
                           Sub_metering_2 = col_double(),
                           Sub_metering_3 = col_double()
                         ),
                         show_col_types = FALSE)

# Convert Date and Time to appropriate formats and filter for the required dates
power_data <- power_data %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    dateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  filter(dateTime >= as.POSIXct("2007-02-01") & dateTime < as.POSIXct("2007-02-03"))


plot1 <- ggplot(power_data, aes(x = Global_active_power)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  labs(
    title = "Global Active Power",
    x = "Global Active Power (kilowatts)",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("plot1.png", plot1, width = 6, height = 6, dpi = 80)



plot2 <- ggplot(power_data, aes(x = dateTime, y = Global_active_power)) +
  geom_line() +
  labs(
    x = "",
    y = "Global Active Power (kilowatts)"
  ) +
  theme_minimal()

ggsave("plot2.png", plot2, width = 6, height = 6, dpi = 80)


plot3 <- ggplot(power_data, aes(x = dateTime)) +
  geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1")) +
  geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, color = "Sub_metering_3")) +
  labs(
    x = "",
    y = "Energy sub metering",
    color = "Legend"
  ) +
  theme_minimal()

ggsave("plot3.png", plot3, width = 6, height = 6, dpi = 80)



plot4 <- ggplot(power_data, aes(x = dateTime, y = Global_reactive_power)) +
  geom_line() +
  labs(
    x = "datetime",
    y = "Global Reactive Power"
  ) +
  theme_minimal()

ggsave("plot4.png", plot4, width = 6, height = 6, dpi = 80)


library(patchwork)
# Combine all plots
# Combine plots into a 2x2 grid
combined_plot <- (plot1 | plot2) / (plot3 | plot4)

# Save the combined plot
ggsave("plot_combined.png", combined_plot, width = 12, height = 12, dpi = 80)


