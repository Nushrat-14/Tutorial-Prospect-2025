# Clear all objects from the global environment
rm(list = ls())
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(janitor)
# Read the CSV
abs_data <- read_csv("C:/Users/N_Chowdhury1/Desktop/TRACER-MAP/TAP_NEPH_10min.csv")

colnames(abs_data)
str(abs_data$Datetime)
# Attempt to parse the date-time column using parse_date_time
abs_data$Datetime <- parse_date_time(abs_data$Datetime, orders = c("dmy HMS"))

# Filter for the two time periods
E1 <- abs_data %>%
  filter(Datetime >= mdy_hms("07/01/2022 00:00:00") & Datetime <= mdy_hms("07/03/2022 11:26:59"))

E2 <- abs_data %>%
  filter(Datetime >= mdy_hms("08/01/2022 10:53:00") & Datetime <= mdy_hms("08/08/2022 10:00:59"))



#Line plot for E1 (AAE vs Time)
ggplot(E1, aes(x=Datetime,y=AAE)) +
  geom_line(color="black", linewidth = 1) +
  geom_hline(yintercept = 1.2, linetype = "dashed",color = "red", linewidth = 1.2) +
  labs (title = "Absorption Angstrom exponent (July)",
        y="AAE",
        x="Date-Time") +
  scale_x_datetime(
    date_labels = "%b-%d, %H:%M",
    breaks = scales::date_breaks("6 hours")  # adjust as needed
  ) +
  theme_bw() +  
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face="bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate 45°, right-justified
  )

#Line plot for E2 (AAE vs Time)
ggplot(E2, aes(x=Datetime,y=AAE)) +
  geom_line(color="black", linewidth = 1) +
  geom_hline(yintercept = 1.2, linetype = "dashed",color = "red", linewidth = 1.2) +
  labs (title = "Absorption Angstrom exponent (August)",
        y="AAE",
        x="Date-Time") +
  scale_x_datetime(
    date_labels = "%b-%d, %H:%M",
    breaks = scales::date_breaks("6 hours")  # adjust as needed
  ) +
  theme_bw() +  
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face="bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate 45°, right-justified
  )

#Line plot for E1 (SAE vs Time)
ggplot(E1, aes(x=Datetime,y=SAE)) +
  geom_line(color="red", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed",color = "black", linewidth = 1.2) +
  labs (title = "Scattering Angstrom exponent (July)",
        y="SAE",
        x="Date-Time") +
  scale_x_datetime(
    date_labels = "%b-%d, %H:%M",
    breaks = scales::date_breaks("6 hours")  # adjust as needed
  ) +
  theme_bw() +  
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face="bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate 45°, right-justified
  )
#ggsave(filename="AAE.png", units="in", width=9, height=4.5)

#Line plot for E2 (SAE vs Time)
ggplot(E2, aes(x=Datetime,y=SAE)) +
  geom_line(color="red", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed",color = "black", linewidth = 1.2) +
  labs (title = "Scattering Angstrom exponent (August)",
        y="SAE",
        x="Date-Time") +
  scale_x_datetime(
    date_labels = "%b-%d, %H:%M",
    breaks = scales::date_breaks("6 hours")  # adjust as needed
  ) +
  theme_bw() +  
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face="bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate 45°, right-justified
  )