# Clear all objects from the global environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(janitor)
library(extrafont)

# Extracting Aerosol Concentration data in csv file format
VOC_data <- read_csv("C:/Users/N_Chowdhury1/OneDrive - Baylor University/Documents/Lab/Research/TRACER_Colorado/TRACER-MAP2022_VOC.csv")

#-------------------------------- Missing data handling-------------------------------------------
# Replaicng the missing data with one-third of MDL
MDL_VOC_data <- VOC_data

# Find rows where all columns 2-16 are NA. these columns are the VOCs
all_nan_rows <- apply(MDL_VOC_data[, 2:16], 1, function(x) all(is.na(x)))

# Create replacement values
replacement_value <- c(0.1, 0.36, 0.44, 0.06, 0.16, 0.16, 0.17, 0.14, 0.13, 0.1, 0.52, 0.41, 0.19, 0.36, 0.2)
replacement_values <- replacement_value / 3

# Apply replacements only to rows that don't have all NAs
for (row in seq_len(nrow(MDL_VOC_data))) {
  if (!all_nan_rows[row]) {
    for (col in 2:16) {
      if (is.na(MDL_VOC_data[row, col])) {
        MDL_VOC_data[row, col] <- replacement_values[col - 1]  # col-1 because replacement_values starts at column 2
      }
    }
  }
}

# Step 1: Create the flag values
flag_values <- ifelse(rowSums(is.na(MDL_VOC_data)) == 0, 1, 0)

# Step 2: Add the new column at the end
MDL_VOC_data$Flag_MDL <- flag_values

#---------------End of Missing data handling code---------------------------------------------

#----------------------VOCs of the target time frame--------------------------------------
# Filtering the target time period
#Clean the name of the column names
MDL_VOC_data <- MDL_VOC_data %>% 
  clean_names()# converts to snake_case, lower case, removes punctuation

str(MDL_VOC_data$abs_time)
#Convert column abs_time to datetime
MDL_VOC_data$abs_time <- mdy_hm(MDL_VOC_data$abs_time)

# Filter for the two time periods
E1 <- MDL_VOC_data %>%
  filter(abs_time >= mdy_hms("07/01/2022 00:00:00") & abs_time <= mdy_hms("07/03/2022 11:26:59"))

E2 <- MDL_VOC_data %>%
  filter(abs_time >= mdy_hms("08/01/2022 10:53:00") & abs_time <= mdy_hms("08/08/2022 10:00:59"))


# Select only compound columns (columns 2 to 16)
compound_data <- E1 %>% select(2:16)

# Compute mean ignoring NA
ARM <- compound_data %>% summarise_all(~mean(., na.rm = TRUE)) %>% as.numeric()

# Prepare labels
labels_compound <- c('Acetonitrile', 'Acetaldehyde', 'Acetone', 'DMS',
                     'Isoprene', 'MVKMACR', 'MEK', 'Hydroxyacetone',
                     'Benzene', 'Toluene', 'Styrene', 'mxylene',
                     'C3benzene', 'C4benzene', 'Monoterpene')


# Define custom colors (same RGB values as in MATLAB)
new_colors <- c("#0000FF", "#FF0000", "#006400", "#00FF00", "#00FFFF", "#FF00FF", "#FF8000",
                "#800080", "#804000", "#FFBFC0", "#008080", "#808080", "#808000", "#000080", "#800000")




#Time series plot 
VOC_long <- E1 %>%
  pivot_longer(cols = 2:16, names_to = "VOC", values_to = "Concentration")

# Convert VOC to factor with custom labels
VOC_long$VOC <- factor(VOC_long$VOC, levels = unique(VOC_long$VOC), labels = labels_compound)

ggplot(VOC_long, aes(x = abs_time, y = Concentration, color = VOC)) +
  geom_line(linewidth = 1) +
  labs(
    title = "VOC Concentrations Over Time (July)", 
    x = "Date-Time",
    y = "Concentration (ppbv)"
  ) +
  scale_color_manual(values = new_colors) +  # ← Custom color palette here
  scale_x_datetime(
    date_labels = "%b-%d, %H:%M",
    breaks = scales::date_breaks("3 hours")
  ) +
  theme_bw(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )




# Reshape to long format
#Time series plot 
VOC_long1 <- E2 %>%
  pivot_longer(cols = 2:16, names_to = "VOC", values_to = "Concentration")

# Convert VOC to factor with custom labels
VOC_long1$VOC <- factor(VOC_long1$VOC, levels = unique(VOC_long1$VOC), labels = labels_compound)

ggplot(VOC_long1, aes(x = abs_time, y = Concentration, color = VOC)) +
  geom_line(linewidth = 1) +
  labs(
    title = "VOC Concentrations Over Time (August)", 
    x = "Date-Time",
    y = "Concentration (ppbv)"
  ) +
  scale_color_manual(values = new_colors) +  # ← Custom color palette here
  scale_x_datetime(
    date_labels = "%b-%d, %H:%M",
    breaks = scales::date_breaks("6 hours")
  ) +
  theme_bw(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )