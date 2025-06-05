################################################################################
############################# Growth rate ######################################
###############################################################################

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)


# Define the folder containing your CSV files
folder_path <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/07-population_dynamique/07.1-data/Parcelles_guyafor/Paracou"

file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

all_data <- file_list %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

glimpse(all_data)

# Filter for Dicorynia guianensis and valid measurements
data_dicorynia <- all_data %>%
  filter(Species == "guianensis", !is.na(CircCorr), CircCorr > 0) %>%
  mutate(
    TreeID = as.character(idTree),  
    DBH_cm = CircCorr / pi          
  ) %>%
  select(Forest, Plot, TreeID, CensusYear, DBH_cm)

# Calculate annual growth rate
growth_data <- data_dicorynia %>%
  arrange(Forest, Plot, TreeID, CensusYear) %>%
  group_by(Forest, Plot, TreeID) %>%
  mutate(
    Year_prev = lag(CensusYear),
    DBH_prev = lag(DBH_cm),
    Growth_cm_per_year = (DBH_cm - DBH_prev) / (CensusYear - Year_prev)
  ) %>%
  filter(!is.na(Growth_cm_per_year), Growth_cm_per_year >= 0) %>%
  ungroup()


# Mean growth rate per plot
mean_by_plot <- growth_data %>%
  group_by(Forest, Plot) %>%
  summarise(
    Mean_growth = mean(Growth_cm_per_year, na.rm = TRUE),
    SD_growth = sd(Growth_cm_per_year, na.rm = TRUE),
    N = n()
  ) %>%
  arrange(Forest, Plot)

# Overall mean growth rate across all plots
overall_mean <- growth_data %>%
  summarise(
    Overall_mean_growth = mean(Growth_cm_per_year, na.rm = TRUE),
    SD_growth = sd(Growth_cm_per_year, na.rm = TRUE),
    N = n()
  )

# Print results
print(mean_by_plot)
print(overall_mean)

# Visualize the growth distribution per plot
ggplot(growth_data, aes(x = as.factor(Plot), y = Growth_cm_per_year, fill = Forest)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "DBH Growth Rate of Dicorynia guianensis",
    x = "Plot",
    y = "Growth Rate (cm/year)"
  ) +
  theme_minimal()

