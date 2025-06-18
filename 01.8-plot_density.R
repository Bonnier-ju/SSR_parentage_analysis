##############################################################################
####################### Plots density ########################################
##############################################################################

library(tidyverse)
library(sf)

# === Load your CSV file ===
data <- read.csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Data_initial/Nouragues/Nouragues_full_data.csv")

# === Classify individuals by dbh ===
data <- data %>%
  mutate(dbh_class = case_when(
    dbh < 4 ~ "SED",
    dbh >= 4 & dbh < 30 ~ "INT",
    dbh >= 30 ~ "ADL",
    TRUE ~ NA_character_
  ))

# === Convert to sf object and project to UTM ===
data_sf <- st_as_sf(data, coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 32622)  # UTM zone 22N

# === Estimate surface using all individuals ===
# Create grid of 10 x 10 m cells
grid <- st_make_grid(data_sf, cellsize = 20, square = TRUE)

# Select only grid cells intersecting at least one individual
grid_occ <- grid[st_intersects(grid, data_sf, sparse = FALSE) %>% apply(1, any)]

# Total area in hectares
area_ha <- length(grid_occ) * (20 * 20) / 10000

# === Count individuals by dbh class and compute density ===
density_table <- data %>%
  filter(!is.na(dbh_class)) %>%
  group_by(dbh_class) %>%
  summarise(Nb_individuals = n(), .groups = "drop") %>%
  mutate(
    Area_ha = area_ha,
    Density_ind_per_ha = Nb_individuals / area_ha
  )

# === Add total density (all individuals combined) ===
total_line <- tibble(
  dbh_class = "Total",
  Nb_individuals = nrow(data_sf),
  Area_ha = area_ha,
  Density_ind_per_ha = nrow(data_sf) / area_ha
)

density_table <- bind_rows(density_table, total_line)

# === View result ===
print(density_table)


##############################################################################
########################## DBH class density analysis ########################
##############################################################################

library(tidyverse)
library(sf)

# === Define plot name and color ===
plot_name <- "Regina"

# === Load input CSV ===
data <- read.csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Data_initial/Regina/Regina_full_data.csv")

# === Keep only necessary columns ===
data <- data %>%
  select(ID, plot, lat, long, dbh)

# === Remove rows with missing coordinates or dbh ===
data <- data %>%
  filter(!is.na(lat), !is.na(long), !is.na(dbh))

# === Classify individuals by dbh ===
data <- data %>%
  mutate(dbh_class = case_when(
    dbh < 4 ~ "SED",
    dbh >= 4 & dbh < 30 ~ "INT",
    dbh >= 30 ~ "ADL",
    TRUE ~ NA_character_
  ))

# === Convert to sf object and reproject to UTM ===
data_sf <- st_as_sf(data, coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 32622)

# === Build grid and estimate area ===
grid <- st_make_grid(data_sf, cellsize = 20, square = TRUE)
grid_occ <- grid[st_intersects(grid, data_sf, sparse = FALSE) %>% apply(1, any)]
area_ha <- length(grid_occ) * (20 * 20) / 10000  # convert m² to ha

# === Count individuals by dbh class and compute density ===
density_table <- data %>%
  filter(!is.na(dbh_class)) %>%
  group_by(dbh_class) %>%
  summarise(Nb_individuals = n(), .groups = "drop") %>%
  mutate(
    Area_ha = area_ha,
    Density_ind_per_ha = Nb_individuals / area_ha
  )

# === Add total line ===
total_line <- tibble(
  dbh_class = "Total",
  Nb_individuals = nrow(data_sf),
  Area_ha = area_ha,
  Density_ind_per_ha = nrow(data_sf) / area_ha
)

density_table <- bind_rows(density_table, total_line)
density_table

