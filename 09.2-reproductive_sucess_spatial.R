################################################################################
########################## Reproductive succes spatial #########################
################################################################################

library(dplyr)
library(ggplot2)
library(sf)
library(readr)


# Define plot
plot_name <- "Nouragues"

# Define color per plot
plot_colors <- c(
  Sparouine = "aquamarine3",
  Paracou = "chocolate3",
  Nouragues = "#548B54",
  Regina = "mediumorchid4"
)
plot_color <- plot_colors[[plot_name]]

# Define input/output paths
input_csv <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/05-Parentage_analysis/05.6-parentage_with_haplotype/filtered_data_NOU.csv"
isolines_path <- "C:/Users/bonni/Desktop/Fichiers_cartes_Qgis/Isolignes/Isolignes_Nouragues_5m/Isolignes_Nouragues_5m_WGS84_reprojected.shp"
output_dir <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/09-reproductive_sucess/spatial_link_with_repro"
output_file <- file.path(output_dir, paste0("map_reproductive_success_", plot_name, ".png"))

# Load data
data <- read.csv(input_csv, sep = ",", stringsAsFactors = FALSE)

# Combine Parent1 and Parent2 
parents_long <- data %>%
  select(
    Parent1_Cervus, Latitude_Parent1, Longitude_Parent1,
    Parent2_Cervus, Latitude_Parent2, Longitude_Parent2
  ) %>%
  rename(
    ID_Parent1 = Parent1_Cervus, ID_Parent2 = Parent2_Cervus
  ) %>%
  # Rename to harmonize for pivoting
  rename_with(~ str_replace(., "_Parent1", "_1")) %>%
  rename_with(~ str_replace(., "_Parent2", "_2")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "ParentNum"),
    names_pattern = "(.+)_([12])"
  )


# Count number of offspring per parent
parent_summary <- parents_long %>%
  group_by(ID, Latitude, Longitude) %>%
  summarise(Offspring_Count = n(), .groups = "drop")

# Convert to sf
parent_sf <- st_as_sf(parent_summary, coords = c("Longitude", "Latitude"), crs = 4326)

# Load isolines (already in WGS84)
isolines <- st_read(isolines_path, quiet = TRUE)

# Create bounding box around parent area + small buffer
bbox_expanded <- st_bbox(parent_sf) %>% 
  st_as_sfc() %>% 
  st_transform(st_crs(isolines)) %>% 
  st_buffer(0.001)  # adjust buffer as needed (~100m)

# Crop isolines to the buffered bounding box
isolines_crop <- st_intersection(isolines, bbox_expanded)

# Plot
map_plot <- ggplot() +
  geom_sf(data = isolines_crop, color = "grey60", size = 0.2) +
  geom_sf(data = parent_sf, aes(size = Offspring_Count), color = plot_color, alpha = 0.8) +
  scale_size_continuous(range = c(2, 10)) +
  coord_sf() +
  theme_minimal() +
  labs(title = paste0("Spatial Distribution of Reproductive Success – ", plot_name),
       size = "Number of Offspring")

# Save the figure
ggsave(output_file, map_plot, width = 8, height = 6, dpi = 400, bg = "white")



###############################################################################
########################## Getis-Ord Gi* analysis #############################
###############################################################################

library(spdep)

# Project to a metric CRS (e.g. UTM Zone 22N for French Guiana)
parent_sf_proj <- st_transform(parent_sf, crs = 32622)  # UTM zone 22N

# Extract coordinates
coords <- st_coordinates(parent_sf_proj)

# Define spatial neighbors within 100 m (you can adjust the threshold)
nb <- dnearneigh(coords, d1 = 0, d2 = 100)

# Convert to spatial weights (row-standardized)
lw <- nb2listw(nb, style = "W")

# Compute local G-statistics (Getis-Ord Gi*)
gi_star <- localG(parent_sf_proj$Offspring_Count, listw = lw)

# Add results to the data
parent_sf_proj$Gi_star <- as.numeric(gi_star)

# Classify significance for visualization
parent_sf_proj$Gi_class <- cut(parent_sf_proj$Gi_star,
                               breaks = c(-Inf, -2, -1.65, 1.65, 2, Inf),
                               labels = c("Coldspot (p<0.05)", "Coldspot (p<0.10)", 
                                          "Not significant", "Hotspot (p<0.10)", "Hotspot (p<0.05)"))

print(parent_sf_proj, n=100)

# Plot Getis-Ord Gi* map
getis_plot <- ggplot() +
  geom_sf(data = isolines_crop, color = "grey70", size = 0.2) +
  geom_sf(data = parent_sf_proj, aes(color = Gi_class, size = Offspring_Count), alpha = 0.9) +
  scale_color_manual(values = c(
    "Coldspot (p<0.05)" = "blue",
    "Coldspot (p<0.10)" = "lightblue",
    "Not significant" = "grey60",
    "Hotspot (p<0.10)" = "orange",
    "Hotspot (p<0.05)" = "red"
  )) +
  scale_size_continuous(range = c(2, 8)) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = paste0("Getis-Ord Gi* – Spatial Clustering of Reproductive Success – ", plot_name),
    color = "Gi* significance",
    size = "Offspring Count"
  )

# Save Getis-Ord map
getis_output_file <- file.path(output_dir, paste0("map_getis_ord_", plot_name, ".png"))
ggsave(getis_output_file, getis_plot, width = 8, height = 6, dpi = 400, bg = "white")






