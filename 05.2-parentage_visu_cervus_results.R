

# Charger les packages nécessaires
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(geosphere)

# Lire les fichiers CSV
results <- read.csv("C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.2-parentage_with_cervus/Paracou/output_1paires.csv")
geo_data <- read.csv("C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Data_initial/Paracou/geo_inds.csv")

# Merge les coordonnées GPS avec les résultats
results_geo <- results %>%
  left_join(geo_data, by = c("Offspring.ID" = "Id")) %>%
  rename(lat_offspring = lat, long_offspring = long) %>%
  left_join(geo_data, by = c("First.candidate.ID" = "Id")) %>%
  rename(lat_first_parent = lat, long_first_parent = long) %>%
  left_join(geo_data, by = c("Second.candidate.ID" = "Id")) %>%
  rename(lat_second_parent = lat, long_second_parent = long)

# Créer des dataframes séparés pour les offspring et les parents
offspring_data <- geo_data %>%
  filter(Id %in% results_geo$Offspring.ID) %>%
  rename(lat = lat, long = long) %>%
  mutate(type = "Offspring")

parent_data <- geo_data %>%
  filter(Id %in% results_geo$First.candidate.ID | Id %in% results_geo$Second.candidate.ID) %>%
  rename(lat = lat, long = long) %>%
  mutate(type = "Parent")

# Combiner les données des parents et des offsprings
all_data <- bind_rows(offspring_data, parent_data)

# Préparer les liens individuels pour les parents 1 et 2 avec confiance élevée (*)
links_first <- results_geo %>%
  filter(Pair.confidence == "*") %>%
  select(Offspring.ID, lat_offspring, long_offspring, 
         First.candidate.ID, lat_first_parent, long_first_parent, Pair.confidence) %>%
  rename(parent_id = First.candidate.ID, lat_parent = lat_first_parent, long_parent = long_first_parent, confidence = Pair.confidence)

links_second <- results_geo %>%
  filter(Pair.confidence.1 == "*") %>%
  select(Offspring.ID, lat_offspring, long_offspring, 
         Second.candidate.ID, lat_second_parent, long_second_parent, Pair.confidence.1) %>%
  rename(parent_id = Second.candidate.ID, lat_parent = lat_second_parent, long_parent = long_second_parent, confidence = Pair.confidence.1)

# Combiner les liens des deux parents
links <- bind_rows(links_first, links_second)

# Plotter les individus et les liens parent-enfant avec des couleurs différentes pour les niveaux de confiance et les types d'individus
ggplot() +
  geom_point(data = all_data, aes(x = long, y = lat, color = type, shape = type), size = 3, alpha = 0.6) +
  geom_segment(data = links, aes(x = long_offspring, y = lat_offspring, 
                                 xend = long_parent, yend = lat_parent, color = confidence), size = 0.5, show.legend = FALSE) +
  scale_color_manual(values = c("Offspring" = "green", "Parent" = "blue")) +
  scale_shape_manual(values = c("Offspring" = 16, "Parent" = 17)) +
  theme_minimal() +
  labs(title = "Links between offsprings and parents",
       x = "Longitude", y = "Latitude", color = "Type", shape = "Type")

# Calculer la distance entre parents et offspring
links <- links %>%
  mutate(distance = distHaversine(cbind(long_offspring, lat_offspring), cbind(long_parent, lat_parent)))

# Calculer la distance moyenne, minimale et maximale
distance_stats <- links %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    min_distance = min(distance, na.rm = TRUE),
    max_distance = max(distance, na.rm = TRUE)
  )

# Afficher les statistiques de distance
print(distance_stats)

# Compter le nombre total de liens entre parents et offspring
total_links <- nrow(links)
print(paste("Nombre total de liens entre parents et offspring:", total_links))

# Classer les liens en fonction de la distance
distance_bins <- c(0, 30, 60, 90, 130, 170, 220, 300, 600)
links <- links %>%
  mutate(distance_class = cut(distance, breaks = distance_bins, include.lowest = TRUE, right = FALSE))

# Compter le nombre de liens dans chaque classe de distance
distance_class_count <- links %>%
  group_by(distance_class) %>%
  summarise(count = n())

# Afficher les résultats
print("Nombre de liens par classe de distance :")
print(distance_class_count)


