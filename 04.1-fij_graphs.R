#################### VIsualization of Fij by distance class ###################
library(tidyverse)
library(ggplot2)
library(dplyr)

file_path <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/04-diversity_dist_class_per_sites/Fij_graphs/Fij_SPR.csv"


df <- read.csv(file_path, sep = ";", header = FALSE)

distance_classes <- c("0", "30", "60", "90", "130", "170", "220", "300", "600")

# Extraire les valeurs depuis les bonnes lignes et colonnes
obs_val <- as.numeric(df[6, 2:10])
CI_inf <- as.numeric(df[9, 2:10])
CI_sup <- as.numeric(df[10, 2:10])



# Construire le tableau final
x_plot <- c(1, 3, 4, 5, 6, 7, 8, 9, 11)

kinship_df <- data.frame(
  distance_class = distance_classes,
  obs_val = obs_val,
  CI_inf = CI_inf,
  CI_sup = CI_sup
) %>%
  mutate(
    distance_m = as.numeric(as.character(distance_class)),
    x_plot = x_plot
  )


ggplot() +
  # Ribbon uniquement pour les distances > 0
  geom_ribbon(data = subset(kinship_df, distance_m != 0),
              aes(x = x_plot, ymin = CI_inf, ymax = CI_sup),
              fill = "lightblue", alpha = 0.4) +
  
  # Ligne uniquement pour les points > 0 m
  geom_line(data = subset(kinship_df, distance_m != 0),
            aes(x = x_plot, y = obs_val),
            color = "steelblue", size = 1) +
  
  # Points > 0 m
  geom_point(data = subset(kinship_df, distance_m != 0),
             aes(x = x_plot, y = obs_val),
             size = 3, color = "steelblue") +
  
  # Point 0 m (intra-individuel), isolé
  geom_point(data = subset(kinship_df, distance_m == 0),
             aes(x = x_plot, y = obs_val),
             size = 4, shape = 18, color = "red") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  
  # Axe personnalisé avec les vrais labels
  scale_x_continuous(
    breaks = kinship_df$x_plot,
    labels = kinship_df$distance_class
  ) +
  
  labs(
    title = "Autocorrelogram of Kinship Coefficients (Loiselle et al., 1995)",
    x = "Distance class (pseudo-log scale)",
    y = "Kinship coefficient"
  ) +
  theme_minimal()
