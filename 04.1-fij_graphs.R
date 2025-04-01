# Charger les packages nécessaires
library(tidyverse)

# Lire le fichier CSV avec séparateur ';'
df <- read.csv("Fij_SPR.csv", sep = ";", header = FALSE)

# Extraire les classes de distances (ligne 1 sauf les colonnes vides ou NA)
distance_classes <- as.character(df[1, 2:10])

# Extraire les valeurs observées (ligne "Obs val")
obs_val <- as.numeric(df[5, 2:10])

# Extraire les bornes de l’intervalle de confiance
CI_inf <- as.numeric(df[6, 2:10])
CI_sup <- as.numeric(df[7, 2:10])

# Créer un data.frame propre
kinship_df <- data.frame(
  distance_class = factor(distance_classes, levels = distance_classes),
  obs_val = obs_val,
  error_inf = obs_val - CI_inf,
  error_sup = CI_sup - obs_val
)

# Tracer l’autocorrélogramme
ggplot(kinship_df, aes(x = distance_class, y = obs_val)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = obs_val - error_inf, ymax = obs_val + error_sup), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Autocorrelogram of Kinship Coefficients (Loiselle et al., 1995)",
    x = "Distance class",
    y = "Kinship coefficient"
  ) +
  theme_minimal()
