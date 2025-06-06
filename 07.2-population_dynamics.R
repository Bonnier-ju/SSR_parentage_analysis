###############################################################################
########################## Population dynamics ################################
###############################################################################

library(dplyr)
library(readr)
library(spatstat.geom)
library(spatstat.explore)
library(spatstat.core)
library(ggplot2)
library(stringr)



folder_path <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/07-population_dynamique/07.1-data/Parcelles_guyafor"
file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Importation robuste avec typage forcé sur les colonnes problématiques
all_data <- file_list %>%
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE, 
             col_types = cols(
               Plot = col_character(),
               CorrCode = col_character(),
               .default = col_guess()
             ))
  }) %>%
  bind_rows()


# On garde seulement la première apparition de chaque arbre
dicorynia_unique <- all_data %>%
  filter(str_to_lower(Species) == "guianensis", !is.na(Xutm), !is.na(Yutm)) %>%
  mutate(TreeID = as.character(idTree)) %>%
  arrange(TreeID, CensusYear) %>%  # Tri par arbre puis année
  group_by(TreeID) %>%
  slice(1) %>%  # On garde la première ligne (la plus ancienne)
  ungroup() %>%
  select(Forest, Plot, TreeID, Xutm, Yutm)


# Créer un dossier de sortie
output_dir <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/07-population_dynamique/07.2-population_dynamics"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Liste des forêts uniques
forests <- unique(dicorynia_unique$Forest)

# Boucle par forêt
for (forest_id in forests) {
  cat("\nProcessing Forest:", forest_id, "\n")
  
  # Sous-ensemble des données par forêt
  forest_data <- dicorynia_unique %>% filter(Forest == forest_id)
  
  # Créer la fenêtre spatiale
  win <- owin(
    xrange = range(forest_data$Xutm),
    yrange = range(forest_data$Yutm)
  )
  
  # Objet spatstat : point pattern
  pp <- ppp(x = forest_data$Xutm,
            y = forest_data$Yutm,
            window = win)
  
  # Calcul de la fonction L de Ripley
  L <- Lest(pp, correction = "Ripley")
  
  # Tracer la fonction L(r) - r
  plot(L$r, L$iso - L$r, type = "l", col = "darkgreen", lwd = 2,
       xlab = "Distance r (m)", ylab = "L(r) - r",
       main = paste("Ripley L(r) – Forest", forest_id))
  abline(h = 0, lty = 2)
  
  # Export CSV
  L_df <- data.frame(distance = L$r, Lr_minus_r = L$iso - L$r)
  write_csv(L_df, file.path(output_dir, paste0("Ripley_L_Forest_", forest_id, ".csv")))
  
  # Export du graphique
  png(filename = file.path(output_dir, paste0("Ripley_L_Forest_", forest_id, ".png")),
      width = 800, height = 600)
  plot(L$r, L$iso - L$r, type = "l", col = "darkgreen", lwd = 2,
       xlab = "Distance r (m)", ylab = "L(r) - r",
       main = paste("Ripley L(r) – Forest", forest_id))
  abline(h = 0, lty = 2)
  dev.off()
}


# Initialiser le tableau de résultats
clark_results <- data.frame()

for (forest_id in forests) {
  cat("\nProcessing Forest:", forest_id, "\n")
  
  forest_data <- dicorynia_unique %>% filter(Forest == forest_id)
  
  win <- owin(
    xrange = range(forest_data$Xutm),
    yrange = range(forest_data$Yutm)
  )
  
  pp <- ppp(x = forest_data$Xutm,
            y = forest_data$Yutm,
            window = win)
  
  # Calcul de l’indice de Clark & Evans
  ce <- clarkevans(pp, correction = "none")  # "Donnelly" possible aussi
  ce_summary <- summary(ce)
  
  clark_results <- rbind(clark_results, data.frame(
    Forest = forest_id,
    R = ce_summary$R,
    p_value = ce_summary$p.value,
    mean_observed = ce_summary$mean.nnd,
    mean_expected = ce_summary$mean.exp
  ))
}

# Afficher et sauvegarder les résultats
print(clark_results)
write_csv(clark_results, file.path(output_dir, "ClarkEvans_results.csv"))




