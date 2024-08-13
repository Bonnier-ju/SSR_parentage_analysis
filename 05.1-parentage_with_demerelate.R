
############## Calculate Relatedness on Diploid Genetic Data with Demerelated ################

# Installer les packages nécessaires
install.packages("sfsmisc")
install.packages("mlogit")
install.packages("Demerelate")
install.packages("geosphere")

# Charger les bibliothèques
library(sfsmisc)
library(mlogit)
library(Demerelate)
library(geosphere)
library(ggplot2)

# Chemin des fichiers
SSR_data <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.1-parentage_with_demerelate/Input_files/nSSR_Regina.csv"
geo_data <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.1-parentage_with_demerelate/Input_files/geo_inds_REG.csv"
dir_result <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.1-parentage_with_demerelate/results"

# Lire les fichiers CSV
SSR_table <- read.csv(SSR_data, header = TRUE)
geo_table <- read.csv(geo_data, header = TRUE)

# Transformer en data frame
SSR_df <- as.data.frame(SSR_table)
geo_df <- as.data.frame(geo_table)

# Vérifier les individus communs entre les deux fichiers
common_ids <- intersect(SSR_df$Id, geo_df$Id)
SSR_df <- SSR_df[SSR_df$Id %in% common_ids, ]
geo_df <- geo_df[geo_df$Id %in% common_ids, ]

# Réindexer les lignes
SSR_df <- SSR_df[order(SSR_df$Id), ]
geo_df <- geo_df[order(geo_df$Id), ]

# Calculer la parenté
relat_all_shar <- Emp.calc(SSR_df, value="loiselle", ref.pop="NA")

# Extraire les valeurs de parenté
relat_values <- as.numeric(relat_all_shar)

# Créer des catégories de parenté
categories <- cut(relat_values, breaks=c(-Inf, 0, 0.25, 0.5, Inf), labels=c("unrelated", "4th degree", "3rd degree", "2nd degree"))

# Calculer les distances entre les paires d'individus
geo_coords <- geo_df[, c("Id", "lat", "long")]
dist_matrix <- distm(geo_coords[, c("long", "lat")], fun=distHaversine)


##################### Apply on each sampling sites ######################

# Créer les paires d'individus par sites
pairs_REG <- expand.grid(1:nrow(geo_coords), 1:nrow(geo_coords))
pairs_REG <- pairs_REG[pairs_REG$Var1 < pairs_REG$Var2, ]
pairs_REG$distance <- dist_matrix[upper.tri(dist_matrix)]
pairs_REG$relatedness <- relat_values
pairs_REG$category <- categories

# Créer le boxplot horizontal
ggplot(pairs_REG, aes(x=category, y=distance)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Relatedness Categories by Distance", x="Relatedness Category", y="Distance (meters)")



################### Combine results of 4 sites in one plot ###############

# Ajouter une colonne pour identifier chaque site avec les noms complets
pairs_NOU$site <- "Nouragues"
pairs_REG$site <- "Regina"
pairs_PAR$site <- "Paracou"
pairs_SPR$site <- "Sparouine"

str(pairs_NOU)

# Concaténer les quatre jeux de données
combined_pairs <- rbind(pairs_NOU, pairs_REG, pairs_PAR, pairs_SPR)
# Créer un facteur avec l'ordre des sites souhaité
combined_pairs$site <- factor(combined_pairs$site, levels = c("Sparouine", "Paracou", "Nouragues", "Regina"))


# Créer le boxplot horizontal avec différenciation des sites
ggplot(combined_pairs, aes(x=category, y=distance, fill=site)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="", x="Family relationship", y="Distance (meters)") +
  scale_fill_manual(values = c("Sparouine" = "#EE0000", 
                               "Paracou" = "darkgoldenrod2", 
                               "Nouragues" = "springgreen3", 
                               "Regina" = "#4682B4")) +
  theme(legend.title = element_blank())



