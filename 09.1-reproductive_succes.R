################################################################################
################################################################################
################ Analyses on tree's repoductive succes #########################
################################################################################
################################################################################

######### Analyse Offspring Count per Cluster from Colony BestCluster ##########

library(dplyr)
library(ggplot2)
library(stringr)

plot_name <- "Nouragues"

# Define color mapping
plot_colors <- c(
  Sparouine = "aquamarine3",
  Paracou = "chocolate3",
  Nouragues = "#548B54",
  Regina = "mediumorchid4"
)
plot_color <- plot_colors[[plot_name]]

# file path 
file_path <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/05-Parentage_analysis/05.4-parentage_with_colony/Nouragues/Results_Nouragues/Nouragues_colony.BestCluster.csv"
output_dir <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/09-reproductive_sucess/by_clusters"

# Read the file
cluster_data <- read.csv(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Count number of offspring per cluster
offspring_count <- cluster_data %>%
  group_by(ClusterIndex) %>%
  summarise(Offspring_Count = n()) %>%
  arrange(desc(Offspring_Count))

# Display
print(offspring_count)

# Plot
clusters_plot <- ggplot(offspring_count, aes(x = reorder(as.factor(ClusterIndex), -Offspring_Count), y = Offspring_Count)) +
  geom_bar(stat = "identity", fill = plot_color) +
  geom_text(aes(label = Offspring_Count), vjust = -0.5, size = 3) +
  labs(title = paste0("Number of Offspring per Cluster – ", plot_name),
       x = "Cluster",
       y = "Number of Offspring") +
  theme_minimal()
clusters_plot


# Save the plot
output_file <- file.path(output_dir, paste0("offspring_per_cluster_", plot_name, ".png"))
ggsave(output_file, clusters_plot, width = 8, height = 6, bg = "white" , dpi = 400)
