################################################################################
################################################################################
################ Analyses on tree's repoductive succes #########################
################################################################################
################################################################################

######### Analyse Offspring Count per Cluster from Colony BestCluster ##########

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


plot_name <- "Regina"

# Define color mapping
plot_colors <- c(
  Sparouine = "aquamarine3",
  Paracou = "chocolate3",
  Nouragues = "#548B54",
  Regina = "mediumorchid4"
)
plot_color <- plot_colors[[plot_name]]

# file path 
file_path <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/05-Parentage_analysis/05.4-parentage_with_colony/Regina/Results_Regina/Regina_colony.BestCluster.csv"
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


################## Parental success within clusters ###########################

# Create ParentID–Cluster table (combine Father and Mother IDs)
parent_contributions <- cluster_data %>%
  select(OffspringID, ClusterIndex, FatherID, MotherID) %>%
  pivot_longer(cols = c(FatherID, MotherID), names_to = "ParentType", values_to = "ParentID") %>%
  filter(ParentID != "" & !is.na(ParentID)) %>%                   # remove missing
  filter(!grepl("^#", ParentID))                                 # keep only real sampled parents

# Count offspring per parent per cluster
parent_cluster_counts <- parent_contributions %>%
  group_by(ParentID, ClusterIndex) %>%
  summarise(Offspring_Count = n(), .groups = "drop")

# Order parents by total offspring
parent_order <- parent_cluster_counts %>%
  group_by(ParentID) %>%
  summarise(Total_Offspring = sum(Offspring_Count)) %>%
  arrange(desc(Total_Offspring)) %>%
  pull(ParentID)

# Plot
cluster_parent_plot <- ggplot(parent_cluster_counts, aes(x = factor(ParentID, levels = parent_order),
                                                         y = Offspring_Count,
                                                         fill = as.factor(ClusterIndex))) +
  geom_bar(stat = "identity") +
  labs(title = paste0("Number of Offspring per Parent (by Cluster) – ", plot_name),
       x = "Parent ID",
       y = "Number of Offspring",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Display
print(cluster_parent_plot)

# Save the plot
output_file2 <- file.path(output_dir, paste0("offspring_per_parent_by_cluster_", plot_name, ".png"))
ggsave(output_file2, cluster_parent_plot, width = 10, height = 6, dpi = 400, bg = "white")


