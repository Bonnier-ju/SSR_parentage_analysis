################################################################################
################################################################################
################ Analyses on tree's repoductive succes #########################
################################################################################
################################################################################

######### Analyse Offspring Count per Cluster from Colony BestCluster ##########


library(dplyr)
library(ggplot2)
library(stringr)

plot_code <- "Nouragues"

# file path 
base_path <- "C:\Users\bonni\OneDrive\University\Thesis\Dicorynia\Article-SSR_population\Analysis\05-Parentage_analysis\05.4-parentage_with_colony\Nouragues\Results_Nouragues\Nouragues_colony.BestCluster.csv"
file_path <- file.path(base_path, plot_code, paste0(plot_code, "_colony.BestCluster.csv"))

# Read the file
cluster_data <- read.csv(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
names(cluster_data) <- str_replace_all(names(cluster_data), "\\s+", "_") # Clean names

# Count number of offspring per cluster
offspring_count <- cluster_data %>%
  group_by(Cluster) %>%
  summarise(Offspring_Count = n()) %>%
  arrange(desc(Offspring_Count))

# Display
print(offspring_count)

# Plot
p <- ggplot(offspring_count, aes(x = reorder(as.factor(Cluster), -Offspring_Count), y = Offspring_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Offspring_Count), vjust = -0.5, size = 3) +
  labs(title = paste0("Number of Offspring per Cluster – ", plot_code),
       x = "Cluster",
       y = "Number of Offspring") +
  theme_minimal()

# Display plot
print(p)

# Save plot
ggsave(
  filename = paste0("offspring_per_cluster_", plot_code, ".png"),
  path = file.path(base_path, plot_code),
  plot = p,
  width = 8,
  height = 5
)
