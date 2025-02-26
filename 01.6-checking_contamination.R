###################################################################
#################### Checking contamination on SSR ################
###################################################################

# Load required packages
library(adegenet)
library(dplyr)
library(tidyr)
library(poppr)
library(ggplot2)

################## Pre-processing of CSV file ######################
####################################################################

# Read the CSV file (Replace with the actual path)
file_path <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Data_initial/4sites/nSSR_4sites.csv"
data <- read.csv2(file_path, stringsAsFactors = FALSE)

# Remove non-genetic columns (ID, Pop, lat, long)
geno_data <- data %>% select(-Id, -Pop, -lat, -long)

# Replace '0' with NA
geno_data[geno_data == "0"] <- NA

# Transform concatenated alleles into "X/Y" format
geno_data <- geno_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), NA, gsub("(.{3})(.{3})", "\\1/\\2", .))))

# Add metadata back
geno_data <- cbind(data %>% select(Id, Pop), geno_data)

# Convert to `genind` format
genind_obj <- df2genind(geno_data[, -c(1,2)], sep = "/", pop = geno_data$Pop, ind.names = geno_data$Id, ploidy = 2)

# Check genind object
print(genind_obj)
print(genind_obj@pop)

################## Contamination Detection ######################
#################################################################

# Compute pairwise genetic distances
dist_matrix <- dist(genind_obj, method = "euclidean")  # Alternative: "Rogers", "Nei"

# Convert distance matrix to a data frame
dist_df <- as.data.frame(as.table(as.matrix(dist_matrix)))
colnames(dist_df) <- c("Ind1", "Ind2", "GeneticDistance")

# Remove self-comparisons (distance = 0)
dist_df <- dist_df[dist_df$Ind1 != dist_df$Ind2, ]

# Define a contamination threshold (e.g., bottom 1% of genetic distances)
threshold <- quantile(dist_df$GeneticDistance, 0.01)  

# Identify potential contamination cases
contaminated_pairs <- dist_df %>% filter(GeneticDistance < threshold)

# Print possible contamination cases
print(contaminated_pairs)

# Save contamination results
write.csv(contaminated_pairs, "contaminated_pairs.csv", row.names = FALSE)

################## Visualization ######################
#######################################################

# Plot the distribution of pairwise genetic distances
ggplot(dist_df, aes(x = GeneticDistance)) +
  geom_histogram(binwidth = 0.02, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = threshold, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Pairwise Genetic Distances",
       x = "Genetic Distance",
       y = "Frequency") +
  theme_minimal()
