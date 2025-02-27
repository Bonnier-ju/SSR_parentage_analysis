###################################################################
#################### Checking contamination on SSR ################
###################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(adegenet)
library(pegas)


#Load data 

data <- read.csv("C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Data_initial/Sparouine/Sparouine_full_data.csv", sep=",", header=TRUE)

# Store individual names before selecting SSR markers
indiv_ids <- data$ID

################### Checking heterozygosity excess #################
####################################################################


# Select only the SSR marker columns
markers <- data %>% select(starts_with("SSRSeq"))
# Identify loci pairs corresponding to allele 1 and allele 2
loci_pairs <- seq(1, ncol(markers), by=2)


# Function to calculate individual heterozygosity
heterozygosity <- function(row) {
  het_count <- 0
  total_loci <- 0
  
  for (i in loci_pairs) {
    allele1 <- row[i]
    allele2 <- row[i+1]
    
    # Check if data is available
    if (!is.na(allele1) & !is.na(allele2)) {
      total_loci <- total_loci + 1
      if (allele1 != allele2) {
        het_count <- het_count + 1
      }
    }
  }
  
  if (total_loci > 0) {
    return(het_count / total_loci)  # Proportion of heterozygosity
  } else {
    return(NA)  # Return NA if no valid loci
  }
}

# Apply the function to each individual
data$Heterozygosity <- apply(markers, 1, heterozygosity)

# Visualization of heterozygosity distribution
#800x600

ggplot(data, aes(x = Heterozygosity)) +
  geom_histogram(binwidth = 0.05, fill = "aquamarine3", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = mean(Heterozygosity, na.rm = TRUE) + 2 * sd(Heterozygosity, na.rm = TRUE)),
             color = "red", linetype = "dashed") +
  ggtitle("Distribution of Individual Heterozygosity - Sparouine") +
  xlab("Heterozygosity Rate") + ylab("Number of Individuals") +
  theme_minimal()

# Compute the heterozygosity threshold as the mean + 2 standard deviations
threshold <- mean(data$Heterozygosity, na.rm = TRUE) + 2 * sd(data$Heterozygosity, na.rm = TRUE)
# Select individuals with heterozygosity greater than the threshold
outliers <- data %>% filter(Heterozygosity > threshold)

# Save the list of suspect individuals to a CSV file
write.csv(outliers, "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/01-Pre-traitements/01.6-checking_contamination/suspect_individuals_heterozygosity_SPR.csv", row.names = FALSE)




################### Checking excess of genetic proximity with Nei's index #################
###########################################################################################

# Function to impute missing genotypes with the most frequent allele per locus
impute_na <- function(x) {
  most_frequent <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))  # Most frequent allele
  x[is.na(x)] <- most_frequent  # Replace NA with the most frequent value
  return(x)
}

# Apply imputation to all SSR columns
imputed_markers <- as.data.frame(lapply(markers, impute_na))

# Convert to genind after imputation
gen_data <- df2genind(imputed_markers, ploidy = 2, NA.char = "NA", ncode = 3)
rownames(gen_data@tab) <- indiv_ids

# Compute Nei's genetic distance between individuals
nei_dist <- nei.dist(gen_data, warning = TRUE)

# Convert distance matrix to a data frame for analysis
nei_dist_df <- as.data.frame(as.table(as.matrix(nei_dist)))

# Rename columns for clarity
colnames(nei_dist_df) <- c("Indiv1", "Indiv2", "Nei_Distance")

# Remove self-comparisons (Indiv1 == Indiv2)
nei_dist_df <- nei_dist_df %>% filter(Indiv1 != Indiv2)

# Plot the distribution of Nei's genetic distances
ggplot(nei_dist_df, aes(x = Nei_Distance)) +
  geom_histogram(binwidth = 0.01, fill = "aquamarine3", alpha = 0.5, color = "black") +
  ggtitle("Distribution of Nei's Genetic Distance - Sparouine") +
  xlab("Nei's Distance") + ylab("Frequency") +
  theme_minimal()


# Define a logical threshold for detecting individuals with excessive similarity
threshold <- 0.2

# Identify suspect pairs of individuals with excessively high genetic similarity
suspect_pairs <- nei_dist_df %>% filter(Nei_Distance < threshold)

# Print suspect pairs of individuals
print(suspect_pairs)

# Save suspect pairs to a CSV file for further investigation
write.csv(suspect_pairs, "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/01-Pre-traitements/01.6-checking_contamination/suspect_individuals_nei_SPR.csv", row.names = FALSE)









