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


################### Checking heterozygosity excess #################

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





