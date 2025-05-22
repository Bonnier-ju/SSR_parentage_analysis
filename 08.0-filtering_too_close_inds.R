################################################################################
################### Filtering too close individuals ############################
################################################################################

install.packages("Demerelate")
install.packages("sfsmisc")
install.packages("mlogit")
install.packages("fts")

library(sfsmisc)
library(mlogit)
library(Demerelate)

# File path
data <- read.csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.0-filtered_relationship_inds/Sparouine/Sparouine_full_adl.csv")
data_f <- as.data.frame(data)

# Compute pairwise relatedness using Wang estimator
relat_wang <- Emp.calc(data_f, value = "wang", ref.pop = "NA")

# Plot the distribution of relatedness values
relatedness_values <- as.numeric(relat_wang)

hist(relatedness_values,
     breaks = 50,
     col = "lightblue",
     main = "Distribution of pairwise relatedness (Wang)",
     xlab = "Relatedness coefficient (Wang)",
     ylab = "Number of pairs")

abline(v = 0.5, col = "red", lty = 2)        # Full siblings / Parent-offspring
abline(v = 0.25, col = "orange", lty = 2)    # Half siblings
abline(v = 0.125, col = "green", lty = 2)    # First cousins
abline(v = 0.0625, col = "darkgreen", lty = 2) # Second cousins

legend("topright",
       legend = c("0.5", "0.25", "0.125", "0.0625"),
       col = c("red", "orange", "green", "darkgreen"),
       lty = 2,
       title = "Thresholds")

# --- Relatedness threshold (modifiable)
seuil_relatedness <- 0.125  # Change this value to modify the exclusion threshold

# --- Convert relatedness vector into a pairwise table
rel_tab <- data.frame(
  ind1 = sub("_.*", "", names(relat_wang)),
  ind2 = sub(".*_", "", names(relat_wang)),
  value = as.numeric(relat_wang),
  stringsAsFactors = FALSE
)

# --- Identify all pairs above the threshold
above_thresh <- rel_tab[rel_tab$value > seuil_relatedness, ]

# --- Prune individuals: iteratively remove one individual per pair until no pair exceeds the threshold
ids_to_remove <- c()
pairs_to_check <- above_thresh

while (nrow(pairs_to_check) > 0) {
  # Count the number of times each individual appears in related pairs
  id_freq <- table(c(pairs_to_check$ind1, pairs_to_check$ind2))
  id_most_common <- names(which.max(id_freq))
  ids_to_remove <- c(ids_to_remove, id_most_common)
  # Remove all pairs involving this individual
  pairs_to_check <- pairs_to_check[!(pairs_to_check$ind1 == id_most_common | pairs_to_check$ind2 == id_most_common), ]
}
ids_to_remove <- unique(ids_to_remove)

#Keep only unrelated individuals
all_ids <- unique(c(rel_tab$ind1, rel_tab$ind2))
indep_ids <- setdiff(all_ids, ids_to_remove)

cat("Number of unrelated individuals retained:", length(indep_ids), "\n")

#Filter the original dataset to retain only unrelated individuals
final_data <- data_f[data_f[[1]] %in% indep_ids, ]

#Export to a new CSV file
write.csv(final_data,
          paste0("unrelated_individuals_under_", seuil_relatedness, ".csv"),
          row.names = FALSE)
