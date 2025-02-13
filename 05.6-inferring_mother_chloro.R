##############################################################################################
############ Comparing Cervus and Colony results + inferring mother with haplotype ###########
##############################################################################################


library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(utils)

# Load and prepare Cervus data
file_path_cervus <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.2-parentage_with_cervus/5.2.2 Analyses V2/Sparouine/summary_Sparouine.csv"
data_cervus <- read_csv2(file_path_cervus)  # Assuming ';' as delimiter
data_cervus <- data_cervus %>%
  filter(grepl("\\+|\\*", Pair_confidence1) | grepl("\\+|\\*", Pair_confidence2)) %>%
  rename(OffspringID = Offspring_ID) %>%  # Rename to match Colony data
  select(OffspringID, Parent1_Cervus = First_candidate_ID, Parent2_Cervus = Second_candidate_ID)

# Load and prepare Colony data
file_path_colony <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.4-parentage_with_colony/Sparouine/Results_Sparouine/test_on_Sparouine.BestConfig.csv"
data_colony <- read_csv2(file_path_colony) %>%
  filter(!(str_starts(FatherID, "#") & str_starts(MotherID, "#"))) %>%  # Exclude only if both parents are supposed
  select(OffspringID, Parent1_Colony = FatherID, Parent2_Colony = MotherID)

# Merge Cervus and Colony data
comparison_data <- left_join(data_cervus, data_colony, by = "OffspringID")

# Analyze the concordance
comparison_data %>%
  mutate(Parent1_Match = Parent1_Cervus == Parent1_Colony,
         Parent2_Match = Parent2_Cervus == Parent2_Colony) %>%
  summarise(Concordance_Parent1 = sum(Parent1_Match, na.rm = TRUE),
            Concordance_Parent2 = sum(Parent2_Match, na.rm = TRUE),
            Total = n())

# Print the comparison data
print(comparison_data)



# Load haplotype data
file_path_haplotypes <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Data_initial/Chloroplastic_markers/Haplotype.csv"

# Load haplotype data using read.table
data_haplotypes <- read.table(file_path_haplotypes, 
                              sep = ";",               # Set semicolon as the field separator
                              dec = ".",               # Set period as the decimal mark
                              header = TRUE,           # Assumes the first line in your file has column headers
                              stringsAsFactors = FALSE, # Ensures text data isn't converted to factors
                              colClasses = c(ID="character", 
                                             plot="character",
                                             lat="numeric",
                                             long="numeric",
                                             dbh="numeric",
                                             class="character",
                                             Haplotypes="character") # Explicitly define column classes
)

# Merge haplotype data into comparison_data for offspring and both parents
comparison_data <- comparison_data %>%
  left_join(data_haplotypes, by = c("OffspringID" = "ID")) %>%
  left_join(data_haplotypes %>% rename(Haplotypes_Parent1 = Haplotypes), by = c("Parent1_Cervus" = "ID")) %>%
  left_join(data_haplotypes %>% rename(Haplotypes_Parent2 = Haplotypes), by = c("Parent2_Cervus" = "ID"))

# Add columns to indicate haplotype match with each parent
comparison_data <- comparison_data %>%
  mutate(
    Match_Parent1 = ifelse(Haplotypes == Haplotypes_Parent1, "Yes", "No"),
    Match_Parent2 = ifelse(Haplotypes == Haplotypes_Parent2, "Yes", "No")
  )

# Display the results with haplotype match information
print(comparison_data %>% select(OffspringID, Parent1_Cervus, Parent2_Cervus, Match_Parent1, Match_Parent2, Haplotypes, Haplotypes_Parent1, Haplotypes_Parent2))
