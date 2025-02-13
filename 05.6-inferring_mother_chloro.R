##############################################################################################
############ Comparing Cervus and Colony results + inferring mother with haplotype ###########
##############################################################################################


library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(utils)
library(geosphere)

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

print(comparison_data)

# Clean up the comparison_data by properly structuring the information for offspring, Parent1, and Parent2
comparison_data <- comparison_data %>%
  select(
    # Offspring Information
    OffspringID,
    Plot_Offspring = plot.x,
    Latitude_Offspring = lat.x,
    Longitude_Offspring = long.x,
    DBH_Offspring = dbh.x,
    Class_Offspring = class.x,
    Haplotypes_Offspring = Haplotypes,
    
    # Parent 1 Information
    Parent1_Cervus,
    Plot_Parent1 = plot.y,
    Latitude_Parent1 = lat.y,
    Longitude_Parent1 = long.y,
    DBH_Parent1 = dbh.y,
    Class_Parent1 = class.y,
    Haplotypes_Parent1,
    
    # Parent 2 Information
    Parent2_Cervus,
    Plot_Parent2 = plot,
    Latitude_Parent2 = lat,
    Longitude_Parent2 = long,
    DBH_Parent2 = dbh,
    Class_Parent2 = class,
    Haplotypes_Parent2,
    
    # Haplotype Matching Results
    Match_Parent1,
    Match_Parent2
  ) %>%
  distinct()  # Remove any duplicate rows if they exist

# Print the cleaned and structured data
print(comparison_data)


# Create a new dataset where exactly one parent matches the offspring's haplotype
filtered_data <- comparison_data %>%
  filter((Match_Parent1 == "Yes" & Match_Parent2 == "No") | 
           (Match_Parent1 == "No" & Match_Parent2 == "Yes")) %>%
  
  # Assign mother and father based on haplotype match
  mutate(
    Mother_ID = ifelse(Match_Parent1 == "Yes", Parent1_Cervus, Parent2_Cervus),
    Father_ID = ifelse(Match_Parent1 == "No", Parent1_Cervus, Parent2_Cervus)
  )


# Print the filtered dataset with maternal and paternal assignments
print(filtered_data)






# Function to calculate Haversine distance (in meters)
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  if (is.na(lat1) | is.na(lon1) | is.na(lat2) | is.na(lon2)) {
    return(NA)  # Return NA if any coordinate is missing
  }
  return(distHaversine(c(lon1, lat1), c(lon2, lat2)))  # Returns distance in meters
}

# Compute the correct latitude and longitude for the mother and father
filtered_data <- filtered_data %>%
  mutate(
    # Assign correct coordinates for Mother
    Latitude_Mother = ifelse(Mother_ID == Parent1_Cervus, Latitude_Parent1, Latitude_Parent2),
    Longitude_Mother = ifelse(Mother_ID == Parent1_Cervus, Longitude_Parent1, Longitude_Parent2),
    
    # Assign correct coordinates for Father
    Latitude_Father = ifelse(Father_ID == Parent1_Cervus, Latitude_Parent1, Latitude_Parent2),
    Longitude_Father = ifelse(Father_ID == Parent1_Cervus, Longitude_Parent1, Longitude_Parent2)
  ) %>%
  
  # Calculate distances
  rowwise() %>%
  mutate(
    Distance_To_Mother = calculate_distance(Latitude_Offspring, Longitude_Offspring, Latitude_Mother, Longitude_Mother),
    Distance_To_Father = calculate_distance(Latitude_Offspring, Longitude_Offspring, Latitude_Father, Longitude_Father)
  ) %>%
  ungroup()

# Compute the mean distance separately for mothers and fathers
mean_distance_mother <- mean(filtered_data$Distance_To_Mother, na.rm = TRUE)
mean_distance_father <- mean(filtered_data$Distance_To_Father, na.rm = TRUE)

# Print results
print(paste("Mean Distance to Mothers:", round(mean_distance_mother, 2), "meters"))
print(paste("Mean Distance to Fathers:", round(mean_distance_father, 2), "meters"))

# Optionally, save the dataset for further analysis
# write.csv(filtered_data, "Filtered_Parentage_With_Distances.csv", row.names = FALSE)

