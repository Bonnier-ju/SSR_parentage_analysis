##################################################
########### Genotype accumulation curve ##########
##################################################

library(adegenet)
library(dplyr)
library(tidyr)

################## Pre-processing of csv file ######################
####################################################################

# Read the CSV file (Replace 'my_file.csv' with the actual filename)
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

print(genind_obj)
genind_obj@pop

################# Plotting of the curves ##############
#######################################################












