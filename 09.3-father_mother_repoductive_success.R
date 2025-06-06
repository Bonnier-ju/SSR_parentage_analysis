###############################################################################
################ Father/mother reproductive success ###########################
###############################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tools)
library(tidyr)

plot_code <- "REG"  

# Automatic variables from that
plot_names <- c("NOU" = "Nouragues", "PAR" = "Paracou", "SPR" = "Sparouine", "REG" = "Regina")
plot_colors <- c("Nouragues" = "#548B54", "Paracou" = "chocolate3", 
                 "Sparouine" = "aquamarine3", "Regina" = "mediumorchid4")

plot_name <- plot_names[[plot_code]]
plot_color <- plot_colors[[plot_name]]

input_file <- paste0("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/",
                     "Analysis/05-Parentage_analysis/05.6-parentage_with_haplotype/",
                     "filtered_data_", plot_code, ".csv")


# Define output paths
output_dir <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/09-reproductive_sucess/mother_father_success"
output_csv <- file.path(output_dir, paste0("repro_success_by_role_", plot_name, ".csv"))
output_plot <- file.path(output_dir, paste0("repro_success_by_role_", plot_name, ".png"))

# Read data
data <- read_csv(input_file)

# Count number of times each ID is assigned as mother or father
mother_counts <- data %>%
  filter(!is.na(Mother_ID) & Mother_ID != "") %>%
  group_by(Mother_ID) %>%
  summarise(N_Mother = n(), .groups = "drop") %>%
  rename(ID = Mother_ID)

father_counts <- data %>%
  filter(!is.na(Father_ID) & Father_ID != "") %>%
  group_by(Father_ID) %>%
  summarise(N_Father = n(), .groups = "drop") %>%
  rename(ID = Father_ID)

# Combine the two
repro_summary <- full_join(mother_counts, father_counts, by = "ID") %>%
  mutate(N_Mother = replace_na(N_Mother, 0),
         N_Father = replace_na(N_Father, 0),
         Total = N_Mother + N_Father)

# Save results
write_csv(repro_summary, output_csv)


# Barplot
repro_barplot <- ggplot(repro_long, aes(x = ID, y = Offspring_Count, fill = Role)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("N_Mother" = "#d95f02", "N_Father" = "#1b9e77")) +
  labs(title = paste0("Reproductive Success by Role – ", plot_name),
       x = "Parent ID", y = "Number of Offspring", fill = "Role") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

repro_barplot

# Save plot
ggsave(file.path(output_dir, paste0("barplot_reproductive_success_", plot_name, ".png")),
       plot = repro_barplot, width = 10, height = 6, dpi = 400, bg = "white")
