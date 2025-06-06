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



# Add +1 to avoid division by zero
repro_summary <- repro_summary %>%
  mutate(
    Role_Bias = (N_Mother + 1) / (N_Father + 1),
    Role_Diff = N_Mother - N_Father
  )

hist_ratio <- ggplot(repro_summary, aes(x = log10(Role_Bias))) +
  geom_histogram(bins = 30, fill = plot_color, color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = paste0("Distribution of log(Mother/Father) Reproductive Bias – ", plot_name),
    x = "log10(N_Mother + 1 / N_Father + 1)", y = "Number of Parents"
  ) +
  theme_minimal()

ggsave(file.path(output_dir, paste0("role_bias_log_histogram_", plot_name, ".png")),
       plot = hist_ratio, width = 8, height = 6, dpi = 400, bg = "white")

# Wilcoxon signed-rank test
wilcox_test <- wilcox.test(repro_summary$N_Mother, repro_summary$N_Father, paired = TRUE)
print(wilcox_test)


# ---- DBH comparison between roles ----

# Create dataframes for DBH of mothers and fathers
dbh_mother <- data %>%
  filter(!is.na(Mother_ID) & Mother_ID != "") %>%
  select(ID = Mother_ID, DBH = DBH_Parent2) %>%
  mutate(Role = "Mother")

dbh_father <- data %>%
  filter(!is.na(Father_ID) & Father_ID != "") %>%
  select(ID = Father_ID, DBH = DBH_Parent1) %>%
  mutate(Role = "Father")

# Combine both
dbh_combined <- bind_rows(dbh_mother, dbh_father) %>%
  filter(!is.na(DBH)) %>%
  distinct(ID, Role, DBH)

# Boxplot
dbh_boxplot <- ggplot(dbh_combined, aes(x = Role, y = DBH, fill = Role)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Mother" = "#d95f02", "Father" = "#1b9e77")) +
  labs(title = paste0("DBH of Parents by Role – ", plot_name),
       x = "Reproductive Role", y = "DBH (cm)") +
  theme_minimal()
dbh_boxplot 
# Save boxplot
ggsave(file.path(output_dir, paste0("dbh_comparison_by_role_", plot_name, ".png")),
       plot = dbh_boxplot, width = 8, height = 6, dpi = 400, bg = "white")

# Wilcoxon test on DBH by role
wilcox_dbh <- wilcox.test(DBH ~ Role, data = dbh_combined)
print(wilcox_dbh)


# ---- Scatterplot: DBH vs. number of offspring (by role) ----

# Merge DBH with reproductive success
dbh_success <- dbh_combined %>%
  left_join(repro_summary, by = c("ID")) %>%
  mutate(N = ifelse(Role == "Mother", N_Mother, N_Father))

# Scatterplot
dbh_scatter <- ggplot(dbh_success, aes(x = DBH, y = N, color = Role)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Mother" = "#d95f02", "Father" = "#1b9e77")) +
  labs(title = paste0("Reproductive Success vs. DBH – ", plot_name),
       x = "DBH (cm)", y = "Number of Offspring") +
  theme_minimal()

# Save scatterplot
ggsave(file.path(output_dir, paste0("scatter_repro_vs_dbh_", plot_name, ".png")),
       plot = dbh_scatter, width = 8, height = 6, dpi = 400, bg = "white")

# Correlation test (optional, by role)
cor_test_mother <- cor.test(
  dbh_success$DBH[dbh_success$Role == "Mother"],
  dbh_success$N[dbh_success$Role == "Mother"]
)

cor_test_father <- cor.test(
  dbh_success$DBH[dbh_success$Role == "Father"],
  dbh_success$N[dbh_success$Role == "Father"]
)

print(cor_test_mother)
print(cor_test_father)













