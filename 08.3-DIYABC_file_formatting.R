################################################################################
####################### DIYABC file formatting #################################
################################################################################

library(tidyverse)
library(stringr)

# === INPUT FILES ===
genofile <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Data_initial/4sites/nSSR_4sites_data_frame.csv"
seqfile <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.3-DIYABC/Input_files/Alleles_code.csv"

# === Load data ===
geno <- read.csv(genofile, check.names = FALSE)
seqmap <- read.csv(seqfile, check.names = FALSE)

# === Identifier les colonnes génotypiques ===
allele_cols <- names(geno)[grepl("_\\d$", names(geno))]
locus_names <- unique(gsub("_\\d$", "", allele_cols))

# === Construction du format GENEPOP ===
geno_genepop <- geno %>%
  select(plot, ID, all_of(allele_cols)) %>%
  mutate(across(all_of(allele_cols), ~ifelse(is.na(.), NA, as.character(.)))) %>%
  pivot_longer(cols = all_of(allele_cols),
               names_to = c("Locus", "Allele"),
               names_pattern = "(.*)_(\\d)",
               values_to = "Code") %>%
  pivot_wider(names_from = "Allele", values_from = "Code") %>%
  mutate(Genepop = ifelse(is.na(`1`) | is.na(`2`), "000000", paste0(`1`, `2`))) %>%
  select(plot, ID, Locus, Genepop) %>%
  pivot_wider(names_from = "Locus", values_from = "Genepop")

# === Préparation des séquences ===
seqmap_clean <- seqmap %>%
  rename(Locus = Locus, Code = AlleleSeqCode, Sequence = AlleleSequence) %>%
  mutate(Code = as.character(Code))

geno_seq_long <- geno %>%
  select(plot, ID, all_of(allele_cols)) %>%
  pivot_longer(cols = all_of(allele_cols),
               names_to = c("Locus", "Allele"),
               names_pattern = "(.*)_(\\d)",
               values_to = "Code") %>%
  pivot_wider(names_from = "Allele", values_from = "Code") %>%
  mutate(across(c(`1`, `2`), as.character))

geno_seq_mapped <- geno_seq_long %>%
  left_join(seqmap_clean, by = c("Locus", "1" = "Code")) %>%
  rename(Seq1 = Sequence) %>%
  left_join(seqmap_clean, by = c("Locus", "2" = "Code")) %>%
  rename(Seq2 = Sequence)

max_lens <- geno_seq_mapped %>%
  pivot_longer(cols = c(Seq1, Seq2), names_to = "Allele", values_to = "Sequence") %>%
  group_by(Locus) %>%
  summarise(MaxLength = max(nchar(Sequence), na.rm = TRUE), .groups = "drop")

pad_seq <- function(seq, maxlen) {
  if (is.na(seq)) return(NA)
  pad <- maxlen - nchar(seq)
  paste0(seq, str_dup("N", pad))
}

geno_seq_final <- geno_seq_mapped %>%
  left_join(max_lens, by = "Locus") %>%
  mutate(Seq1_padded = mapply(pad_seq, Seq1, MaxLength),
         Seq2_padded = mapply(pad_seq, Seq2, MaxLength),
         DIYABC_seq = ifelse(is.na(Seq1_padded) | is.na(Seq2_padded),
                             "0",
                             paste0("<[", Seq1_padded, "][", Seq2_padded, "]>"))) %>%
  select(plot, ID, Locus, DIYABC_seq) %>%
  pivot_wider(names_from = "Locus", values_from = "DIYABC_seq",
              names_prefix = "SEQ_")

# Export Genepop-like data
write.csv(geno_genepop,
          "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.3-DIYABC/Input_files/genepop_format.csv",
          row.names = FALSE)


# Export DIYABC-like sequence data
write.csv(geno_seq_final,
          "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.3-DIYABC/Input_files/diyabc_format.csv",
          row.names = FALSE)



