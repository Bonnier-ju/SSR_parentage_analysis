###############################################################################
#################### Variation of Ne accross time #############################
###############################################################################

library(mcmc)
library(VarEff)
library(readr)
library(dplyr)
library(tidyr)


allele_info <- read_csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.4-VarEff/Allele_information.csv")  
geno <- read_csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.0-filtered_relationship_inds/Paracou/unrelated_PAR_under_0.08.csv")
out_dir <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.4-VarEff"
outfile <- file.path(out_dir, "InputVarEff_PAR.txt")

loci <- geno %>% 
  select(matches("_1$")) %>% 
  names() %>% 
  sub("_1$","",.)  

############### Prepare the infiles ######################

# 5. Ouvrir la connexion en écriture
con <- file(outfile, open = "w")

# 6. Pour chaque locus, générer les deux lignes attendues
for (locus in loci) {
  
  # 6.1 Sous-table des allèles pour ce locus
  ai_loc <- allele_info %>%
    filter(Locus == locus) %>%
    select(AlleleSeqCode, AlleleLength) %>%
    distinct()
  
  # 6.2 – DÉTECTION AUTOMATIQUE DU MOTIF
  longueurs <- sort(unique(ai_loc$AlleleLength))
  diffs_all <- diff(longueurs)
  # choisir comme motif la différence la plus fréquente (mode)
  motif_len <- as.numeric(
    names(sort(table(diffs_all[diffs_all > 0]), decreasing=TRUE)[1])
  )
  
  # 6.3 – CALCUL DU “repeat number” 
  min_len <- min(longueurs)
  ai_loc <- ai_loc %>%
    mutate(
      repeat_num = round((AlleleLength - min_len) / motif_len)
    )
  
  # 6.4 Récupérer les deux colonnes génotype pour ce locus dans 'geno'
  col1 <- paste0(locus, "_1")
  col2 <- paste0(locus, "_2")
  geno_long <- geno %>%
    select(all_of(c(col1, col2))) %>%
    pivot_longer(cols = everything(), values_to = "AlleleSeqCode")
  
  # 6.5 Joindre pour obtenir les repeat_num observés
  geno_repeat <- geno_long %>%
    left_join(ai_loc, by = "AlleleSeqCode")
  
  # 6.6 Construire la grille complète de repeat_num (avec zéros)
  all_repeats <- seq(min(ai_loc$repeat_num), max(ai_loc$repeat_num))
  grid <- tibble(repeat_num = all_repeats)
  
  # 6.7 Compter les occurrences de chaque repeat_num
  freq_tbl <- geno_repeat %>%
    count(repeat_num) %>%
    right_join(grid, by = "repeat_num") %>%
    arrange(repeat_num) %>%
    replace_na(list(n = 0))
  
  # 6.8 Écrire dans le fichier :
  #     • ligne 1 = nombre de positions alléliques
  #     • ligne 2 = vecteur des counts séparés par espaces
  n_alleles <- nrow(freq_tbl)
  counts    <- freq_tbl$n
  
  writeLines(as.character(n_alleles), con)
  writeLines(paste(counts, collapse = " "), con)
  
  # (optionnel) afficher un message pour suivre l’avancement
  message(sprintf("– Locus %s : %d positions, written.", locus, n_alleles))
}

# 7. Fermer la connexion
close(con)
message("→ Infile VarEff écrit dans : ", outfile)


################## Running VarEff ######################

# ====================================================================================
# Preliminary Θ estimation with Theta()
#    — gives Θ₀, Θ₁, Θ₂, imbalance indices, MinNe/MaxNe suggestions :contentReference[oaicite:3]{index=3}
# ====================================================================================
# 3.1 Set “quick” MCMC controls for a fast run
INFILE      <-"C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.4-VarEff/InputVarEff_SPR.txt"
NBLOC       <- 66      # number of markers
MUTAT       <- 0.001             # assumed per‐locus mutation rate
JMAX        <- 3                 # number of Ne‐change events to model
MODEL       <- "S"               # Single‐Step mutation model
NBAR        <- 1000              # rough prior mean for Θ = 4·Ne·u
VARP1       <- 3                 # variance of log‐Ne prior
RHOCORN     <- 0                 # no auto‐correlation in sizes
GBAR        <- 5000              # max generations back
VARP2       <- 3                 # variance of log‐time prior
DMAXPLUS    <- max(diff(sort(unique(allele_info$AlleleLength)))) + 1
Diagonale   <- 0.5               # covariance smoothing
NumberBatch <- 1000              # small for speed
LengthBatch <- 5
SpaceBatch  <- 5

Theta(
  infile      = INFILE,
  parafile    = "theta_prel",
  NBLOC       = NBLOC,
  MUTAT       = MUTAT,
  JMAX        = JMAX,
  MODEL       = MODEL,
  NBAR        = NBAR,
  VARP1       = VARP1,
  RHOCORN     = RHOCORN,
  GBAR        = GBAR,
  VARP2       = VARP2,
  DMAXPLUS    = DMAXPLUS,
  Diagonale   = Diagonale,
  NumberBatch = NumberBatch,
  LengthBatch = LengthBatch,
  SpaceBatch  = SpaceBatch
)
message("✔ Preliminary Theta run complete; see ‘theta_prel.Theta’")

# ====================================================================================
# Read Theta output & choose data‐driven priors
# ====================================================================================
theta_vals <- scan("theta_prel.Theta", what = numeric(), nmax = 3)
Ne0_est     <- theta_vals[1] / (4 * MUTAT)   # present Ne
Ne2_est     <- theta_vals[3] / (4 * MUTAT)   # ancestral Ne
message(sprintf(
  "Suggested Ne range: %d – %d (from Θ₀, Θ₂)", 
  floor(Ne0_est), ceiling(Ne2_est)
))

# ====================================================================================
# 5. Full VarEff run using data‐driven priors
# ====================================================================================
# Choose NBAR between floor(Ne0_est) and ceiling(Ne2_est), e.g.:
NBAR_full <- round(mean(c(Ne0_est, Ne2_est)))

result <- VarEff(
  infile       = INFILE,
  parafile     = "final_job",
  NBLOC        = NBLOC,
  JMAX         = JMAX,
  MODEL        = MODEL,
  MUTAT        = MUTAT,
  NBAR         = NBAR_full,    # data‐driven prior for 4·Ne·u
  VARP1        = VARP1,
  RHOCORN      = RHOCORN,
  GBAR         = GBAR,
  VARP2        = VARP2,
  DMAXPLUS     = DMAXPLUS,
  Diagonale    = Diagonale,
  NumberBatch  = 10000,        # full‐scale MCMC
  LengthBatch  = 10,
  SpaceBatch   = 10
)
message("✔ Full VarEff run complete; output prefix ‘final_job’")

# ====================================================================================
# 6. Post‐processing: Ne through time
# ====================================================================================
# 6.1 Summary statistics at 10 time‐points up to 200 generations ago
NatSizeDist(
  NameBATCH = "final_job.Batch",
  MUTAT     = MUTAT,
  TMAX      = 200,
  NBT       = 10
)

# 6.2 2D contour plot of log‐Ne through time
NTdist(
  NameBATCH = "final_job.Batch",
  MUTAT     = MUTAT,
  TMAX      = 200
)

# 6.3 Detailed density plots at a few time‐points
plotNdistrib(
  infile  = "final_job.Ndist",
  nbcases = 5
)









