###############################################################################
#################### Variation of Ne accross time #############################
###############################################################################

library(mcmc)
library(VarEff)
library(readr)
library(dplyr)
library(tidyr)

data(InputTest)


allele_info <- read_csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.4-VarEff/Allele_information.csv")  
geno <- read_csv("C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Analysis/08-past_demographic_history/08.4-VarEff/Sparouine_full_data.csv")

# extract locus names
loci <- geno %>% 
  select(matches("_1$")) %>% 
  names() %>% 
  sub("_1$","",.)  
