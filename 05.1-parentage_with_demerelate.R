
############## Calculate Relatedness on Diploid Genetic Data with Demerelated ################



install.packages("sfsmisc")
install.packages("mlogit")

library(Demerelate)


### Input file for demerelated 
# col 1 : ID
# col 2 : pop_info (better to only used one pop by analysis)
# col 3 and 4 etc. : 2 allele for 1 marker


#Files paths 
SSR_data <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.1-parentage_with_demerelate/Input_files/nSSR_Nouragues.csv"
dir_result <- "C:/Users/bonni/OneDrive/Université/Thèse/Dicorynia/Article - SSR Populations/Analysis/05-Parentage_analysis/05.1-parentage_with_demerelate/results"

SSR_table <- read.csv(SSR_data, header = T)

# Transform into data frame
SSR_df <- as.data.frame(SSR_table)



#Function to calculate pairwise relatedness within populations based on allele sharing

relat_all_shar <- Emp.calc(SSR_table, value="loiselle", ref.pop="NA")

summary(relat_all_shar)
hist(as.numeric(relat_all_shar), main="Histogramme des coefficients de parenté", xlab="Coefficient de parenté", breaks=20)


# Afficher les paires avec les coefficients de parenté les plus élevés
top_pairs <- sort(relat_all_shar, decreasing=TRUE)[1:10]
print(top_pairs)



# Installer et charger les packages nécessaires
if(!require(igraph)) install.packages("igraph")
library(igraph)

# Convertir les résultats en un data frame d'arêtes
edges <- as.data.frame(do.call(rbind, strsplit(names(relat_all_shar), "_")))
edges$weight <- as.numeric(relat_all_shar)
colnames(edges) <- c("from", "to", "weight")

# Créer un graphe
graph <- graph_from_data_frame(edges, directed=FALSE)

# Visualiser le réseau
plot(graph, edge.width=E(graph)$weight * 10, vertex.label=V(graph)$name, main="Réseau des relations génétiques")





