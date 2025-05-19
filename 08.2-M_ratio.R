################################################################################
################################## M ratio #####################################
################################################################################

library(strataG)

# Load your dataset
file_path <- "C:/Users/bonni/OneDrive/University/Thesis/Dicorynia/Article-SSR_population/Data_initial/4sites/nSSR_4sites_data_frame.csv"
data <- read.csv(file_path)

g_data <- df2gtypes(data, 2, id.col = 1, strata.col = 2, loc.col = 3)

m_ratios <- mRatio(g_data, by.strata = TRUE)

print(m_ratios)


