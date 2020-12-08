# Load dataset
library("readxl")
data_full = data.frame(read_excel("CarData.xls"))

# Remove missing values
data_full_new = na.omit(data_full)

# Define varaibles to be used for PCA
data = data_full_new[,2:13]

# Standardize variables to a single scale
data_scaled = data.frame(scale(data))

##################################
##################################

# Factor Analysis
fa = factanal(data_scaled, factors=4, scores="regression", rotation="none")
fa$loadings

# Factor Analysis with varimax rotation
fa_varimax = factanal(data_scaled, factors=4, scores="regression", rotation="varimax")
fa_varimax$loadings

##################################
##################################

#### Sort dataset by each factors
factor1_sorted = data.frame(sort(fa_varimax$scores[,1], decreasing=T))
factor2_sorted = data.frame(sort(fa_varimax$scores[,2], decreasing=T))
factor3_sorted = data.frame(sort(fa_varimax$scores[,3], decreasing=T))
factor4_sorted = data.frame(sort(fa_varimax$scores[,4], decreasing=T))

# Factor 1: Overal size of car
factor1_df = data.frame(data_full_new[rownames(factor1_sorted), c("CARMARK", "COMPANY")], factor1_sorted)
colnames(factor1_df)[3] = "Factor_Scores"

# Factor 2: Interal Space of car
factor2_df = data.frame(data_full_new[rownames(factor2_sorted), c("CARMARK", "COMPANY")], factor2_sorted)
colnames(factor2_df)[3] = "Factor_Scores"

# Factor 3: After service of car
factor3_df = data.frame(data_full_new[rownames(factor3_sorted), c("CARMARK", "COMPANY")], factor3_sorted)
colnames(factor3_df)[3] = "Factor_Scores"

# Factor 4: Price of car
factor4_df = data.frame(data_full_new[rownames(factor4_sorted), c("CARMARK", "COMPANY")], factor4_sorted)
colnames(factor4_df)[3] = "Factor_Scores"

##################################
##################################
## Print the results

# Cars sorted based on factor 1
head(factor1_df, 10); tail(factor1_df, 10)

# Cars sorted based on factor 2
head(factor2_df, 10); tail(factor2_df, 10)

# Cars sorted based on factor 3
head(factor3_df, 10); tail(factor3_df, 10)

# Cars sorted based on factor 4
head(factor4_df, 10); tail(factor4_df, 10)


