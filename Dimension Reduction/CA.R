library("readxl")
data_full = data.frame(read_excel("CarData.xls"))

# Remove missing values
data_full_new = na.omit(data_full)

# Define varaibles to be used for PCA
data = data_full_new[,2:13]

# Standardize variables to a single scale
data_scaled = data.frame(scale(data))

## K-means Cluster
library(factoextra)

# Calculate distance matrix based on Euclidean distance
distance = get_dist(data_scaled, method="euclidean")

# Visualize distance matrix
fviz_dist(distance, gradient = list(low = "skyblue", mid = "white", high = "red"))

## Define optimal number of clusters
par(mfrow=c(1,2))
fviz_nbclust(data_scaled, kmeans, method = "wss") + labs(title="Elbow Method")
fviz_nbclust(data_scaled, kmeans, method = "silhouette") + labs(title="Silhouette Method")

## K-means Clustering
ca <- kmeans(data_scaled, centers = 3, nstart = 25)

# Visulaize the clusters
company = factor(data_full_new[,14])
fviz_cluster(ca, data = data_scaled, repel=TRUE) + geom_point(aes(color=company)) + 
  labs(title="K-means Clustering for k=3",  x="PC1 (57.5%)", y="PC2 (14.2%)")

# cars within the first cluster
unlist(data_full_new[ca$cluster == 1,]["CARMARK"])

# cars within the second cluster
unlist(data_full_new[ca$cluster == 2,]["CARMARK"])

# cars within the third cluster
unlist(data_full_new[ca$cluster == 3,]["CARMARK"])

