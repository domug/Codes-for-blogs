# Import dataset
library("readxl")
data_full = data.frame(read_excel("CarData.xls"))
dim(data_full)
head(data_full)

# Remove missing values
num_nas = c()
for(i in 1:length(data_full)){
  num_nas = c(num_nas, sum(is.na(data_full[,i])))
}
num_nas

data_full_new = na.omit(data_full)

# Variables to be used for PCA
data = data_full_new[,2:13]
head(data)
dim(data)

# Standardize variables
data_scaled = data.frame(scale(data))
head(data_scaled)

# Perform PCA
pca = prcomp(data_scaled)
summary(pca)

# Scree Plot 
plot(pca, type="l", sub="Scree Plot")
variances = pca$sdev^2
sum(variances[1:2])/sum(variances)

# Biplot
pc1_values = pca$x[, 1]
pc2_values = pca$x[, 2]
myfun = function(ele){
  if(ele == 1){
    return("blue")
  } else if (ele == 2){
    return("red")
  } else {
    return("green")
  }
}
palette = unlist(lapply(data_full_new[,14], myfun))


biplot(pca, main="Biplot of PCA on car dataset")
text(pc1_values, pc2_values, labels=data_full_new[, 14], col=palette)
abline(h=0)
abline(v=0)

# PC Loadings
pca$rotation[, 1:2]

