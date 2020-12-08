#########################################
# Linear Discriminant Analysis

library("readxl")
data_full = data.frame(read_excel("CarData.xls"))

# Remove missing values
data_full_new = na.omit(data_full)

# Define varaibles to be used for PCA
data = data_full_new[,2:13]

# Standardize variables to a single scale
data_scaled = data.frame(scale(data))

# LDA
library(MASS)
data = data_full_new[, 2:14]
model = lda(COMPANY ~ ., data=data)
model

# Visualize the result
company = factor(data[, "COMPANY"])
prediction = predict(model)
prediction$x
ggplot(data.frame(prediction$x), aes(x=LD1, y=LD2, col=company)) + 
  geom_point() + labs(title="Linear Discriminant Analysis")

# Calculate Error Rate
filter = (prediction$class != data[, "COMPANY"])
sum(filter)/nrow(data)



#########################################
## When using train/test data
#########################################
set.seed(7)

## train test split by 80% : 20%
train_size = floor(0.8 * nrow(data_full_new))

## set the seed to make your partition reproducible
train_index = sample(seq_len(nrow(data_full_new)), size = train_size)

indep_variables = data.frame(scale(data_full_new[, 2:13]))


