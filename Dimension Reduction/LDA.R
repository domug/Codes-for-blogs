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

## Define train data
train_index = sample(seq_len(nrow(data_full_new)), size = train_size)
variables = data.frame(scale(data_full_new[, 2:13]))
train_variables = variables[train_index, ]
train_labels = data_full_new[train_index, 14]
train = data.frame(train_variables, "COMPANY"=train_labels)

head(train)
dim(train)

## Define test data
test_variables = variables[-train_index, ]
test_labels = data_full_new[-train_index, 14]
test = data.frame(test_variables, "COMPANY"=test_labels)
dim(test)

## Perform LDA
library(MASS)
model = lda(COMPANY ~ ., data=train)
model

## Visualize the result
company = factor(train[, "COMPANY"])
train_pred = predict(model)
ggplot(data.frame(train_pred$x), aes(x=LD1, y=LD2, col=company)) + 
  geom_point() + labs(title="Linear Discriminant Analysis")

## Error rate on train dataset
filter = (train_pred$class != train[, "COMPANY"])
sum(filter)/nrow(train)


## Error rate on test dataset
test_pred = predict(model, test)
filter = (test_pred$class != test[, "COMPANY"])
sum(filter)/nrow(test)
