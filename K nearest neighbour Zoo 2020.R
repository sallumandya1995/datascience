
#Zoo Animals
 
dataset <- read.csv(choose.files())
str(dataset)
 
dim(dataset)
# 
dataset <- dataset[-1]
dim(dataset)
# 
table(dataset$type)
## 
# 
round(prop.table(table(dataset$type))*100, digits = 1)
 
summary(dataset[c("feathers","aquatic","legs")]) 
normalize_data <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

dataset_n <- as.data.frame(lapply(dataset[1:16], normalize_data))
summary(dataset_n[c("feathers","aquatic","legs")]) 
dataset_train <- dataset_n[1:80,]
dataset_test <- dataset_n[81:101,]

dataset_train_labels <- dataset[1:80,17]

dataset_test_labels <- dataset[81:101,17]
# EDA part ends  

library(class)

dataset_pred <- knn(train = dataset_train, test = dataset_test, cl = dataset_train_labels, k=5)
 
dataset_pred <- knn(train = dataset_train, test = dataset_test, cl = dataset_train_labels, k=2)
 
library(gmodels)
 CrossTable(dataset_test_labels, dataset_pred)
 # Out of 21 Records, Model got 17 Correct Predictions and 4 Wrong Prediction
 # Model is 81% Accurate 
 