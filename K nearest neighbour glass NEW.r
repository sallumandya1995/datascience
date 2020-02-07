library(caret)
library(pROC)
library(mlbench)
library(lattice)

 glass <- read.csv(file.choose())
glass$Type[glass$Type==1] <- 'Type1'
glass$Type[glass$Type==2] <- 'Type2'
glass$Type[glass$Type==3] <- 'Type3'
glass$Type[glass$Type==4] <- 'Type4'
glass$Type[glass$Type==5] <- 'Type5'
glass$Type[glass$Type==6] <- 'Type6'
glass$Type[glass$Type==7] <- 'Type7'
str(glass)



glass$Type <- as.factor(glass$Type) # Factorize the Type in Glass dataset
View(glass)

# Data partition
set.seed(123)
ind <- sample(2,nrow(glass), replace = T, prob = c(0.7,0.3))
train <- glass[ind==1,]
test <- glass[ind==2,]

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
fit <- train(Type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"))
# default metric is accuracy but if u want to use ROC, then mention the same
# Model Performance :
fit # the optimum value for k should be 9


plot(fit)


varImp(fit)



pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$Type)













