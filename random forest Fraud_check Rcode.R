library(randomForest)
library(caret)
library(mass)
#using set.seed to get same result in each iteration

set.seed(123)


FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)


hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","yellow"))


Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky otherwise  Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)


table(FC$Risky_Good)   # 476 good customers and 124 risky customers
 
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf  


# Out of bag estimate of error rate is 0.47 % in Random Forest Model.
attributes(rf)


# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1) 
head(train$Risky_Good)

 
confusionMatrix(pred1, train$Risky_Good)    

# Prediction with  Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) 


# Error Rate in Random Forest Model :
window(  )
plot(rf)

# at 200 there is a constant line and it doesnot vary after 200 trees

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)


rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1



pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good)



# no of nodes of trees

hist(treesize(rf1), main = "number of Nodes for the trees", col = "red")



 
# Variable Importance :

varImpPlot(rf1)


varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

# Quantitative values 
importance(rf1)





varUsed(rf)   # which predictor variables are actually used in the random forest.
  
partialPlot(rf1, train, Taxable.Income, "Good")


# On that graph, i see that if the taxable Income is 30000 or greater,
# than they are good customers else those are risky customers.
# Extract single tree from the forest :

tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)

