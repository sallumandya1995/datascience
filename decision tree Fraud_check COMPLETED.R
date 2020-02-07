mydata <- read.csv(choose.files())
colnames(mydata)


TIRisky <- NULL
TIRisky <- ifelse(mydata$Taxable.Income<=30000,"risky","good")
mydata[,"TIRisky"] <- TIRisky
 str(mydata)
mydata$Undergrad <- as.factor(mydata$Undergrad)
mydata$Marital.Status <- as.factor(mydata$Marital.Status)
mydata$Urban <- as.factor(mydata$Urban)
mydata$TIRisky <- as.factor(mydata$TIRisky)




fraud_risky <- mydata[mydata$TIRisky == "risky",] 
fraud_not_risky <- mydata[mydata$TIRisky == "good",]

data_train <- rbind(fraud_risky[1:93,], fraud_not_risky[1:357,])
data_test <- rbind(fraud_risky[94:124,], fraud_not_risky[357:476,])

trained_model <- C5.0(data_train[,-c(7)], data_train$TIRisky)
plot(trained_model)

mean(data_train$TIRisky == predict(trained_model, data_train))
## [1] 1

pred_test <- predict(trained_model, newdata = data_test)

mean(pred_test == data_test$TIRisky)
library(gmodels)
CrossTable(data_test$TIRisky, pred_test)














 