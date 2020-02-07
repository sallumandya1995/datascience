 
#install.packages('AER')
library(AER)
 data(Affairs,package="AER")

mydata <- Affairs
View(mydata)
attach(mydata)
summary(mydata)
 colnames(mydata)
 CPaffairs <- NULL
CPaffairs <- ifelse(affairs>0,1,0)
mydata[,"CPaffairs"] <- CPaffairs

CPgender <- NULL
CPgender <- ifelse(gender=="male",1,0)
mydata[,"CPgender"] <- CPgender

CPchildren <- NULL
CPchildren <- ifelse(children=="yes",1,0)
mydata[,"CPchildren"] <- CPchildren

mydata <- mydata[,-c(1,2,5)]

plot(mydata)


cor(mydata)



  library(corpcor)

cor2pcor(cor(mydata))
 model <- glm(CPaffairs ~ CPgender + age + yearsmarried + CPchildren + religiousness + education + occupation + rating, family = "binomial")
summary(model)
 influenceIndexPlot(model)


influencePlot(model)


 model2 <- glm(CPaffairs ~ CPgender + age + yearsmarried + CPchildren + religiousness + education + occupation + rating, family = "binomial", data = mydata[-c(170,405,552,568),])
summary(model2)
 #Identification for Not performed Column
vif(model)
 avPlots(model)


FinalModel <- glm(CPaffairs ~ age + yearsmarried + religiousness + rating, family = "binomial", data= mydata[-c(170,405,552,568),])
summary(FinalModel)

 prob <- predict(FinalModel, mydata, type = "response")
prob <- as.data.frame(prob)

final <- cbind(mydata, prob)

confusion <- table(prob>0.5, CPaffairs)
confusion
 accuracy <- sum(diag(confusion)/sum(confusion))
accuracy

#roc
 library(ROCR)
 rocrpred <- prediction(prob, CPaffairs)
rocrperf <- performance(rocrpred, 'tpr', 'fpr')

str(rocrperf)
 plot(rocrperf,colorize=T, text.adj=c(0.2,-1.7))
 