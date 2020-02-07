bank=read.csv(choose.files())
bank <- na.omit(bank) # Omit 
dim(bank)
View(bank)
colnames(bank)



mod_lm <- lm(y~.,data=bank)


pred1 <- predict(mod_lm,bank)
pred1
plot(bank$y,pred1)


plot(pred1
     )

model <- glm(y~.,data=bank,family = "binomial")

summary(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))





# Confusion matrix table 
prob <- predict(model,bank,type="response")
summary(model)
View(prob)


confusion<-table(prob>0.5,bank$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 


#calculating ROC

library(ROCR)


rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.0))
