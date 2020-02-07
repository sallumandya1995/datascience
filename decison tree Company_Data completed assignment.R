mydata=read.csv(choose.files())
str(mydata)
View(mydata)
mydata$Sales=as.factor(mydata$Sales)
library(party)
mytree <- ctree(Sales~., mydata, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(mytree)
plot(mytree,type="simple")

#Misclassification error
tab<-table(predict(mytree), mydata$Sales)
print(tab)
1-sum(diag(tab))/sum(tab)
