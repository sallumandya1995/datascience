#Read data file
  data("iris")
 
#Decision tree with party
library(party)
   
mytree <- ctree(Species~., data = iris, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(mytree)
plot(mytree,type="simple")

#Misclassification error
tab<-table(predict(mytree), iris$Species)
print(tab)
1-sum(diag(tab))/sum(tab)

