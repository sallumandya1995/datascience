library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)
library(psych)
trainsal=read.file.csv()

str(trainsal)
View(trainsal)
trainsal$educationno=as.factor(trainsal$educationno)
class(trainsal$educationno)

testsal=read.file.csv()
str(testsal)
testsal$educationno=as.factor(testsal$educationno)
class(testsal$educationno)


# Plot and ggplot 
ggplot(data=trainsal,aes(x=trainsal$Salary, y = trainsal$age, fill = trainsal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


plot(trainsal$workclass,trainsal$Salary)

plot(trainsal$education,trainsal$Salary)
plot(trainsal$educationno,trainsal$Salary)
plot(trainsal$maritalstatus,trainsal$Salary)
plot(trainsal$occupation,trainsal$Salary)
plot(trainsal$relationship,trainsal$Salary)
plot(trainsal$race,trainsal$Salary)
plot(trainsal$sex,trainsal$Salary)


ggplot(data=trainsal,aes(x=trainsal$Salary, y = trainsal$capitalgain, fill = trainsal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=trainsal,aes(x=trainsal$Salary, y = trainsal$capitalloss, fill = trainsal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=trainsal,aes(x=trainsal$Salary, y = trainsal$hoursperweek, fill = trainsal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(trainsal$native,trainsal$Salary)

#Density Plot 

ggplot(data=trainsal,aes(x = trainsal$age, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'black')

ggtitle("Age - Density Plot")

ggplot(data=trainsal,aes(x = trainsal$workclass, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("Workclass Density Plot")

ggplot(data=trainsal,aes(x = trainsal$education, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'red')

ggtitle("education Density Plot")

ggplot(data=trainsal,aes(x = trainsal$educationno, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'yellow')

ggtitle("educationno Density Plot")




ggplot(data=trainsal,aes(x = trainsal$maritalstatus, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("maritalstatus Density Plot")


ggplot(data=trainsal,aes(x = trainsal$occupation, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("occupation Density Plot")

ggplot(data=trainsal,aes(x = trainsal$sex, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("sex Density Plot")

ggplot(data=trainsal,aes(x = trainsal$relationship, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Relationship Density Plot")

ggplot(data=trainsal,aes(x = trainsal$race, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')



ggtitle("Race Density Plot")
 
ggplot(data=trainsal,aes(x = trainsal$capitalgain, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Capitalgain Density Plot")
 
ggplot(data=trainsal,aes(x = trainsal$capitalloss, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Capitalloss Density Plot")
 
ggplot(data=trainsal,aes(x = trainsal$hoursperweek, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Hoursperweek Density Plot")
 
ggplot(data=trainsal,aes(x = trainsal$native, fill = trainsal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("native Density Plot")



  # Naive Bayes Model 
  Model <- naiveBayes(trainsal$Salary ~ ., data = trainsal)
  Model

  
  Model_pred <- predict(Model,testsal)
  mean(Model_pred==testsal$Salary)

Model_pred


confusionMatrix(Model_pred,testsal$Salary)



