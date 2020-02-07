data=read.csv(choose.files())
str(data)
data=data[-4]
data$R.D.Spend <- (data$R.D.Spend - min(data$R.D.Spend))/(max(data$R.D.Spend) - min(data$R.D.Spend))
data$Administration <- (data$Administration - min(data$Administration))/(max(data$Administration) - min(data$Administration))
data$Marketing.Spend <- (data$Marketing.Spend - min(data$Marketing.Spend))/(max(data$Marketing.Spend)-min(data$Marketing.Spend))
da


# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]
# Neural Networks
library(neuralnet)
set.seed(333)
 


n <- neuralnet(Profit~.,
               data = training,
               hidden = 1,
               linear.output = FALSE)


plot(n)




 








































# Prediction
output <- compute(n, training[,-1])
head(output$net.result)
head(training[1,])

# Node Output Calculations with Sigmoid Activation Function
in4 <- 0.0455 + (0.82344*0.7586206897) + (1.35186*0.8103448276) + (-0.87435*0.6666666667)
out4 <- 1/(1+exp(-in4))
in5 <- -7.06125 +(8.5741*out4)
out5 <- 1/(1+exp(-in5))

# Confusion Matrix & Misclassification Error - training data
output <- compute(n, training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, training$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
 
