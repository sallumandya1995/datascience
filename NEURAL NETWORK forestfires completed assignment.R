data=read.csv(choose.files())
str(data)
data=data[3:10]
 data$FFMC <- (data$FFMC - min(data$FFMC))/(max(data$FFMC) - min(data$FFMC))
data$DMC <- (data$DMC - min(data$DMC))/(max(data$DMC) - min(data$DMC))
data$DC <- (data$DC - min(data$DC))/(max(data$DC) - min(data$DC))
data$ISI <- (data$ISI - min(data$ISI))/(max(data$ISI) - min(data$ISI))
data$temp <- (data$temp - min(data$temp))/(max(data$temp) - min(data$temp))
data$RH <- (data$RH - min(data$RH))/(max(data$RH) - min(data$RH))
data$wind <- (data$wind - min(data$wind))/(max(data$wind) - min(data$wind))
 


# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]
# Neural Networks
library(neuralnet)
set.seed(333)

n <- neuralnet(rain~.,
               data = training,
               hidden = 1,
               linear.output = FALSE)
plot(n)


 
