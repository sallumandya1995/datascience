getwd()
data <- read.csv(choose.files())
str(data)
data$cement <- (data$cement - min(data$cement))/(max(data$cement) - min(data$cement))
data$slag <- (data$slag - min(data$slag))/(max(data$slag) - min(data$slag))
data$ash <- (data$ash - min(data$ash))/(max(data$ash) - min(data$ash))
data$water <- (data$water - min(data$water))/(max(data$water) - min(data$water))
data$superplastic <- (data$superplastic - min(data$superplastic))/(max(data$superplastic) - min(data$superplastic))
data$coarseagg <- (data$coarseagg - min(data$coarseagg))/(max(data$coarseagg) - min(data$coarseagg))
data$fineagg <- (data$fineagg - min(data$fineagg))/(max(data$fineagg) - min(data$fineagg))
data$age <- (data$age - min(data$age))/(max(data$age) - min(data$age))


set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]



#neural network

library(neuralnet)
set.seed(333)
n <- neuralnet(strength~.,
               data = training,
               hidden = 1,
                
               linear.output = FALSE)
plot(n)




