wine <- read.csv(file.choose())
View(wine)
str(wine)
summary(wine)
View(wine[-1])
cor(wine)


install.packages("psych")
library(psych)

pairs.panels(wine, gap = 0, bg = c("red","green","yellow"), pch = 21)

PC <- prcomp(wine, center = TRUE, scale. = TRUE)

attributes(pc)

pc$center

pc$scale

mean(wine$Malic)

print(pc)

pairs.panels(pc$x, gap=0, bg = c("red","yellow","blue")[wine$Alcohol], pch = 21)

loadings(pc)
plot(pc)
screeplot(pc, type = "line", main = "Scree Plot")

biplot(pc)


pc$scores
pc$scores[1:14]


mydata <- cbind(data,pc$scores[,1:14])
clus_data <- mydata[8:10]
View(mydata)
normalized_data <- scale(clus_data)
d <- dist(normalized_data,method = "euclidean")
fit <- hclust(d,method = "complete")

prd <- predict(pc, wine)
prd

library(nnet)
wine$Alcohol<- relevel(wine$Alcohol)
wine <- multinom(Alcohol~., data = wine)

plot(fit)
plot(fit, hang=-1)
group <- cutree(fit,k=10)
rect.hclust(fit,k=10,border="red")
membership <- as.matrix(group)
final <- data.frame(data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

fa1 <- factanal(wine, factors = 3)
fa1

fa2 <- factanal(wine, factors = 3, rotation = "varimax")
fa2

fa3 <- factanal(wine, factors = 3, rotation = "varimax", scores = "regression")
fa3




#############kmeans#######################

x <- runif(50)
y <- runif(50)
data <- cbind(x,y)
View(data)
library(plyr)
plot(data)
km <- kmeans(data,4)
str(km)
library(animation)
km$cluster
km <- kmeans.ani(data,4)
km$centers
mydata <- wine[1:50,c(1:13)]
normalized_data <- scale(mydata)
d <- dist(normalized_data,method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit)
plot(fit, hang=-1)
group <- cutree(fit,k=10)
rect.hclust(fit,k=10,border="red")
membership <- as.matrix(group)
final <- data.frame(mydata,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)