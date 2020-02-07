crime=read.csv(choose.files())
str(crime)
head(crime)
pairs(crime)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(crime[,2:5]) 


d <- dist(normalized_data, method = "euclidean") # distance matrix

 
fit <- hclust(d, method="complete")
plot(fit)
plot(fit, hang=-1)

 


rect.hclust(fit, k=3, border="red") 

groups <- cutree(fit, k=3) # cut tree into 5 clusters



membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)

View(final)

