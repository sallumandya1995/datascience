airline=read.csv(choose.files())
str(airline)
summary(airline)
airline=airline[-1]
ndata=scale(airline)
View(ndata)
d=dist(ndata,method = "euclidean")
?hclust
fit <- hclust(d, method = "complete")
 
plot(fit) # display dendrogram
plot(fit, hang=-1)
