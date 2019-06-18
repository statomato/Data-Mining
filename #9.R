#hw 3.1

data("USArrests")
USArrests.scaled=scale(USArrests)
correlation=as.dist(1-cor(t(USArrests.scaled)))
euclidean=dist(USArrests.scaled)^2
summary(correlation/euclidean)
summary(correlation-0.1339*euclidean)
plot(hclust(correlation,method="average"))
plot(hclust(euclidean,method="average"))

#hw 3.2
#a
set.seed(42)
data= matrix(sapply(1:3,function(x){ rnorm(20*50,mean = 10*sqrt(x))}),ncol=50) 
class=unlist(lapply(1:3,function(x){rep(x,20)}))

#b
pr.out <- prcomp(data)
plot(pr.out$x[,c(1,2)],col=class)

#c
set.seed(1)
kmeans.out <- kmeans(data,3)
table(kmeans.out$cluster)
plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)

#d
kmeans.out <- kmeans(data,2)
table(kmeans.out$cluster)
table(class)
plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)

#e
kmeans.out <- kmeans(data,4)
table(kmeans.out$cluster)
plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)

#f
kmeans.out <- kmeans(pr.out$x[,c(1,2)],3)
table(kmeans.out$cluster)
plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)

#g
set.seed(1)
kmeans.out <- kmeans(scale(data,center=T,scale=T),3)
table(kmeans.out$cluster)
plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)
