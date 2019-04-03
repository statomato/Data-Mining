set.seed(55)

#2차원 산점도 
n <- 1000
p <- 2
data <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
data <- t(apply(data, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
xyplot(data[,1]~data[,2], xlab = "x1",ylab = "x2", pch = 19, cex=0.5,main="2 dimensional scatter plot")

#3차원 산점도 
n <- 1000
p <- 3
data <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
data <- t(apply(data, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
scatterplot3d(data[,1],data[,2],data[,3],
              xlab="x1", ylab="x2", zlab="x3",
              main="3 dimensional scatter plot",
              color="blue")

#2차원 히스토그램
n <- 10000
p <- 2
data <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
data <- t(apply(data, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
y <- apply(data,1, function(z){sqrt(sum(z^2))})
histogram(~y,nint=100, ylab="",xlab="",main="2-dim histogram")

#10차원 히스토그램
n <- 10000
p <- 10
data <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
data <- t(apply(data, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
y <- apply(data,1, function(z){sqrt(sum(z^2))})
histogram(~y,nint=100, ylab="",xlab="",main="10-dim histogram")

#100차원 히스토그램
n <- 10000
p <- 100
data <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
data <- t(apply(data, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
y <- apply(data,1, function(z){sqrt(sum(z^2))})
histogram(~y,nint=100, ylab="",xlab="",main="100-dim histogram")

#simulation
n <- 1000
p <- 1:100
nearest.m <- (1-0.5^(1/n))^(1/p)
get.nearest <- function(p){
  data <- rmvnorm(n, mean=rep(0,p), sigma=diag(p))
  data <- t(apply(data, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
  res <- apply(data,1,function(z){sqrt(sum(z^2))})
  ifelse(p==1, min(data^2), min(res))
}

sim.median <- sapply(1:100, function(z){
                quantile(replicate(100,get.nearest(z)),prob=0.5)
})
#sim.median


compares <- data.frame(y1 = nearest.m[1:100],
                      y2 = sim.median,
                      p = p[1:100])
compares

xyplot(y1 + y2 ~p, data=compares, type=c("l","g"),ylim=c(0,1.1),
       xlab="p", ylab="distance",
       auto.key=list(columns=2,
                     text=c("Result 1","Result 2")))
