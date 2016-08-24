library(ISLR)
library(datasets)
attach(iris)

#sepal.width vs sepal.length
train= iris
test=iris

pairs(iris)

#plot(1:20,1:20, pch=4)1:20)

nearest1= knn(train=train,test=test, k=5,cl=Species)

#DOUBT : about LIXO in the Rscript 1 : Glass example
#DOUT : how to exclude black dots
plot(iris[,c(1,2)], col=Species, cex=1.0, main="k-Nearest Neighbors Errors, k=5",pch=19)

set.seed(10)
lixo = sample(1:dim(iris[which(iris$Species!="setosa" & nearest1!='setosa'),])[1],25)

for(i in 1:length(lixo)){
  points(iris[lixo[i],1],iris[lixo[i],2], pch=4, col='orange', cex=2)
}

