#install.packages('ElemStatLearn')
#install.packages('GGally')
library(ElemStatLearn)
library(MASS)
library(cluster)
library(ggplot2)
data("spam")


X<-spam[,1:(dim(spam)[2]-1)]
y<-spam[,dim(spam)[2]]

kowariancja <- cov(X)
heatmap(kowariancja)

X<-scale(X)
X.pca <- prcomp(X,retx = T)
ggplot(as.data.frame(X.pca$x[,1:2]), aes(PC1,PC2))+geom_point()
summary(X.pca)
wariancja <- (X.pca$sdev ^2)/sum(X.pca$sdev^2)
wariancja.narast <- cumsum(wariancja)
barplot(wariancja)
barplot(wariancja.narast)

n<-3
dane<-cbind(X.pca$x[,1:n],as.factor(y))
ggplot(as.data.frame(dane), aes(x=PC1,y=PC2,colour=as.factor(y))) + geom_point()
plot(as.data.frame(X.pca$x[,1:n]), col=y)

#######################
