dat=iris
colnames(dat)=c(paste0('x',1:4),'g')
shapiro.test(dat[,1])
dat$g=rep(1:3,each=50)
#1:3分别表示setosa，versicolor，virginica
cor(dat[,1:4])
plot(dat)
attach(dat)
g=factor(g)

library(MASS)
pd=qda(g~x1+x2+x3+x4)
table(g,predict(pd)$class)