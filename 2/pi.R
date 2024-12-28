x=scan('pi20w.txt',what='character')
x=unlist(strsplit(x,''))
x=as.integer(x)
head(x)

fun2=function(n) max(n)-min(n)

x1=matrix(x,nrow=20000,byrow=TRUE)
x1.mean1=apply(x1,1,mean)
x1.mean2=apply(x1,2,mean)
x1.sd1=apply(x1,1,sd)
x1.sd2=apply(x1,2,sd)
x1.range1=apply(x1,1,fun2)
x1.range2=apply(x1,2,fun2)
x1.median1=apply(x1,1,median)
x1.median2=apply(x1,2,median)
summary(x1)

x2=as.data.frame(x1)
colnames(x2)=1:10
plot(x2)

x2
head(x2)
summary(x2)
par(mfrow=c(2,5))

for(i in 1:10) hist(x1[,i],col='blue',border='yellow',main='',xlab='')


dim(x2)
boxplot(x2)


fit=manova(y~g)
y=cbind(x.dat$x)

ks.test(x,"punif")