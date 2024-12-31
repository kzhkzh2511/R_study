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


#输入dat的前面几列为数据，最后一列为因子g
func1=function(dat,type)
{	
	mx=matrix(rep(0,each=length(dat)-1,time=type),nrow=length(dat)-1)
	s=0
	tab1=table(dat$g)
	for(i in 1:type){
	mx[,i]=apply(dat[dat$g==i,1:length(dat)-1],2,mean)
	s=s+tab[i]*
}
}
attach(dat)
ld=lda(g~x1+x2+x3+x4,prior=c(1,1,1)/3)
table(g,predict(ld)$class)
Z=predict(ld)
cbind(g,round(Z$posterior,3),Z$class) #看分类细节
table(g,Z$class)  #混淆矩阵
prop.table(table(g,Z$class),1)  #正确率

pd=qda(g~x1+x2+x3+x4,prior=c(1,1,1)/3)
Z=predict(pd)
cbind(g,round(Z$posterior,3),Z$class) #看分类细节
table(g,Z$class)  #混淆矩阵
prop.table(table(g,Z$class),1)  #正确率

n=dim(dat)[1]
k=3
ld=lda(g~x1+x2+x3+x4,dat)
ev=ld$svd^2*(k-1)/(n-k)
round(ev,3)
prop=ev/sum(ev)
round(prop,4)
round(cumsum(prop),4)
round(ld$scalling,3)
Z=predict(ld)
round(Z$x,3)
plot(Z$x,cex=0)
text(Z$x[,1],Z$x[,2],cex=0.7,g)

distinguish.distance <- function(TrnX,TrnG,TstX=NULL,var.equal=FALSE){
  if (is.factor(TrnG) == FALSE){
    mx <- nrow(TrnX); mg <- nrow(TrnG)
    TrnX <- rbind(TrnX, TrnG)
    TrnG <- factor(rep(1:2,c(mx,mg)))  # 1重复mx遍，2重复mg遍
  }
  if (is.null(TstX) == TRUE) TstX <- TrnX
  if (is.vector(TstX) == TRUE) 
    TstX <-t(as.matrix(TstX))
  else if (is.matrix(TstX) != TRUE)
    TstX <- as.matrix(TstX)
  if (is.matrix(TrnX) != TRUE)
    TrnX <- as.matrix(TrnX)
  
  nx <- nrow(TstX)
  # blong用于存放预测值
  blong <- matrix(rep(0,nx),nrow=1,dimnames=list("blong",1:nx))
  g <- length(levels((TrnG)))    # 计算群体类别个数
  mu <- matrix(0,nrow=g,ncol=ncol(TrnX))
  # 每一个群体都有一个均值
  for(i in 1:g)   
    mu[i,] <- colMeans(TrnX[TrnG == i,])
  print(mu)
  # 计算马氏距离
  D <- matrix(0,nrow=g,ncol=nx)
  if (var.equal == TRUE || var.equal == T){
    for (i in 1:g)    # 样本到每一个类别的马氏距离
      D[i,] <- mahalanobis(TstX,mu[i,],var(TrnX))  # 混合样本方差
  }
  
  else{
    for (i in 1:g)
      D[i,] <- mahalanobis(TstX, mu[i,],var(TrnX[TrnG == i,]))
  }
  print(D)
  for (j in 1:nx){   # 分别判别每一个样本属于哪一个类别
    dmin <- Inf
    for (i in 1:g){    # 遍历每一个类别，找出最小距离
      if (D[i,j] < dmin){
        dmin <- D[i,j];
        blong[j] <- i
      }
    }
  }
  blong
}
result=distinguish.distance(dat[,1:4],g)
newg=result[1:length(result)]
table(g,newg)






