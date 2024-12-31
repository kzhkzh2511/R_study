colnames(dat)=c('t','freq','loss','waveform',paste0('x',1:1024),'g')

zbl=dat[,5:1028]
head(zb1)
formula_str <- paste("waveform ~ ", paste(paste0("x", 1:1024), collapse = " + "))
formula_obj <- as.formula(formula_str)
ld=lda(formula_obj,prior=c(1,1,1)/3,dat)
Z=predict(ld)
newg=Z$class
table(g=dat$waveform,newg)

qd=qda(formula_obj,prior=c(1,1,1)/3,dat)
qd
Z1=predict(qd)
newg1=Z1$class
table(g=dat$waveform,newg1)



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
head(dat)
dat$waveform=factor(dat$waveform)
result=distinguish.distance(dat[,5:1028],k)
newg=result[1:length(result)]
table(g,newg)
fun1=function(d)
{
if(d=="正弦波") t=1
else if(d=='梯形波') t=2
else t=3
return(t)
}
dat$waveform
k=dat$waveform
k=sapply(k,fun1)
k=factor(k)


