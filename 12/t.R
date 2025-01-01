dat=data.frame(x1=c(8,10,6,11,8,7,10,9,9,6,12,9),
         x2=c(135.0,150.0,124.5,157.6,129.6,126.1,134.4,125.8,129.0,116.8,155.0,141.9),
         y=c(29.1,32.2,24.1,34.0,24.5,23.5,29.8,23.8,26.7,23.2,34.5,30.9))
lm.out=lm(y~x1+x2,data=dat)
summary(lm.out)
attach(dat)
cor(x1,x2)
detach(dat)
lm2=lm(y~1,data=dat);  
step(lm2, y~x1+x2,directrion='forward')    
step(lm.out,directrion='backward') 
lm3=lm(y~.,data=dat); 
step(lm3,direction='both')

dat=read.table('clipboard',header=FALSE)
colnames(dat)=c('t','vol')
plot(dat)

fun1=function(a,b,t)
{	
	return(4200/49*a*(exp(-b*t)-exp(-a*t))/(a-b))
}

m.1 <- nls(vol ~ fun1(a,b,t), data = dat, start = list(a=1,b=2), trace = T)

y=predict(m.1)
new_dat=cbind(dat$t,y)
plot(dat,type='l')
lines(new_dat, col = "red",
      lty = 2)

dat=read.table('clipboard',header=TRUE)
res=leaps(dat[,1:4],dat[,5],nbest=6,method="r2")
ymean=mean(dat[,5]); n=nrow(dat);  p=ncol(dat)-1
ST=sum((dat[,5]-ymean)^2)
SE=ST*(1-res$r2)
MSE=SE/(n-p-1)
AIC=n*log(SE)+2*apply(res$which,1,sum)
BIC=n*log(SE)+apply(res$which,1,sum)*log(n)
R2=res$r2
adR2=leaps(dat[,1:4],dat[,5],nbest=6,method="adjr2")$adjr2
res$which[max(res$r2),]

regsubsets(formula,data,nbest)
res=regsubsets(y~., data=dat, nbest=1)
summary(res)
names(res)  #看到相关的输出数据
a=summary(res)
names(a)   

