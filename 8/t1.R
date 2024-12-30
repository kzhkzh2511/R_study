mu=10
sigma=2
set.seed=123
x=rnorm(40,mu,sigma)
conf=0.95
mx=mean(x);  sdx=sd(x); 
2*(1-pnorm(abs(mx-mu)*sqrt(n)/sigma))

mu1=10; mu2=5;
 sigma=2; conf=0.95; 
m=20; n=30; 
set.seed(100);
x=rnorm(m,mu1,sigma); y=rnorm(n,mu2,sigma); 
   mx1=mean(x); my1=mean(y);
varx=var(x); vary=var(y); 
qua=qnorm((1+conf)/2);  qta=qt((1+conf)/2,m+n-2);  
c(mx1-my1-qua*sqrt(sigma^2/m+sigma^2/n),mx1-my1+qua*sqrt(sigma^2/m+sigma^2/n))
Sp=sqrt(((m-1)*varx+(n-1)*vary)/(m+n-2))*sqrt(1/m+1/n)
c(mx1-my1-qta*Sp,mx1-my1+qta*Sp)            # t.test(x,y,var.equal=T) 可得到相同结果


x=rnorm(200,0,1)
x=data.frame(x=rnorm(200),y=as.factor(sample(1:4,200,replace=TRUE)))
mx=mean(x[,1]);  mxs=by(x[,1],x[,2],mean);  
ns=table(x[,2]);  K=length(ns);  n=nrow(x)
(St=sum((x[,1]-mx)^2));  (Sr=sum((mxs-mx)^2*ns)); 
 (Se=St-Sr);
(F=Sr/(K-1)/(Se/(n-K))); (p.value=1-pf(F,K-1,n-K)); 
summary(aov(x~y,data=x));

u0=c(90,58,16);
dat=matrix(c(78,76,92,81,81,84,60.6,58.1,63.2,59.0,60.8,59.5,16.5,12.5,14.5,14.0,15.5,14.0),ncol=3,byrow=FALSE)
HotellingsT2(dat, mu = c(172,65,82), test = "chi")


dat=matrix(c(78,76,92,81,81,84,60.6,58.1,63.2,59.0,60.8,59.5,16.5,      12.5,14.5,14.0,15.5,14.0),ncol=3,byrow=FALSE)
dat2=matrix(c(80,75,78,75,79,78,75,64,80,58.4,59.2,60.3,57.4,59.5,58.1,58.0,55.5,59.2,14.0,15.0,15.0,13.0,14.0,14.5,12.5,11.0,12.5),ncol=3,byrow=FALSE)
dat=as.data.frame(dat)
colnames(dat)=c('x1','x2','x3')
dat2=as.data.frame(dat2)
colnames(dat2)=c('x1','x2','x3')
cdat=rbind(dat,dat2)
cdat$y=c(rep(1,each=6),rep(2,each=9))
HotellingsT2(cbind(cdat$x1,cdat$x2,cdat$x3)~cdat$y)


dat=read.table('clipboard',header=TRUE)
dat2=read.table('clipboard',header=TRUE)
dat=rbind(dat,dat2)
dat$y=rep(0:1,each=30)
dat$y=factor(dat$y)
fit=manova(cbind(dat$x1,dat$x2,dat$x3,dat$x4)~dat$y)
summary.aov(fit)
summary(fit,test="Wilks")
summary(fit)
summary(fit,test='Roy')