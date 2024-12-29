fun=function(x,y,mu=c(0,0),sigma=c(1,1),rho=0)
{ xs=(x-mu[1])/sigma[1];  ys=(y-mu[2])/sigma[2];
  res=exp(-(xs^2+ys^2-2*rho*xs*ys)/(2*(1-rho^2)))/(2*pi*sigma[1]*sigma[2]*sqrt(1-rho^2))
  return(res)
}
x=seq(-4,4,length=100); y=x;  rho=c(0,0.75,-0.75)
op=par(mfrow=c(1,3))
for(i in rho)
{  z=outer(x,y,fun,rho=i);    persp(z,box=F,phi=30,theta=30) }
par(op)


n=100;  x=rnorm(n, 0, 2);      y=rnorm(n,0,2);    xy1=cbind(x,y)
mu=c(0,0);   Sigma=diag(c(4,4));     xy2=mvrnorm(n,mu,Sigma)
#比较xy1,xy2的均值、标准差、协方差或相关系数矩阵、核密度图等
apply(xy1,2,mean);  apply(xy2,2,mean); apply(xy1,2,sd);   apply(xy2,2,sd);  
cor(xy1);  cor(xy2);         #随着n的增大，两者的差异性如何？


mu=c(2,-3,1);   Sigma=matrix(c(1,1,1,1,3,2,1,2,2),nrow=3,byrow=TRUE)
a=c(3,-2,1) ;   zmean=sum(a*mu);   zvar=t(a)%*%Sigma%*%a
xy=mvrnorm(1000,mu,Sigma)
apply(xy,2,mean);    apply(xy,2,var);   cov(xy);  cor(xy[,1],xy[,2])
z=a[1]*xy[,1]+a[2]*xy[,2]+a[3]*xy[,3]
mean(z);  var(z)

chifun=function(x,mu,Sigma)  t(x-mu)%*%solve(Sigma)%*%(x-mu)
chis=apply(xy,1,chifun,mu=mu,Sigma=Sigma);  
plot(density(chis))

mu=c(2,-3,1)
Sigma=matrix(c(1,1,1,1,3,2,1,2,2),nrow=3)
mx=matrix(0,nrow=1000,ncol=3)
for(i in 1:1000)
{   x=mvrnorm(100,mu,Sigma);     mx[i,]=apply(x,2,mean); }
apply(mx,2,mean);  cov(mx);  Sigma/100


N=1000; n=100;  mu=c(1,2,3); Sigma=diag(c(1/3,4/3,3))
mx=matrix(0,nrow=1000,ncol=3);
for(i in 1:N)
{   x=cbind(runif(n,0,2),runif(n,0,4),runif(n,0,6));
    mx[i,]=apply(x,2,mean)-mu; }
mx=sqrt(n)*mx;    mean(mx);   var(mx); cor(mx)


mu=c(2,-3,1)
Sigma=matrix(c(1,1,1,1,3,2,1,2,2),nrow=3)
mx=matrix(0,nrow=1000,ncol=3)
for(i in 1:N)
{   x=cbind(runif(n,0,2),runif(n,0,4),runif(n,0,6));
    mx[i,]=apply(x,2,mean)-mu; }
mx=sqrt(n)*mx;    mean(mx);   var(mx); cor(mx)

# 加载MASS包，因为要用到mvrnorm函数
library(MASS)

func1 <- function(N, p, mu, Sigma) {
    mx <- matrix(0, nrow = N, ncol = length(mu))
    for (i in 1:N) {
        # 从多元正态分布中抽取样本
        x <- mvrnorm(n = p, mu = mu, Sigma = Sigma)
        mx[i, ] <- apply(x, 2, mean) - mu
    }
    mx <- sqrt(p) * mx
    result <- list(
        mean_mx = mean(mx),
        var_mx = var(mx),
        cor_mx = cor(mx)
    )
    return(result)
}
p=10
func1(N,p,mu,Sigma)
data <- func1(N, p, mu, Sigma)

# 抽取的样本数据
x <- mvrnorm(N, mu, Sigma)
head(x)
# 使用rgl绘制3D散点图
open3d()
points3d(x[, 1], x[, 2], x[, 3], size = 3, col = "blue")
axes3d(edges = c("x--", "y--", "z--"), 
       ntick = 5,            # 坐标轴刻度数量
       labels = TRUE,       # 是否显示坐标轴标签
       col = "black")       # 坐标轴颜色



