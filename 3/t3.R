fun=function(n,a)
{	
	c=runif(n, min = 0, max = 1)
	for(i in 1:n) 	c[i]=-1/a*log(1-c[i])
	return(c)
}

set.seed(123)
n=1000

m1=fun(n,1/10)
m2=rexp(n,1/10)
summary(m1)
summary(m2)

dat=c()
for(i in 1:5) 	dat=c(dat,rnorm(50, mean = 2*i, sd = i))
dat.1=matrix(dat,nrow=50)
colnames(dat.1)=c('x1','x2','x3','x4','x5')
summary(dat.1)
#欧式距离
dist(dat.1)
#马氏距离
Cov=cov(dat.1)
Cor=cor(dat.1)
Mean=colMeans(dat.1)
eCov <- eigen(Cov)   # 计算Cov矩阵的特征值与特征向量
SrDIV <- diag(eCov$values^(-1/2)) # 用特征值的-1/2次方生成对角矩阵
U <- eCov$vectors   # Cov矩阵的特征向量
SrDIV <- U %*% SrDIV %*% t(U)
Y <- as.matrix(dat.1) %*% SrDIV
MD <- dist(Y)
MD

dnorm(2, 1, 1)
dnorm(2, 3, 2)

# 生成两组随机样本
x1 <- rnorm(100, 1, 1)
x2 <- rnorm(100, 3, 2)

# 计算x = 2在两个总体下的概率密度
density_x1 <- dnorm(2, mean = mean(x1), sd = sd(x1))
density_x2 <- dnorm(2, mean = mean(x2), sd = sd(x2))
if (density_x1 > density_x2) {
  print("x更有可能属于x1所在的总体")
} else {
  print("x更有可能属于x2所在的总体")
}
