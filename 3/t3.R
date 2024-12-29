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
#
for循环生成伪随机数序列
linear_congruential_generator <- function (n) {
sequence <- numeric (n)
for (i in 1:n) {
current_number <- (a * seed_value + c) % m # 计算下一个随机数
sequence [i] <- current_number
seed_value <- current_number
# 更新种子值
return ( sequence)

func1 <- function(n, m, a, c,seed_value =1) {
    seq <- numeric(n)

    for (i in 1:n) {
        xn = (a * seed_value + c) %% m
        seq[i] <- xn
        seed_value <- xn/10
    }
    return(seq/m)
}

func1(50,2^35,5^15,1)

a <- 3
c <- 1
m <- 10
seed_value <- 1
xn =(a * seed_value + c)
xn=xn%m

print(xn)

# 定义 LCG 参数
a <- 1664525
c <- 1013904223
m <- 2^32
seed <- 12345  # 初始种子

# LCG 函数
lcg <- function(n, seed, a, c, m) {
  random_numbers <- numeric(n)
  random_numbers[1] <- seed
  for (i in 2:n) {
    random_numbers[i] <- (a * random_numbers[i - 1] + c) %% m
  }
  return(random_numbers / m)  # 归一化到 [0, 1)
}

# 生成 10 个伪随机数
n <- 10
random_numbers <- lcg(n, seed, a, c, m)
print(random_numbers)


N=10000;  x=rexp(N,1); y=rexp(N,1)
zfun=function(x,y) exp(-x^2-y^2)/exp(-x-y)
mean(zfun(x,y))   #输出估计结果

#下面求解比较精确的结果
a=integrate(function(x) exp(-x^2),0,Inf)
a$value^2       

qualify=function(groups=10,n=100000)
{	avg=numeric(groups)
   for(i in 1: groups)
  { x=runif(n,1.909,2.091)
    x=x+1.0    #2，7之和
    x=x+runif(n,4.468,4.682);       x=x+runif(n,2.909,3.091)
    x=x+runif(n,0.898,1.102);      x=x+runif(n,12.9,13.1)
    x=x+runif(n,1.909,2.091)
    avg[i]=sum(x<=27)/n
  }  
  hist(x,main='Last simulation',xlab='overall thickness')
  list(groups = groups,each_n=n,each_mean=avg,total_mean=mean(avg))
	return(mean(avg))
}
qualify()


coms=combn(50,5)          #耗时：1.74秒
cfun=function(x) return(c(min(x),median(x),max(x)))
a=apply(coms,2,cfun)   #全部组合计算耗时：90.19秒
apply(a,1,mean)           
com1=coms[,sample(ncol(coms),1000)]        #这个非常快
a=apply(com1,2,cfun) 
apply(a,1,mean)     #结果与全部计算相近

cfun=function(x) return(c(min(x),median(x),max(x)))
a=comboSample(v=50,m=5,n=1000,seed=10,namedSample=TRUE,FUN=cfun)
a=matrix(unlist(a),nrow=1000,byrow=TRUE,dimnames=list(names(a)))
apply(a,2,mean)   


