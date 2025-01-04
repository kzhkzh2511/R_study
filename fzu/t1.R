#1.2
N=100000;x=runif(N,0,5);y=runif(N,0,10)
f <- function(x, y) {return(x * y * exp(-x * y - x^2))}
v=f(x,y)
summary(v)
mean(v)*50

install.packages("pracma")
library(pracma)
# 使用integral2函数进行数值积分
result <- integral2(f, 0, 5, 0, 10)
print(result)

#1.3
-qchisq(0.025, 10)+qchisq(0.975,10)
for (i in 10:50) print(-qchisq(0.025, i)+qchisq(0.975,i))

#1.4
N=100000
x=rexp(N,2);y=rexp(N,1);
p=0
n=1000
for(i in 1:n)
{	x1=sample(x,100);y1=sample(y,100);mx=mean(x1);my=mean(y1)
if((2*mx/my)>1) p=p+1
}

p=p/n
p

#1.5

#1.6
x=c(11,15,21,31,35,40,9,28,25)
d=dist(x,method='euclidean')
op=par(mfrow=c(2,4))
methods=c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
for(i in 1:8){ hc=hclust(d,method=methods[i]); plot(hc,hang=-1,main='') }

hc=hclust(d,method=methods[3]); 
plot(hc,hang=-1)
rect.hclust(hc,k=3) 

#2
x=c(-0.8,-0.5,0,0.5,2,2.4,2.7,2.97)
mean(x)
var(x)
max(-x)
max(1/3*x)
