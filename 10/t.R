index=sample(1:50,5,replace=FALSE); index=c(index,index+50,index+100)
subiris=iris[index,];op=par(mfrow=c(2,4))
d=dist(subiris[,1:4],method='euclidean')
methods=c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
for(i in 1:8){ hc=hclust(d,method=methods[i]); plot(hc,hang=-1,main='') }

my.iris=iris;  colnames(my.iris)= c('x1','x2','x3','x4','g')
d=dist(my.iris[,1:4],method='euclidean')
op=par(mfrow=c(2,4))
for(i in 1:8){ hc=hclust(d,method=methods[i]); plot(hc,hang=-1,main='') }

hc=hclust(d,method=methods[4]); 
cutree(hc,k=1:5)    #将某个聚类结果hc，分成k类的结果展示
A=cutree(hc,k=3)    #分成三个类
table(A,my.iris$g)  #complete的分类效果比较。

mx=apply(my.iris[,1:4],2,mean)
W=sum(diag(var(my.iris[,1:4])*(nrow(my.iris)-1)))   #得到总离差平方和
clust=cutree(hc,k=3)  #得到分类结果
ns=table(clust)  #得到各类个数
k=max(clust)     #得到分类个数
mxs=by(my.iris[,1:4],clust,colMeans)  #得到新分类的类均值
B=0
for(i in 1:k) B=B+sum((mx-mxs[[i]])^2)*ns[i]  #计算新分类的组间离差平方和
B/W

mxs=by(my.iris[,1:4],my.iris[,5],colMeans) #得到原始分类的类均值
B=0
for(i in 1:k) B=B+sum((mx-mxs[[i]])^2)*50 #计算原始分类的组间离差平方和
B/W


my.iris=iris;  colnames(my.iris)= c('x1','x2','x3','x4','g')
km=kmeans(my.iris[,1:4],centers=3)
km   #可以看到分类的一些基本信息，比如组间离差占比，分类结果等。
cbind(km$cluster,my.iris$g)
table(km$cluster,my.iris$g)   #发现主要在第2，3类之间聚类出现问题。