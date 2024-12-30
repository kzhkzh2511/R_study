dat=read.table("clipboard",header=FALSE)
colnames(dat)=c('t','freq','loss','waveform',1:1024)
newdat=read.table("clipboard",header=FALSE)
colnames(newdat)=c('t','freq','loss','waveform',1:1024)
dat <- rbind(dat, newdat)


n_rows <- 3400 + 3000 + 3200 + 2800
type_vec <- rep(0, n_rows)

# 分段赋值
type_vec[1:3400] <- 1
type_vec[(3400 + 1):(3400 + 3000)] <- 2
type_vec[(3400 + 3000 + 1):(3400 + 3000 + 3200)] <- 3
type_vec[(3400 + 3000 + 3200 + 1):n_rows] <- 4

# 将向量赋值给dat$type列
dat$type <- type_vec


table(dat$type)
table1=table(dat$t,dat$waveform)
prop.table(table(dat$waveform))
table(dat$waveform,dat$loss)
table(dat[dat$type==1,'t'],dat[dat$type==1,'waveform'])
table(dat[dat$type==2,'t'],dat[dat$type==2,'waveform'])
table(dat[dat$type==3,'t'],dat[dat$type==3,'waveform'])
table(dat[dat$type==4,'t'],dat[dat$type==4,'waveform'])

boxplot(dat$loss~dat$waveform+dat$type)

boxplot(dat$loss~dat$waveform+dat$t)
prop.table(table(dat$waveform,dat$t))

cols=c('正弦波'='black','三角波'='blue','梯形波'='green');
par(mfrow=c(2,2))
for(i in 1:4){
k=apply(dat[dat$type==i,paste0(1:1024)],1,diff)
k=apply(k,2,var)
mydat=data.frame(waveform=dat[dat$type==i,4],k=k)
plot(mydat[,2],col=cols[mydat[,1]])
}
k=apply(dat[dat$type==1,paste0(1:1024)],1,diff)
k=apply(k,1,var)
