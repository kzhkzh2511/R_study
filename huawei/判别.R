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

