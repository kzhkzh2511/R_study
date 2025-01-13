dat=read.table("winequality-red.csv",sep=';',header=TRUE)
dat2=read.table("winequality-white.csv",sep=';',header=TRUE)

wdat=rbind(dat,dat2)
colnames(wdat)=c('fix_a','vol_a','cir_a','sugar','chl','f_s_d','t_s_d','density','ph','sulphates','alcohol','quality')

wdat$g=c(rep(0,1599),rep(1,4898))
par(mfrow=c(3,4))
for(i in 1:12){
	boxplot(dat3[,i]
}
corr=cor(wdat)
corrplot(corr,method='color',addCoef.col = 'black')


dat3=rbind(dat,dat2)
colnames(dat3)=c('fix_a','vol_a','cir_a','sugar','chl','f_s_d','t_s_d','density','ph','sulphates','alcohol','quality')
lm=lm(quality~.,dat3)
summary(lm)
lm.1=lm(quality~1,dat3)

lm.2=step(lm,direction="backward")


summary(lm.2)


df<-dat3[,-12]
res.pca <- prcomp(df,scale. = T)
get_eig(res.pca)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 40))

#加载这两个R包
library(FactoMineR)
library(factoextra)

X<-res.pca$x[,1:4]
data1<-data.frame(X,dat3[,12])
colnames(data1)<-c(paste("X",1:4,sep = ""),"Y")

# 设置随机数种子，以确保结果可重复
set.seed(123)
# 计算训练集和测试集的观测数量
train_size <- round(0.8 * nrow(data1))
test_size <- nrow(data1) - train_size

# 随机抽样选取训练集的行索引
train_rows <- sample(1:nrow(data1), train_size)

# 根据训练集的行索引获取训练集和测试集
train_set <- data1[train_rows, ]
test_set <- data1[-train_rows, ]




# 2. 决策树（Decision Tree）:
# 创建模型
library(rpart)
model_dt <- rpart(Y~ ., data = train_set)

# 预测
predictions_dt <- predict(model_dt, newdata = test_set, type = "class")
# 通过table函数计算混淆矩阵
confusion_matrix <- table(test_set$Y, predictions_dt)

# 打印混淆矩阵
print(confusion_matrix)


# 3. 随机森林（Random Forest）:
# 创建模型
library(randomForest)
model_rf <- randomForest(Y ~ ., data = train_set)

# 预测
predictions_rf <- predict(model_rf, newdata = test_set)
predictions_rf <- round(predictions_rf)
# 通过table函数计算混淆矩阵
confusion_matrix <- table(test_set$Y, predictions_rf)


# 打印混淆矩阵
print(confusion_matrix)

r=t(confusion_matrix)

# 4. 支持向量机（Support Vector Machine，SVM）:
# 创建模型
library(e1071)
model_svm <- svm(Y ~ ., data = train_set)

# 预测
predictions_svm <- predict(model_svm, newdata = test_set)
# 通过table函数计算混淆矩阵
confusion_matrix <- table(test_set$Y, predictions_svm)

# 打印混淆矩阵
print(confusion_matrix)
m=confusion_matrix[2:6,]
sum(diag(m)) / sum(confusion_matrix)




