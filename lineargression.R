library(car)
library(corpcor)
library(lmtest)
library(MASS)
library(leaps)

#数据导入 注意y放在数据框的第一列
a <- c(seq(0.10,0.18,by = 0.01),0.20,0.21,0.23)
b <- c(45,46,41,48,51,47.5,52,53.0,56.0,57.0,61.0,60.0)
c <- c(42.0,43.5,45.0,45.5,45.0,47.5,49.0,53.0,50.0,55.0,55.0,60.0)
data = data.frame(y=c,x1=a,x2=b)

formula=y~x1+x2 #定义回归方程

#lm拟合
LinearRegression <- lm(formula,data =data) #模型建立
summary(LinearRegression)#参数详细表 参数显著性检验 回归系数显著性检验

#回归方程的显著性检验 t检验看summary(linearRegress)
Anova(LinearRegression,type="III") #多元

#相关系数显著性检验F检验 
r<-cor(data,method="pearson") #相关系数矩阵 method有pearson kendall spearman,alternative two.sided双边检验 less左侧 greater 右侧 一个x对应一个y
for (i in 2:length(data))
{
  x<-as.vector(as.matrix(data[i]))
  print(cor.test(x,data$y,alternative = "two.sided",method="pearson",conf.level=0.95))
  }
pcor <-cor2pcor(r) #偏相关矩阵 Partial correlation matrix
pcor

confint(LinearRegression) #回归系数置信区间估计

#预测
new <- data.frame(x1=0.24,x2=38) #必须以数据框格式输出预测的x值
ypred <-predict(LinearRegression,new,interval='prediction',level=0.95) #预测单值和单值置信区间
ypred
yconf <-predict(LinearRegression,new,interval='confidence',level=0.95) #预测单值和单值均值置信区间
yconf

#原数据拟合
Fitting=predict(LinearRegression)

#残差分析
e <- resid(LinearRegression)
#e #残差
SRE <-rstandard(LinearRegression) #学生化残差 Studentized residual
#残差图 x与e
for (i in 2:length(data))
{
  plot(as.vector(as.matrix(data[i])),SRE,xlab = names(data[i]))
}

#异方差
#异方差等级相关系数检验 x与SRE
abse <- abs(e)
for (i in 2:length(data))
{
  x<-as.matrix(data[i])
  print(cor.test(x,abse,alternative="two.sided",method="spearman",conf.level = 0.95))
}
#异方差消除 加权最小二乘法
lb=-2 #搜索下界
ub=2 #搜索上界
s=seq(lb,ub,0.5) #遍历搜索序列
result1 <- vector(length=length(s),mode = "list") #对数似然统计量结果
result2 <- vector(length=length(s),mode = "list") #估计系数和显著性检验
for (j in 1:9)
{
    w <- data[2]^(-s[j]) #计算权重向量 w多元时候要注明是x几
    w	<-as.matrix(w)
    w <- as.vector(w)
    lm <- lm(formula,data =data,weights = c(w))
    result1[[j]]<-logLik(lm)
    result2[[j]]<-summary(lm)
}
result1<-as.numeric(substr(a,1,10))
i<-which(result1==min(result1))
print(s[i])
print(result2[i]) #输出最优加权结果

#自相关判断
plot(e[c(2:length(e))],e[c(1:length(e)-1)],xlab = 'e(t)',ylab = 'e(t-1)') #图示检验法
E = as.vector(e)
dwtest(LinearRegression,alternative = 'two.sided')

#box-cox变换
bc <-boxcox(formula,data=data,lambda = seq(-2,2,0.01))
lambda <- bc$x[which.max(bc$y)]
lambda
y_bc <- (data$y^lambda-1)/lambda
lm_bc <- lm(formula,data=data)
summary(lm_bc)
#bc异方差等级相关系数检验
abse_bc <- abs(resid(lm_bc))
for (i in 2:length(data))
{
y<-cor.test(as.vector(as.matrix(data[i])),abse_bc,alternative="two.sided",method="spearman",conf.level = 0.95)
print(y)
}
#bc自相关判断
e_bc<-resid(lm_bc)
plot(e_bc[c(2:length(e))],e_bc[c(1:length(e)-1)],xlab = names(data[i])) #图示检验法
E = as.vector(e_bc)
dwtest(lm_bc,alternative = 'two.sided')
#使用box-cox变换后记得变换原方程

#自变量选择
#全子集回归
exps <- regsubsets(y~x1+x2,data =data,nbest=1,really.big = T) 
expres<- summary(exps)
res<-data.frame(expres$outmat,调整R平方=expres$adjr2) #输出结果
res
#前进法 后退法 
lm_cs <-lm(y~1,data=data) #常数项回归
lm_cs.2<-step(lm_cs,scope=list(upper=~x1+x2,lower=~1),direction = "forward") #后退法 backward
summary(lm_cs.2) #调用时候注意scope参数的输入
#逐步回归
formula_start = y~x1 #注意开始式的定义
lm_dy <-lm(formula_start,data=data) 
lm_dy_step <-step(lm_dy,scope=list(upper=~x1+x2,lower=~1),direction="both")
summary(lm_dy_step)

#多重共线性
#判断 特征根
XX<-cor(data[,2:length(data)])
XX
kappa(XX,exact=TRUE)

