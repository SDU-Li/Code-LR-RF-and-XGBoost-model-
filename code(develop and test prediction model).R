rm(list=ls())
getwd()
setwd("D:/R/")
bb<-read.csv("2018data.csv")
bb<-bb[,c(-1,-23)]
naf<-function(x){ 
  nas<-sum(is.na(x)) 
  return(nas) }
Na<-apply(bb,2,naf)
# 查看数据缺失情况
View(Na)
#####插补缺失值
install.packages("missForest")
library(missForest)
View(bb)
###样本量估计
library(pmsampsize)  #加载pmsampsize包用于估算样本量
pmsampsize(type = "b", csrsquared = 0.05, parameters = 20, prevalence = 0.06)
summary(train_bb$Status)
bb$Gender<-as.factor(bb$Gender)
bb[names(bb)[3:21]]<-lapply(bb[names(bb)[3:21]],as.factor)
bb$Sex<-as.factor(bb$Sex)
names(bb)[names(bb) == "CHDaddecg"] <- "Coronaryheartdisease"
names(bb)[names(bb) == "Gender"] <- "Sex"
names(bb)[names(bb) == "AD8"] <- "AD8score"
names(bb)[names(bb) == "GDSF"] <- "GDS15score"
names(bb)[names(bb) == "Drink"] <- "Alcoholdrinking"
names(bb)[names(bb) == "Smoke"] <- "Currentsmoking"
names(bb)[names(bb) == "Hyperlipoidemia"] <- "Hyperlipidemia"
names(bb)[names(bb) == "PSQIA"] <- "PSQIscore"
names(bb)[names(bb) == "EGFR"] <- "Impairedkidneyfunction"
names(bb)[names(bb) == "Senile.Osteoarthritis"] <- "Osteoarthritis"
names(bb)[names(bb) == "PD"]<-"Parkinsondisease"
bb <- missForest(bb, ntree = 500) 
oob_error <- bb$OOBerror
####OBB插补误差0.146分类变量
View(oob_error)
BB <- bb$ximp 
bb<-BB
summary(BB)
write.csv(BB,"2018data.csv")
###拆分数据集6:4
set.seed(111)
s <-  sample(nrow(bb), round(nrow(bb)*0.6))
test_bb<- bb[-s,]
train_bb <- bb[s,]
names(train_bb)
summary(train_bb)
summary(bb)
summary(test_bb)
####ss table1第一种方法
s1<-read.csv("2018data.csv")
library(tableone)
library(psych)
library(table1)
library(compareGroups)
vars<-c("Age","Sex","Education","BMI","CHD","Stroke","Alcoholdrinking","Currentsmoking","AD8score","GDS15score","EGFR","PD","PSQIA","Hearing","Diabetes","Hyperlipidemia","Hypertension","Senile.Osteoarthritis","Ulcer"             
        , "Fatty.liver" )
bb1<-CreateTableOne(vars=vars,strata=c("Status"),addOverall=TRUE,data=bb,includeNA = TRUE)
bb2<-print(bb1,showAllLevels=TRUE,quote=FALSE,noSpaces=TRUE,printToggle=FALSE,catDigits = 2, contDigits = 2, pDigits = 3)
write.csv(bb2,"Stable1.csv")

###(19)hypertension，手动提取结果
summary(bb)
aa19<-na.omit(aa19)
aa19$hypertension<-factor(aa19$hypertension,levels=c(0,1))
fit19<-glm(Status~Hypertension,data=bb,family=binomial,x=T,y=T)
summary(fit19)
library(gmodels)
CrossTable(aa19$hypertension,aa19$status,digits=4, prop.c=TRUE, prop.r=FALSE,prop.t=FALSE,chisq = TRUE)

####两个数据集进行组间变量分析
library(tableone)
library(psych)      
library(table1)
library(compareGroups)
write.csv(train_bb,"train.csv")
write.csv(test_bb,"test.csv")
library(tableone)
library(psych)
train_bb$group <- "train"
test_bb$group <- "test"
totalbb <- rbind(train_bb,test_bb)
vars<-c("Sex","Age","Education","BMI","CHD","Stroke","Alcoholdrinking","Currentsmoking","AD8score","GDS15score","EGFR","PD","PSQIA","Hearing","Diabetes","Hyperlipidemia","Hypertension","Senile.Osteoarthritis"             
        , "Fatty.liver" )
bb1<-CreateTableOne(vars=vars,strata=c("Status"),addOverall=TRUE,data=bb,includeNA = TRUE)

bb2<-print(bb1,showAllLevels=TRUE,quote=FALSE,noSpaces=TRUE,printToggle=FALSE,catDigits = 2, contDigits = 2, pDigits = 3)
write.csv(bb2,"mytable1.csv")
train_bb$group <- "train"
test_bb$group <- "test"
tab1<-CreateTableOne(vars=vars,strata=c("group"),addOverall=TRUE,data=totalbb,includeNA = TRUE)
tab_result<-print(tab1,showAllLevels=TRUE,quote=FALSE,noSpaces=TRUE,printToggle=FALSE,catDigits = 2, contDigits = 2, pDigits = 3)
write.csv(tab_result,"tab_result.csv")
totalbb <- rbind(train_bb,test_bb)
# 输出Table 1
print(table1_result, smd = TRUE)
hist(train_bb$Age)
# 验证训练集和验证集的均衡性
library(compareGroups)
train_bb$group <- "train"
test_bb$group <- "test"
total <- rbind(train_bb,test_bb)
base_tab<-data.frame()
base_tab<-descrTable(group~., data= total, method = c(Age=1),digits = 2,show.all=TRUE)
print(base_tab)
export2csv(base_tab, file='base_tab.csv')
summary(bb)
#####单因素logistics
table2<- data.frame()
for(l in c(21:1)){
  result <- glm(train_data$Status~train_data[,l],train_data,family=binomial())
  sm <- as.data.frame(summary(result)$coe)
  con <- as.data.frame((confint(result)))
  out <- cbind(names(train_data)[l],sm[2,1],exp(sm[-1,1]),sm[-1,4],exp(con[-1,]))
  table2 <- rbind(out,table2)}
table2$`sm[2,1]` <- round(sm[2, 1], 3)
table2$`exp(sm[-1, 1])`<- as.numeric(table2$`exp(sm[-1, 1])`)
table2$`sm[-1, 4]`<- as.numeric(table2$`sm[-1, 4]`)
table2$`2.5 %`<- as.numeric(table2$`2.5%`)
table2$`97.5 %`<- as.numeric(table2$`97.5 %`)
table2$'OR'<-paste0(round(table2$`exp(sm[-1, 1])`,2),
                    "(",
                    round(table2$`2.5 %`,2),
                    "~",
                    round(table2$`97.5 %`,2),
                    ")")
table2$`sm[-1, 4]`<-round(table2$`sm[-1, 4]`,5)
# 重新排列列的顺序：包括自变量名、β系数、OR值、置信区间、p值
table2 <- table2[, c(1,2,8,4)]

fit<-glm(Status~Age+,data=train_data,family=binomial,x=T,y=T)
summary(fit)
library(questionr)
odds.ratio(fit)

# 标准化自变量
train_data<- train_bb
train_data[, 2] <- scale(train_bb[, 2])  # 对除第一列（因变量）以外的列进行标准化
fit<-glm(Status~Age +Gender +Education+Drink+Stroke+AD8+Smoke+GDSF+EGFR+PSQIA+Hearing+Hyperlipoidemia,data=train_data,family=binomial,x=T,y=T)
summary(fit)
library(questionr)
# 设置选项以避免使用科学计数法  
options(scipen=999)  

# 重新运行 odds.ratio  
odds.ratio(fit)

######多因素logistic回归

MODEL2<-glm(Status ~ Age +Gender +Education+Drink+Stroke+AD8+Smoke+GDSF+EGFR+PSQIA+Hearing+Hyperlipoidemia,  data = train_bb_standardized, family = binomial())
summary(MODEL2)
install.packages("questionr")
library(questionr)
# 设置选项以避免使用科学计数法  
options(scipen=999)  

# 重新运行 odds.ratio  
odds.ratio(MODEL2)
# 假设您的 odds.ratio 结果存储在 result 中  
result <- odds.ratio(MODEL2)  

# 将 result 转换为数据框  
result_df <- as.data.frame(result)  

# 将行名转换为一列，便于输出到 CSV  
result_df$Variable <- rownames(result_df)  

# 只保留数值列中的三位小数  
result_df[] <- lapply(result_df, function(x) if(is.numeric(x)) round(x, 3) else x)  

# 将结果导出为 CSV 文件  
write.csv(result_df, file = "odds_ratio_results.csv", row.names = FALSE)
#####lasso
library(glmnet)
library(foreign)#阅读文件
bb_lasso<-subset(train_bb,select = c(Age,Sex,Education,Alcoholdrinking,Stroke,AD8score,GDS15score,PSQIscore,Hearing,Hyperlipidemia,Status,BMI,Diabetes,Currentsmoking,Impairedkidneyfunction))
library(dplyr)
library(glmnet)
library(MASS)
library(survival)
library(rms)
CandidateVariablesbb <- c("Age","Sex","Education","Alcoholdrinking","Stroke","AD8score","GDS15score","Impairedkidneyfunction","PSQIscore","Hearing","Hyperlipidemia","BMI","Diabetes","Currentsmoking")
tmp.ybb <- bb_lasso$Status
tmp.xbb <- model.matrix(~.,bb_lasso[CandidateVariablesbb])


#开始构建模型
f2 = glmnet(tmp.xbb, tmp.ybb, family="binomial", nlambda=100, alpha=1,cv=10)


#可视化f1
plot(f2, xvar="lambda", label=TRUE)
legend("topright", legend = colnames(tmp.xbb), lty = 1, col = 1:ncol(tmp.xbb), cex = 0.8)
print(f2)
windowsFonts(
  A=windowsFont("Airal"))
par(ps=10) 
par(family='A',font=1)


dev.off()






#进行交叉验证
set.seed(1234)
cv.model2 <- cv.glmnet(tmp.xbb, tmp.ybb, family="binomial", nlambda=50, nfolds = 10,alpha=1, standardize=TRUE)
plot(cv.model2)
cv.model2$lambda.min
min<-cv.model2$lambda.min
log.lambda.min <- log(min)
print(log.lambda.min)
cv.model2$lambda
coef(cv.model2, s=cv.model2$lambda.min) 

cv.model2$lambda.1se
coef(cv.model2, s=cv.model2$lambda.1se) 
cv.model2$lambda.1se
se<-cv.model2$lambda.1se
log.lambda.se<- log(se)
print(log.lambda.se)
#####纳入8个变量
library(pROC)
Lgs <- glm(Status~ Age+Education+Drink+Stroke+AD8+GDSF+EGFR+Hearing, data =train_bb,family=binomial,x=T,y=T  )
train_bb$pre<-predict(Lgs,newdata = train_bb,type='response')
head(bb_lasso)
Lgs_roc.bb<-roc(Status~pre,train_bb)
bb.p1<-plot(Lgs_roc.bb,col="skyblue",legacy.axes=T,print.auc=T,print.thres = "best")##添加截点和95%CI
library(reportROC)
res.num<- reportROC(gold=train_bb$Status,#金标准
                    predictor=train_bb$pre,#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num
###等值回归
# 加载必要的包
install.packages("isotone")  # 如尚未安装，请先安装
library(isotone)

# 假设train_bb_standardized中包含模型预测概率和真实标签
# pre: 模型的预测概率
# Status: 真实的二分类结果 (0或1)
# 1. 对数据进行排序，确保预测概率单调递增
train_bb_standardized <- train_bb_standardized[order(train_bb_standardized$pre), ]
# 3. 对x值添加微小扰动，避免重复值
unique_x <- jitter(isotone_model$x, factor = 1e-7)  # 添加极小的扰动

# 执行等值回归（等值回归对预测概率进行单调校准）
isotone_model <- isoreg(train_bb_standardized$pre, train_bb_standardized$Status)

# 对预测概率进行校准
pre_cal <- approx(
  x = unique_x, 
  y = isotone_model$yf, 
  xout = train_bb_standardized$pre
)$y
# 将校准后的概率保存到数据集中
train_bb_standardized$pre_calibrated <- pre_cal

# 查看校准前后的概率对比
head(data.frame(
  原始预测概率 = train_bb_standardized$pre, 
  校准后概率 = train_bb_standardized$pre_calibrated
))
# 去重x值，确保唯一性
unique_index <- !duplicated(isotone_model$x)
unique_x <- isotone_model$x[unique_index]
unique_y <- isotone_model$yf[unique_index]

# 检查unique_x和unique_y
print(data.frame(unique_x, unique_y))

# 执行插值
pre_cal <- approx(
  x = unique_x,
  y = unique_y,
  xout = train_bb_standardized$pre,
  rule = 2  # 允许外推，防止边界值问题
)$y

# 检查校准后的概率
summary(pre_cal)
train_bb_standardized$pre_calibrated <- pre_cal
# 检查预测概率和真实标签
summary(train_bb_standardized$pre)
table(train_bb_standardized$Status)

# 检查是否有缺失值
sum(is.na(train_bb_standardized$pre))
sum(is.na(train_bb_standardized$Status))
# 原始概率 vs 校准后概率
plot(train_bb_standardized$pre, train_bb_standardized$pre_calibrated,
     xlab = "原始预测概率", ylab = "校准后概率",
     main = "等值回归校准效果")
abline(0, 1, col = "red", lty = 2)  # 理想校准线


# 安装必要的包
install.packages("ModelMetrics")
library(ModelMetrics)

# 计算Brier得分
brier_original <- brier(train_bb_standardized$Status, train_bb_standardized$pre)
brier_calibrated <- brier(train_bb_standardized$Status, train_bb_standardized$pre_calibrated)

# 输出Brier得分
cat("原始Brier得分:", brier_original, "\n")
cat("校准后Brier得分:", brier_calibrated, "\n")
test_bb$pre<-predict(Lgs,newdata = test_bb,type='response')
brier_testlgs <- brier(test_bb$Status, test_bb$pre)









####在测试集中的表现
test_bb$pre<-predict(Lgs,newdata = test_bb,type='response')
bb_Lgs_roc.test<-roc(Status~pre,test_bb)
p1<-plot(Lgs_roc,col="skyblue",legacy.axes=T,print.auc=TRUE)
bb.p1.test<-plot(bb_Lgs_roc.test,col="skyblue",legacy.axes=T)

res.num<- reportROC(gold=test_bb$Status,#金标准
                    predictor=test_bb$pre,#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num
####RF
####随机森林绘制
####将所有变量放进RF
library(caret)
library(tidyverse)
library(mlr3verse)
library(randomForest)
library(ggplot2)
if(file.exists('D/rf_grid.rda')){
  rf_grid <- readRDS("D/rf_grid.rda")
} else {
  # Create model with default parameters
  trControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
  
  # 根据经验或感觉设置一些待查询的参数和参数值
  tuneGrid <- expand.grid(mtry=c(2,3,5,10,15,17))
  # 设置随机数种子，使得结果可重复
  seed <- 1
  set.seed(seed)
  
  rf_grid <- train(x=bb_RF[,-18], y=bb_RF$Status, method="rf", 
                   tuneGrid = tuneGrid, # 随机15个参数值或参数值组合
                   metric="Accuracy", #metric='Kappa'
                   trControl=trControl)
  
}
print(rf_grid)
# Manual Search
trControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# 用默认值固定mtry

tunegrid <- expand.grid(mtry=c(3))

# 定义模型列表，存储每一个模型评估结果
modellist <- list()
# 调整的参数是决策树的数量
for (ntree in c(500,700, 800, 1000)) {
  set.seed(seed)
  fit <- train(x=bb_RF[,-18], y=bb_RF$Status, method="rf", 
               metric="Accuracy", tuneGrid=tunegrid, 
               trControl=trControl, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
##调参结束，选择参数拟合模型
set.seed(123)
bb_RF<-train_bb[(2:21)]
bb.modelrf <-randomForest(Status ~ .,data = bb_RF)
bb.modelrf
importance(bb.modelrf)
varImpPlot(bb.modelrf,pch=19,main = "Importance of variable") 
b<- brier(RF_data$Status, RF_data$pre[,1])
b2 <- brier(test_bb$Status, test_bb$pre_rf)
# 输出Brier得分
cat("原始Brier得分:", brier_original, "\n")
cat("校准后Brier得分:", brier_calibrated, "\n")
test_bb$pre<-predict(Lgs,newdata = test_bb,type='response')
brier_testlgs <- brier(test_bb$Status, test_bb$pre)
library(pROC)
roc <- roc(train_bb$Status, prediction_prob[,1])
library(ggplot2)
prediction_prob <- predict(bb.RF_TRAIN, newdata=train_bb, type="prob")
library(pROC)
roc <- roc(train_bb$Status, prediction_prob[,1])
roc
prediction_prob <- predict(bb.RF_TRAIN, newdata=test_bb, type="prob")
roc <- roc(test_bb$Status, prediction_prob[,1])
roc
library(Boruta)
set.seed(123)
install.packages("extrafont")
library(extrafont)
font_import(pattern="Times New Roman")
tiff("RF-imp.tiff", width = 8, height = 6, units = "in", res = 300,compression = "lzw")
options(scipen = 2)
windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)
bb.modelrf$importance%>%
  as.data.frame()%>%
  rownames_to_column(var='Predictor')%>%
  mutate(Predictor=fct_reorder(Predictor, MeanDecreaseGini))%>%
  mutate(Predictor = recode(Predictor, 
                            "GDSF" = "GDS-15 score",  # 替换 
                            "Education" = "Education","AD8"="AD8 score",
                            "Hearing"="Hearing problem","CHDaddecg"="Coronary heart disease",
                            "Hyperlipoidemia"="Hyperlipemia","PSQIA"="PSQI score",
                            "EGFR"="Impaired kidney function","Gender"="Sex","Fatty.liver"="Fatty liver",
                            "Drink"="Alcohol drinking","Senile.Osteoarthritis"="Osteoarthritis","PD"="Parkinson disease"
  )) %>% 
  ggplot(aes(x=Predictor,y= MeanDecreaseGini,fill= MeanDecreaseGini))+
  geom_col(show.legend = FALSE,fill = "#3C3777")+
  coord_flip()+
  labs(title='Variable Importance in Random Forest')+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "A")) 
dev.off()


###rftrain计算预测值
####选择变量重要性前11名的变量MeanDecreaseGini>=10

RF_TRAIN<-randomForest(Status ~ Age+AD8+BMI+CHDaddecg+Education+GDSF+Hearing+Hyperlipoidemia+Stroke+Hypertension+PSQIA,data = train_bb)
RF_TRAIN
plot(RF_TRAIN,main = "randomforset origin")
library(ggplot2)
train_bb$prr<- predict(RF_TRAIN, newdata=train_bb, type="prob")
rf_roc <- roc(train_bb$Status, train_bb$prr[,2])
bb.p2<-plot(rf_roc,col="lightgreen",legacy.axes=T,print.auc=T,print.thres=TRUE)
rf_roc <- roc(bb_RF$Status, bb_RF$pre[,2])
RF_TRAIN <- glm(Status~ Age+Education+Stroke+AD8+GDSF+EGFR+Hearing, data =train_bb,family=binomial,x=T,y=T  )

test_bb$pre_rf<-predict(RF_TRAIN,newdata = test_bb,type="response")
bb.test_roc_rf<-roc(test_bb$Status, test_bb$pre_rf)
bb.p2.test<-plot(bb.test_roc_rf,col="lightgreen",legacy.axes=T,print.auc=TRUE, print.auc.y = 0.3)



library(reportROC)
res.num<- reportROC(gold=rf$Status,#金标准
                    predictor=rf$pz[,2],#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num
####rftest
library(pROC)
test_bb$pre<-predict(rffit1,newdata = test_bb,type = "prob")
bb.test_roc_rf<-roc(response=test_bb$Status,predictor=test_bb$pre[,2])
bb.p2.test<-plot(bb.test_roc_rf,col="lightgreen",legacy.axes=T,print.auc=TRUE,print.thres=TRUE, print.auc.y = 0.3)
dev.off()
library(reportROC)
res.num<- reportROC(gold=test_bb$Status,#金标准
                    predictor=test_bb$pre[,2],#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num
#####xgb
library(xgboost)
library(Matrix)
xgboost_data<-train_bb[1:21]
names(bb_xgboost)[names(bb_xgboost) == "Hearing problem"] <- "Hearingproblem"
names(bb_xgboost)[names(bb_xgboost) == "GDS-15score"] <- "GDS15score"

names(bb_xgboost)
colnames(bb_xgboost)  # 查看列名
####xgboost实现，将数据转化为稀疏矩阵###先提取变量重要性再拟合
bb.sparse_matrix<-sparse.model.matrix(Status~., ,data =xgboost_data)[,-1]
sparse_matrix<-sparse.model.matrix(Status~ .,data = bb_xgboost)[,-1]


head(bb.sparse_matrix)
######4.产生输出数值型向量
#逐个元素检查Status列中的值是否等于“dead",返回一个逻辑向量
bb.output_vector = xgboost_data[,18]=="1"
######5.模型建立
#使用xgboost() 函数拟合梯度提升树模型
bb.bst<-xgboost(data=bb.sparse_matrix,
                label=bb.output_vector,
                max_depth=4,
                eta =1,
                nthread=2,
                nrounds = 10,
                objective="binary:logistic")
#####6.特征重要性
#####xgb.importance() 函数用于计算特征重要性
######colnames(spare_matrix) 指定特征名称，使用稀疏矩阵的列名作为特征的名称
####指定模型，即之前训练好的梯度提升树模型
bbimportance<-xgb.importance(feature_names = colnames(bb.sparse_matrix),
                             model = bb.bst)
head(bbimportance)
#####
bbimportanceRaw<-xgb.importance(feature_names = colnames(xgboost_data[,-18]),
                                model = bb.bst,
)
#####对原始的重要性进行操作
#：=用于进行列的赋值操作
bbimportanceClean<-bbimportanceRaw[,':='(Cover=NULL,
                                         Frequency=NULL)]
head(bbimportanceClean)
#####7.特征重要性评估可视化
#绘制特征重要性图表
#指定相对于第一个特征的重要性进行比较
bb.data.xgb<-xgb.plot.importance(bbimportanceClean,
                                 rel_to_first = TRUE,
                                 xlab="Gain")
####ggplot美化重要性评估图
windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)

library(ggplot2)
tiff("XGB-imp.tiff", width = 8, height = 6, units = "in", res = 300,compression = "lzw")

windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)
bb.data.xgb <- bb.data.xgb %>%
  mutate(Feature = recode(Feature, 
                          "GDSF" = "GDS-15 score",  # 替换 
                          "Education" = "Education","AD8"="AD8 score",
                          "Hearing"="Hearing problem","CHDaddecg"="Coronary heart disease",
                          "Hyperlipoidemia"="Hyperlipemia","PSQIA"="PSQI score",
                          "EGFR"="Impaired kidney function","Gender"="Sex","Fatty.liver"="Fatty liver",
                          "Drink"="Alcohol drinking","Senile.Osteoarthritis"="Osteoarthritis","PD"="Parkinson disease"))  # 示例: 将 Income 修改为 IncomeLevel

pxgb <- ggplot(data = bb.data.xgb,
               mapping = aes(x = reorder(Feature, Gain),
                             y = Gain,
                             fill = Feature)) +
  geom_bar(stat = 'identity', fill = "#3C3777",
           width = 0.8,
           position = position_dodge(0.7)) +
  scale_fill_brewer(palette = "Set1") + 
  coord_flip() + 
  labs(title = 'Variable Importance in XGBoost Model') +
  ylab("Gain") +  # y轴
  xlab("Predictor") +
  theme_bw() + 
  theme(legend.position = "", 
        plot.title = element_text(hjust = 0.5, family = "A"),
        axis.text = element_text(family = "A"),  # 设置轴文本字体
        axis.title = element_text(family = "A"))
pxgb
dev.off()
# 设置轴标题字体
#####提取前十名
params <- list(eta = 0.5,                    # 学习率, 默认是0.3
               max_depth = 3                   # 每棵树的最大树深, 默认是6
               # objective 默认值是"reg:squarederror", 指定学习任务(回归:reg)和学习目标(squarederror)
               # nthread 默认为最大可用线程数
)



best_xgb_data<-subset(xgboost_data,select = c(Age,Education,PSQIA,Stroke,AD8,GDSF,Hearing,BMI,Status,Hyperlipoidemia,Drink))
###拟合 
sparse_matrix<-sparse.model.matrix(Status~.,data = best_xgb_data)[,-1]
output_vector=best_xgb_data[,9]=="1"
bb.bst<-xgboost(data=sparse_matrix,
                label=output_vector,
                max_depth=4,
                eta =1,
                nthread=2,
                nrounds = 10,
                objective="binary:logistic")
best_xgb_data$pre<-predict(bb.bst,sparse_matrix )
b<- brier(best_xgb_data$Status, best_xgb_data$pre)
b2 <- brier(test_bb$Status, test_bb$pre_rf)
xgboost$pred<- predict(bb.bst,sparse_matrix )
library(pROC)
bb.xgb_train_roc<-roc(best_xgb_data$Status,best_xgb_data$pre)
bb.p3<-plot(bb.xgb_train_roc,col="#3C3777",legacy.axes=T,print.auc=TRUE, print.auc.y = 0.3,print.thres=TRUE)
library(reportROC)
res.num<- reportROC(gold=bb.best_xgb_data$Status,#金标准
                    predictor=bb.best_xgb_data$pred,#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num
#####测试集
xgb_test<-test_bb[(1:21)]
head(pred)
bb.test_matrix <-sparse.model.matrix(Status~.,data =xgb_test)[,-1]
bb.output_vector.test = xgb_test[,18]=="1"
##拟合测试集model
bb.bst.test<-xgboost(data=bb.test_matrix,
                     label=bb.output_vector.test,
                     max_depth=4,
                     eta =1,
                     nthread=2,
                     nrounds = 10,
                     objective="binary:logistic")
dtrain <- xgb.DMatrix(data = as.matrix(train_bb[,-1]), label = train_bb[, 1] ) 
xgb <- xgb.train(params = params, data = sparse_matrix, nrounds = 10, watchlist = watchlist)
xgb_test$pred<-predict(bb.bst.test,newdata=bb.test_matrix)
b<-brier(xgb_test$Status,xgb_test$pred)
library(pROC)
bb.xgb_test_roc<-roc(xgb_test$Status,xgb_test$pred)

bb.p3.test<-plot(bb.xgb_test_roc,col="#3C3777",legacy.axes=T,print.auc=TRUE, print.auc.y = 0.3,print.thres=TRUE)
library(reportROC)
res.num<- reportROC(gold=xgb_t.t$Status,#金标准
                    predictor=xgb_t.t$pred,#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num
#
library(pROC)
library(scales)
library(ggplot2)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A")              
bb.p1<-plot(Lgs_roc.bb,col="skyblue",legacy.axes=T)
bb.p2<-plot(bb.train_roc_rf,col="lightgreen",add=TRUE,legacy.axes=T)             
bb.p3<-plot(bb.xgb_train_roc,col="#3C3777",add=TRUE,legacy.axes=T)
legend("bottomright",legend = c("Logistic AUC=0.89(0.87-0.91) ", "RF AUC=0.96(0.95-0.97)","XGBoost AUC=0.94(0.92-0.95)"), col = c("skyblue", "lightgreen","#3C3777ed"), lwd = 2, bty = "n")
### 生成图的代码
dev.off()

dev.off()
plot()
dev.off()
#########3

library(ggpubr)
library(ggplot2)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A") 
bb.p1.test<-plot(bb_Lgs_roc.test,col="skyblue",legacy.axes=T)
bb.p2.test<-plot(bb.test_roc_rf,col="lightgreen",add=TRUE,legacy.axes=T,print.thres = "best")
bb.p3.test<-plot(bb.xgb_test_roc,col="#3C3777",add=TRUE,legacy.axes=T,print.thres = "best")

legend("bottomright", legend = c("Logistic AUC=0.88(0.85-0.91) ", "RF AUC=0.88(0.84-0.90)","XGBoost AUC=0.95(0.94-0.98)"), col = c("skyblue", "lightgreen","#3C3777ed"), lwd = 2, bty = "n")
#####cal
#######校准曲线
####lgs训练
###Lgs_roc
###如果报错参数无效更新R包
library(caret)
library(Hmisc)
library(rms)
install.packages("calibrate")
library(calibrate)
library(ROCR)

# 使用 calibrate() 函数生成校准曲线对象
bb_cal1<-lrm(Status~ pre , data = bb_lasso,x=TRUE,y=TRUE)
bb.cal1<-calibrate(bb_cal1,method="boot",B=500,m=500)
bb_cal2<-lrm(Status ~pz[,1], data = rf,x=TRUE,y=TRUE)
bb.cal2<-calibrate(bb_cal2, method="boot", B=500,m=500)
bb_cal3<-lrm(Status~pred,data =train_bb,x=TRUE,y=TRUE)
bb.cal3<-calibrate(bb_cal3,method="boot",B=500,m=500)
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A") 
library(riskRegression)
library(ggplot2)
plot(1,
     type = "n",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8,
)
lines(bb.cal1[1:50,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 2, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "skyblue",
) #连线的颜色#2166AC
lines(bb.cal2[1:50,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="lightgreen")
lines(bb.cal3[,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="#3C3777")

abline(0,1,
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444")#对角线的颜色
legend(0.7,0.25,
       c("Apparent","logistic","RF","Xgboost"), 
       lty = c(2,1,1,1), 
       lwd = c(2,3,3,3), 
       col = c("#224444","skyblue","lightgreen","#3C3777"), 
       bty = "n"
)

#####测试集cal

test_lgs
bb_cal1.t<-lrm(Status~ predict , data = test_bb,x=TRUE,y=TRUE)
bb.cal1.t<-calibrate(bb_cal1.t,method = "boot",B=500,m=500)
bb_cal2.t<-lrm(Status~pre[,2],data = test_bb,x=TRUE,y=TRUE)
bb.cal2.t<-calibrate(bb_cal2.t,method = "boot",B=500,m=500)
bb_cal3.t<-lrm(Status~pred,data =best_xgb_data.t,x=TRUE,y=TRUE)
bb.cal33.t<-calibrate(bb_cal3.t,method = "boot",B=500,m=500)
library(riskRegression)
library(ggplot2)
plot(1,
     type = "n",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8
)
lines(bb.cal1.t[,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 2, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "skyblue") #连线的颜色#2166AC
lines(bb.cal2.t[,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="lightgreen")
lines(bb.cal3.t[,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="#3C3777")
abline(0,1,
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444")#对角线的颜色
legend(0.7,0.25,
       c("Apparent","logistic","RF","Xgboost"), 
       lty = c(2,1,1,1), 
       lwd = c(2,3,3,3), 
       col = c("#224444","skyblue","lightgreen","#3C3777"), 
       bty = "n"
)
####HL检验
brier_score = mean((pre[,1]-RF_data$Status)^2)
install.packages("ResourceSelection")
library("ResourceSelection")
bb_lasso$Status<-as.numeric(as.factor(bb_lasso$Status))
hoslem.test(bb_lasso$Status,bb_lasso$pre, g=10)
rf$Status<-as.factor(rf$Status)

hoslem.test(rf$Status,rf$pz[,2], g=10)
##使用等值回归再校准
#####等值回归
# 加载必要的包
rm(list=ls())
install.packages("isotone")  # 如尚未安装，请先安装
library(isotone)
setwd("D:/R/")
# 假设train_bb_standardized中包含模型预测概率和真实标签
# pre: 模型的预测概率
# Status: 真实的二分类结果 (0或1)
# 1. 对数据进行排序，确保预测概率单调递增
library(pROC)
Lgs <- glm(Status~ Age+Education+Drink+Stroke+AD8+GDSF+EGFR+Hearing, data =train_bb,family=binomial,x=T,y=T  )
test_bb$pre<-predict(Lgs,newdata = test_bb,type='response')
test_bb<-test_bb[order(test_bb$pre), ]
train_bb_standardized <- train_bb_standardized[order(train_bb_standardized$pre), ]
isotone_model <- isoreg(test_bb$pre, test_bb$Status)

# 3. 对x值添加微小扰动，避免重复值
unique_x <- jitter(isotone_model$x, factor = 1e-7)  # 添加极小的扰动
# 执行等值回归（等值回归对预测概率进行单调校准）
isotone_model <- isoreg(train_bb_standardized$pre, train_bb_standardized$Status)
# 对预测概率进行校准
pre_cal <- approx(
  x = unique_x, 
  y = isotone_model$yf, 
  xout = test_bb$pre
)$y
rm(pre_cal)
# 将校准后的概率保存到数据集中
test_bb$pre.post<-pre_cal
train_bb_standardized$pre_calibrated <- pre_cal
# 查看校准前后的概率对比
head(data.frame(
  原始预测概率 = test_bb$pre, 
  校准后概率 = test_bb$pre.post
))
# 去重x值，确保唯一性
unique_index <- !duplicated(isotone_model$x)
unique_x <- isotone_model$x[unique_index]
unique_y <- isotone_model$yf[unique_index]
# 检查unique_x和unique_y
print(data.frame(unique_x, unique_y))
# 执行插值
pre_cal <- approx(
  x = unique_x,
  y = unique_y,
  xout = test_bb$pre,
  rule = 2  # 允许外推，防止边界值问题
)$y
# 检查校准后的概率
summary(pre_cal)
test_bb$pre.post <- pre_cal
# 检查预测概率和真实标签
summary(train_bb_standardized$pre)
table(train_bb_standardized$Status)

# 检查是否有缺失值
sum(is.na(train_bb_standardized$pre))
sum(is.na(train_bb_standardized$Status))
# 原始概率 vs 校准后概率
plot(train_bb_standardized$pre, train_bb_standardized$pre_calibrated,
     xlab = "原始预测概率", ylab = "校准后概率",
     main = "等值回归校准效果")
abline(0, 1, col = "red", lty = 2)  # 理想校准线


# 安装必要的包
install.packages("ModelMetrics")
library(ModelMetrics)

# 计算Brier得分
brier_original <- brier(test_bb$Status, test_bb$pre)
brier_calibrated <- brier(test_bb$Status, test_bb$pre.post)

# 输出Brier得分
cat("原始Brier得分:", brier_original, "\n")
cat("校准后Brier得分:", brier_calibrated, "\n")
test_bb$pre<-predict(Lgs,newdata = test_bb,type='response')
brier_testlgs <- brier(test_bb$Status, test_bb$pre)









######dca
#######DCA曲线

library(rmda)
set.seed(123)
bb_dca<-bb_lasso
bb_dca[names(bb_dca)[1:12]]<-lapply(bb_dca[names(bb_dca)[1:12]],as.numeric)
bb_dca$Status <- ifelse(bb_dca$Status == 1, 0, 1)
fit1 <- decision_curve(Status ~ pre, # R语言里常见的公式类型
                       data = bb_lasso, 
                       study.design = "cohort", # 选择研究类型
                       bootstraps = 50 # 重抽样次数
)

plot_decision_curve(fit1, curve.names = "logistic",
                    cost.benefit.axis = F, # 是否需要损失：获益比 轴
                    confidence.intervals = "none" # 不画可信区间
)
set.seed(1234)
bb_dca_RF<-bb_RF
bb_dca_xg[names(bb_dca_xg)[1:12]]<-lapply(bb_dca_xg[names(bb_dca_xg)[1:12]],as.numeric)
bb_dca_xg$Status <- ifelse(bb_dca_xg$Status == 1, 0, 1)
fit2<-decision_curve(Status~Age+AD8+PSQIA+BMI+Stroke+CHDaddecg+Education+Hypertension+EGFR+GDSF, data = bb_dca_RF,study.design = "cohort", # 选择研究类型
                     bootstraps = 50)
bb_dca_xg<-bb.best_xgb_data
fit3<-decision_curve(Status~pred,data =bb_dca_xg,bootstraps = 50)
fit4<-decision_curve(Status~Age+CHDaddecg+Education+Drink+Stroke+AD8+GDSF+EGFR+Senile.Osteoarthritis+Hearing , data=bb_svm,bootstraps = 50)

plot_decision_curve(list(fit1, fit2,fit3,fit4),
                    curve.names = c("logistic","RF","Xgboost","SVM"), 
                    xlim = c(0, 1), # 可以设置x轴范围
                    legend.position = "topright", # 图例位置,
                    col = c("skyblue","lightgreen","#3C3777","tomato"), # 自定义颜色
                    confidence.intervals = "none",
                    lty = c(1,2,1,1), # 线型，注意顺序
                    lwd = c(3,2,2,1) # 注意顺序，先是自己的2个模型，然后是All,然后是None
)
summary(tt)

####测试集
library(rmda)
set.seed(1234)
test_dca<-test_bb
test_dca[names(test_dca)[2:25]]<-lapply(test_dca[names(test_dca)[2:25]],as.numeric)

test_lgs$Status<-as.numeric(test_lgs$Status)
test_dca$Status <- ifelse(test_dca$Status == 1, 0, 1)
fit1.t <- decision_curve(Status~ predict, # R语言里常见的公式类型
                         data = test_dca, 
                         study.design = "cohort", # 选择研究类型
                         bootstraps = 50 # 重抽样次数
)

plot_decision_curve(fit1.t, curve.names = "logistic",
                    cost.benefit.axis = F, # 是否需要损失：获益比 轴
                    confidence.intervals = "none" # 不画可信区间
)
set.seed(1234)
fit2.t<-decision_curve(Status ~ Age+AD8+PSQIA+BMI+Stroke+CHDaddecg+Education+Hypertension+EGFR+GDSF, data = test_dca,study.design = "cohort", # 选择研究类型
                       bootstraps = 50)
fit3.t<-decision_curve(Status~Age+Education+PSQIA+Stroke+AD8+GDSF+Hypertension+EGFR+Hearing,data =test_dca,bootstraps = 50)
fit4.t<-decision_curve(Status~Age+CHDaddecg+Education+Drink+Stroke+AD8+GDSF+EGFR+Senile.Osteoarthritis+Hearing  , data=test_dca,bootstraps = 50)

plot_decision_curve(list(fit1.t, fit2.t,fit3.t,fit4.t),
                    curve.names = c("logistic","RF","Xgboost","SVM"), 
                    xlim = c(0, 1), # 可以设置x轴范围
                    legend.position = "topright", # 图例位置,
                    col = c("skyblue","lightgreen","#3C3777","tomato"), # 自定义颜色
                    confidence.intervals = "none",
                    lty = c(1,2,1,1), # 线型，注意顺序
                    lwd = c(3,2,2,1) # 注意顺序，先是自己的2个模型，然后是All,然后是None
)
######
#######校准曲线绘制
install.packages("predtools")
library(predtools)
library(ggplot2)
library(caret)
library(Hmisc)
library(rms)
install.packages("calibrate")
library(calibrate)
install.packages("ROCR")
library(ROCR)
rf_random <- train(x=bb_RF[,-18], y=bb_RF[,18], method="rf", 
                   tuneLength = 15, # 随机15个参数值或参数值组合
                   metric="Accuracy", #metric='Kappa'
                   trControl=trControl)
print(rf_random)
install.packages("dcurves")
###训练集
library(dcurves)
train_bb$Logistic<-predict(bbmultiLgs,newdata = train_bb,type='response')
train_bb$RF<-predict(rffit1,newdata =train_bb,type = "prob")
train_bb$RF<-train_bb$RF[,2]
train_bb$XGBoost<-predict(bb.bst,sparse_matrix )
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A") 
dcurves::dca(Status ~ Logistic + RF+XGBoost,
             data = train_bb
) %>% 
  plot(smooth = T,
       show_ggplot_code = T # 显示ggplot2代码，方便大家自己调整
  )+
  theme(text = element_text(family = "A"))  # 更改字体为Newman
####测试集
test_bb$Logistic<-predict(bbmultiLgs,newdata = test_bb,type='response')
test_bb$RF<-test_bb$pzrf[,2]
test_bb$XGBoost<-predict(bb.bst.test,bb.test_matrix)
dcurves::dca(Status ~ Logistic + RF+XGBoost,
             data = test_bb
) %>% 
  plot(smooth = T,
       show_ggplot_code = T # 显示ggplot2代码，方便大家自己调整
  )+
  theme(text = element_text(family = "A"))  # 更改字体为Newman
##Nested CV
rm()
install.packages("caret")
install.packages("e1071")    # SVM 支持（Logistic 可选）
install.packages("randomForest") # RF 模型
install.packages("xgboost")  # XGBoost 模型
install.packages("pROC")     # AUC 计算
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
###
bb<-read.csv("2018data.csv")
bb<-bb[,c(-1,-23)]
bb$Gender<-as.factor(bb$Gender)
set.seed(123) # 保证结果可重复
# 将分类标签改为有效名称
bb$Status <- factor(bb$Status , 
                    levels = c("0", "1"), 
                    labels = c("NC", "Dementia"))

# 检查修改后的分类标签
levels(iris_binary$Species)

# 外层交叉验证（5 折）
outer_folds <- createFolds(bb$Status, k = 5)

# 存储结果
results <- data.frame(
  OuterFold = integer(),
  Model = character(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# 遍历外层交叉验证
for (i in seq_along(outer_folds)) {
  
  # 外层训练集和测试集划分
  test_indices <- outer_folds[[i]]
  train_data <- bb[-test_indices, ]
  test_data <- bb[test_indices, ]
  
  # 内层交叉验证控制参数
  inner_control <- trainControl(
    method = "cv",        # 内层交叉验证
    number = 3,           # 内层 3 折
    classProbs = TRUE,    # 输出概率
    summaryFunction = twoClassSummary # 用 AUC 评估模型
  )
  
  # 模型 1：Logistic 回归
  lr_model <- train(
    Status ~ .,          # 公式
    data = train_data,    # 外层训练集
    method = "glm",       # Logistic 回归
    family = "binomial",
    trControl = inner_control
  )
  
  # 模型 2：随机森林（RF）
  rf_grid <- expand.grid(
    mtry = c(1, 2, 3) # 随机森林的调参范围
  )
  rf_model <- train(
    Status ~ ., 
    data = train_data,
    method = "rf",        # 随机森林
    trControl = inner_control,
    tuneGrid = rf_grid
  )
  
  # 模型 3：XGBoost
  xgb_grid <- expand.grid(
    nrounds = 100,        # 树数量固定
    max_depth = c(2, 3, 4), # 树的最大深度
    eta = c(0.1, 0.3),    # 学习率
    gamma = 0,            # 最小分裂损失
    colsample_bytree = 0.8, # 列采样比例
    min_child_weight = 1, # 最小叶子节点权重
    subsample = 0.8       # 样本采样比例
  )
  xgb_model <- train(
    Status ~ .,
    data = train_data,
    method = "xgbTree",   # XGBoost
    trControl = inner_control,
    tuneGrid = xgb_grid
  )
  
  # 在外层测试集上评估每个模型的 AUC
  models <- list(Logistic = lr_model, RF = rf_model, XGBoost = xgb_model)
  for (model_name in names(models)) {
    model <- models[[model_name]]
    predictions <- predict(model, test_data, type = "prob")
    auc <- roc(test_data$Status, predictions[, 2], levels = rev(levels(test_data$Status)))$auc
    
    # 存储结果
    results <- rbind(results, data.frame(OuterFold = i, Model = model_name, AUC = auc))
  }
}
print(lr_model$finalModel) # Logistic 回归最终模型
print(rf_model$bestTune)   # 随机森林的最佳参数
print(xgb_model$bestTune)  # XGBoost 的最佳参数

# 查看结果
print(results)
library(pROC)
##在外部训练集上进行单因素逻辑回归和多因素逻辑回归
summary(bb)
#####单因素logistics
table2<- data.frame()
for(l in c(21:1)){
  result <- glm(train_sta$Status~train_sta[,l],train_sta,family=binomial())
  sm <- as.data.frame(summary(result)$coe)
  con <- as.data.frame((confint(result)))
  out <- cbind(names(train_sta)[l],sm[2,1],exp(sm[-1,1]),sm[-1,4],exp(con[-1,]))
  table2 <- rbind(out,table2)}
table2$`sm[2,1]` <- round(sm[2, 1], 3)
table2$`exp(sm[-1, 1])`<- as.numeric(table2$`exp(sm[-1, 1])`)
table2$`sm[-1, 4]`<- as.numeric(table2$`sm[-1, 4]`)
table2$`2.5 %`<- as.numeric(table2$`2.5%`)
table2$`97.5 %`<- as.numeric(table2$`97.5 %`)
table2$'OR'<-paste0(round(table2$`exp(sm[-1, 1])`,2),
                    "(",
                    round(table2$`2.5 %`,2),
                    "~",
                    round(table2$`97.5 %`,2),
                    ")")
table2$`sm[-1, 4]`<-round(table2$`sm[-1, 4]`,5)
# 重新排列列的顺序：包括自变量名、β系数、OR值、置信区间、p值
table2 <- table2[, c(1,2,8,4)]

train_data$Status <- factor(train_data$Status , 
                            levels = c("NC", "Dementia"), 
                            labels = c("0", "1"))
train_sta<-train_data
train_sta[, 2] <- scale(train_data[, 2])
##多因素logistic回归
multitable2<- data.frame()
MODEL2<-glm(Status ~ Age +Gender +Education+Drink+Stroke+AD8+Smoke+GDSF+EGFR+PSQIA+Hearing+Hyperlipoidemia+CHDaddecg+PD+Diabetes,  data = train_sta, family = binomial())
summary(MODEL2)
library(questionr)
odds.ratio(MODEL2)
###LASSO回归P<0.05的纳入LASSO和多变量分析
library(glmnet)
library(foreign)#阅读文件
lasso<-subset(train_sta,select = c(Age,Gender,Education,CHDaddecg,Stroke,AD8,Drink,GDSF,PSQIA,Hearing,Hyperlipoidemia,Status,Diabetes,Smoke,EGFR,PD))
library(dplyr)
library(glmnet)
library(MASS)
library(survival)
library(rms)
CandidateVariables <- c("Age","Gender","Education","Drink","Stroke","AD8","GDSF","EGFR","PSQIA","Hearing","Hyperlipoidemia","PD","Diabetes","Smoke","CHDaddecg")
tmp.y <- lasso$Status
tmp.x <- model.matrix(~.,lasso[CandidateVariables])
#开始构建模型
f2 = glmnet(tmp.x, tmp.y, family="binomial", nlambda=100, alpha=1,cv=10)
#可视化f1
tiff("lasso-left.tiff", width = 8, height = 6, units = "in", res = 300,compression = "lzw")

windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)
custom_labels <- c("Age", "Sex", "Education","Alcohol drinking","Stroke","AD8 score","GDS-15 score"
                   ,"Impaired kidney function","PSQI score","Hearing problem","Hyperlipemia","Parkinson disease","Current smoking","Coronary heart disease")  # 自定义标签

# 绘制 f2 图

plot(f2, xvar = "lambda", label = TRUE)
dev.off()
# 修改 legend 中的标签
legend("topright", 
       legend = custom_labels,  # 使用自定义的标签
       lty = 1, 
       col = 1:ncol(tmp.x), 
       cex = 0.8)
dev.off()
#进行交叉验证
set.seed(1234)
cv.model2 <- cv.glmnet(tmp.x, tmp.y, family="binomial", alpha=1,nfolds = 10)
tiff("Lasso-right.tiff", width = 8, height = 6, units = "in", res = 300,compression = "lzw")
windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)
plot(cv.model2)
dev.off()
cv.model2$lambda.min
min<-cv.model2$lambda.min
log.lambda.min <- log(min)
print(log.lambda.min)
cv.model2$lambda
coef(cv.model2, s=cv.model2$lambda.min) 
cv.model2$lambda.1se
coef(cv.model2, s=cv.model2$lambda.1se) 
cv.model2$lambda.1se
se<-cv.model2$lambda.1se
log.lambda.se<- log(se)
print(log.lambda.se)
###7个变量，
##Age               0.56028594
#Education1       -0.16148453
#Stroke1           0.31044902
#AD81              1.39754995
#GDSF1             1.52070797
#EGFR1             0.21653826
###Hearing1          0.09126301####
cv.model2$lambda.1se
#0.009734848；
print(log.lambda.se)
# -4.632043
#ML变量重要性
##最佳参数print(rf_model$bestTune)   # 随机森林的最佳参数
#mtry   3
print(xgb_model$bestTune)  # XGBoost 的最佳参数
#nrounds max_depth eta gamma colsample_bytree
#1     100         2 0.1     0              0.8
#min_child_weight subsample
#1                1       0.8
library(caret)
library(tidyverse)
library(mlr3verse)
library(randomForest)
library(ggplot2)
set.seed(1234)
trControl = inner_control
bb.modelrf <-randomForest(Status ~ ., ,mtry=3,data = train_sta)
set.seed(123) 
importance(bb.modelrf)
varImpPlot(bb.modelrf,pch=19,main = "Importance of variable") 
##选取前十名
library(dplyr)
library(tibble)
tiff("RF-imp.tiff", width = 8, height = 6, units = "in", res = 300,compression = "lzw")
options(scipen = 2)
windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)
bb.modelrf$importance%>%
  as.data.frame()%>%
  rownames_to_column(var='Predictor')%>%
  mutate(Predictor=fct_reorder(Predictor, MeanDecreaseGini))%>%
  mutate(Predictor = recode(Predictor, 
                            "GDSF" = "GDS-15 score",  # 替换 
                            "Education" = "Education","AD8"="AD8 score",
                            "Hearing"="Hearing problem","CHDaddecg"="Coronary heart disease",
                            "Hyperlipoidemia"="Hyperlipemia","PSQIA"="PSQI score",
                            "EGFR"="Impaired kidney function","Gender"="Sex","Fatty.liver"="Fatty liver",
                            "Drink"="Alcohol drinking","Senile.Osteoarthritis"="Osteoarthritis","PD"="Parkinson disease"
  )) %>% 
  ggplot(aes(x=Predictor,y= MeanDecreaseGini,fill= MeanDecreaseGini))+
  geom_col(show.legend = FALSE,fill = "#3C3777")+
  coord_flip()+
  labs(title='Variable Importance in Random Forest')+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "A")) 
dev.off()
#XGBoost
library(Matrix)
dtrain<-xgb.DMatrix(data.matrix(train_sta[, -which(names(train_sta) == "Status")]),label=train_sta$Status)
fit<-xgb.train(params=list(learning_rate=0.1),data=dtrain,nrounds=100,max_depth=2)
bbimportance<-xgb.importance(feature_names = colnames(dtrain),
                             model =fit)
head(bbimportance)
#####
output_vector = train_sta[,18]=="1"
bbimportanceRaw<-xgb.importance(feature_names = colnames(train_sta[,-18]),
                                model = fit,
)
#####对原始的重要性进行操作
#：=用于进行列的赋值操作
bbimportanceClean<-bbimportanceRaw[,':='(Cover=NULL,
                                         Frequency=NULL)]
head(bbimportanceClean)
#####7.特征重要性评估可视化
#绘制特征重要性图表
#指定相对于第一个特征的重要性进行比较
xgb.plot.importance(bbimportanceClean,
                    rel_to_first = TRUE,
                    xlab="Gain")
####ggplot美化重要性评估图
windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)

library(ggplot2)
bb.data.xgb<-as.data.frame(bbimportanceClean)
pxgb<-ggplot(data=bb.data.xgb,
             mapping=aes(x=reorder(Feature,Gain),
                         y=Gain,
                         fill=Feature))+
  geom_bar(stat = 'identity',fill="#3C3777",
           width = 0.8,
           position = position_dodge(0.7))+
  scale_fill_brewer(palette = "Set1")+ 
  coord_flip()+labs(title='Variable Importance in XGBoost Model')+
  ylab("Variable relative importance")+ ##y轴
  xlab("Predictor")+
  theme_bw()+ 
  theme(legend.position = "",plot.title = element_text(hjust=0.5,family="A"),
        axis.text = element_text(family = "A"),  # 设置轴文本字体
        axis.title = element_text(family = "A"))
pxgb
tiff("XGB-imp.tiff", width = 8, height = 6, units = "in", res = 300,compression = "lzw")

windowsFonts(
  A=windowsFont("Times New Roman"))

par(mar = c(8,5,8,5),family='A',cex = 0.8)
bb.data.xgb <- bb.data.xgb %>%
  mutate(Feature = recode(Feature, 
                          "GDSF" = "GDS-15 score",  # 替换 
                          "Education" = "Education","AD8"="AD8 score",
                          "Hearing"="Hearing problem","CHDaddecg"="Coronary heart disease",
                          "Hyperlipidemia"="Hyperlipemia","PSQIA"="PSQI score",
                          "EGFR"="Impaired kidney function","Gender"="Sex","Fatty.liver"="Fatty liver",
                          "Drink"="Alcohol drinking","Senile.Osteoarthritis"="Osteoarthritis","PD"="Parkinson disease"))  # 示例: 将 Income 修改为 IncomeLevel

pxgb <- ggplot(data = bb.data.xgb,
               mapping = aes(x = reorder(Feature, Gain),
                             y = Gain,
                             fill = Feature)) +
  geom_bar(stat = 'identity', fill = "#3C3777",
           width = 0.8,
           position = position_dodge(0.7)) +
  scale_fill_brewer(palette = "Set1") + 
  coord_flip() + 
  labs(title = 'Variable Importance in XGBoost Model') +
  ylab("Gain") +  # y轴
  xlab("Predictor") +
  theme_bw() + 
  theme(legend.position = "", 
        plot.title = element_text(hjust = 0.5, family = "A"),
        axis.text = element_text(family = "A"),  # 设置轴文本字体
        axis.title = element_text(family = "A"))
pxgb
dev.off()
####绘制ROC曲线
# 外层交叉验证（5 折）
outer_folds <- createFolds(bb$Status, k = 10)

# 存储结果
results <- data.frame(
  OuterFold = integer(),
  Model = character(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# 遍历外层交叉验证
for (i in seq_along(outer_folds)) {
  
  # 外层训练集和测试集划分
  set.seed(123)
  test_indices <- outer_folds[[i]]
  train_data <- bb[-test_indices, ]
  test_data <- bb[test_indices, ]
  
  # 内层交叉验证控制参数
  inner_control <- trainControl(
    method = "cv",        # 内层交叉验证
    number = 3,           # 内层 3 折
    classProbs = TRUE,    # 输出概率
    summaryFunction = twoClassSummary # 用 AUC 评估模型
  )
  
  # 模型 1：Logistic 回归
  lr_model <- train(
    Status ~ Age+Education+Stroke+AD8+GDSF+EGFR+Hearing,          # 公式
    data = train_data,    # 外层训练集
    method = "glm",       # Logistic 回归
    family = "binomial",
    trControl = inner_control
  )
  
  # 模型 2：随机森林（RF）
  rf_grid <- expand.grid(
    mtry = c(1, 2, 3)
    
    
    # 随机森林的调参范围
  )
  rf_model <- train(
    Status ~ Age+GDSF+AD8+Stroke+Hearing+BMI+Education+Diabetes+CHDaddecg+Hypertension, 
    data = train_data,
    method = "rf",        # 随机森林
    trControl = inner_control,
    tuneGrid = rf_grid,
    ntree=100
    
  )
  
  # 模型 3：XGBoost
  xgb_grid <- expand.grid(
    nrounds = 100,        # 树数量固定
    max_depth = c(2,3,4,5), # 树的最大深度
    eta = c(0.1, 0.2,0.3),    # 学习率
    gamma = 0,            # 最小分裂损失
    colsample_bytree = 0.8, # 列采样比例
    min_child_weight = 1, # 最小叶子节点权重
    subsample = 0.8       # 样本采样比例
  )
  xgb_model <- train(
    Status ~ Age+AD8+GDSF+Stroke+EGFR+Education+Hearing+Diabetes+Drink+Hypertension,
    data = train_data,
    method = "xgbTree",   # XGBoost
    trControl = inner_control,
    tuneGrid = xgb_grid
  )
  
  # 在外层测试集上评估每个模型的 AUC
  models <- list(Logistic = lr_model, RF = rf_model, XGBoost = xgb_model)
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    # 训练集的预测
    predictions_train <- predict(model, train_data, type = "prob")
    auc_train <- roc(train_data$Status, predictions_train[, 2], levels = rev(levels(train_data$Status)))$auc
    
    # 测试集的预测
    predictions_test <- predict(model, test_data, type = "prob")
    auc_test <- roc(test_data$Status, predictions_test[, 2], levels = rev(levels(test_data$Status)))$auc
    
    # 存储结果
    results <- rbind(results, data.frame(
      OuterFold = i, 
      Model = model_name, 
      AUC_Train = auc_train,  # 训练集 AUC
      AUC_Test = auc_test     # 测试集 AUC
    ))
  }
}
print(results)
# 计算每个模型的训练集 AUC 均值
avg_auc_lr <- mean(results$AUC_Train[results$Model == "Logistic"])
avg_auc_rf <- mean(results$AUC_Train[results$Model == "RF"])
avg_auc_xgb <- mean(results$AUC_Train[results$Model == "XGBoost"])

# 打印每个模型的训练集 AUC 均值
print(paste("Average AUC_Train for Logistic:", avg_auc_lr))
print(paste("Average AUC_Train for RF:", avg_auc_rf))
print(paste("Average AUC_Train for XGBoost:", avg_auc_xgb))

avg_auc_lr.t <- mean(results$AUC_Test[results$Model == "Logistic"])
avg_auc_rf.t <- mean(results$AUC_Test[results$Model == "RF"])
avg_auc_xgb.t <- mean(results$AUC_Test[results$Model == "XGBoost"])

print(paste("Average AUC_Test for Logistic:", avg_auc_lr.t))
print(paste("Average AUC_Test for RF:", avg_auc_rf.t))
print(paste("Average AUC_Test for XGBoost:", avg_auc_xgb.t))

print(lr_model$finalModel) # Logistic 回归最终模型
print(rf_model$bestTune)   # 随机森林的最佳参数
print(xgb_model$bestTune)  # XGBoost 的最佳参数
plot(roc_test_all)
library(ggplot2)
library(pROC)
library(dplyr)
###绘制ROC曲线
train_data$p1<-predict(lr_model,train_data, type = "prob")
lr_roc<-roc(Status~p1[,2],train_data)
bb.p1<-plot(lr_roc,col="#015493",legacy.axes=T,print.auc=T,print.thres=TRUE)##添加截点和95%CI
train_data$p2<-predict(rf_model,train_data, type = "prob")
rf_roc<-roc(Status~p2[,2],train_data)
bb.p2<-plot(rf_roc,col="#019092",legacy.axes=T,print.auc=T,print.thres=TRUE)##添加截点和95%CI
###
train_data$p3<-predict(xgb_model,train_data, type = "prob")
xgb_roc<-roc(Status~p3[,2],train_data)
p3<-plot(xgb_roc,col="#F4A99B",legacy.axes=T,print.auc=T,print.thres=TRUE)
tiff("ROCnew.tiff", width = 8, height = 6, units = "in", res = 600,compression = "lzw")
windowsFonts(
  A=windowsFont("Times New Roman"))
# 重置所有图形参数

par(mar = c(8,5,8,5),family='A',cex = 0.8)           
# 添加背景网格
# 添加背景网格
bb.p1 <- plot(lr_roc, col="#015493", legacy.axes=T, asp=1)
grid()  # 显示背景网格
bb.p2 <- plot(rf_roc, col="#019092", add=TRUE, legacy.axes=T)
bb.p3 <- plot(xgb_roc, col="#F4A99B", add=TRUE, legacy.axes=T)

# 绘制图例并添加边框
legend("bottomright", legend = c("LR", "RF", "XGBoost"), col = c("#015493", "#019092", "#F4A99B"), lwd = 2, bty = "o", box.lwd = 2)
dev.off()
# 查看与其他包冲突的函数
conflicts(detail = TRUE)
remove.packages("reportROC")

# 卸载 reportROC

rm(reportROC)
library(reportROC)
rm(res.num)
tiff("ROCnew2.tiff", width = 6, height = 6, units = "in", res = 600,compression = "lzw")

res.num<- reportROC(gold=train_data$Status,#金标准
                    predictor=train_data$p3[,2],#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
dev.off()
res.num
###验证集
test_data$pz<-predict(lr_model,test_data, type = "prob")
lr_roc.t<-roc(Status~pz[,2],test_data)
bb.p1<-plot(lr_roc.t,col="#015493",legacy.axes=T,print.auc=T,print.thres=TRUE)##添加截点和95%CI
test_data$px<-predict(rf_model,test_data, type = "prob")
rf_roc.t<-roc(Status~px[,2],test_data)
bb.p2<-plot(rf_roc.t,col="#019092",legacy.axes=T,print.auc=T,print.thres=TRUE)##添加截点和95%CI
###
test_data$pc<-predict(xgb_model,test_data, type = "prob")
xgb_roc.t<-roc(Status~pc[,2],test_data)
p3<-plot(xgb_roc.t,col="#F4A99B",legacy.axes=T,print.auc=T,print.thres=TRUE)
tiff("ROC-update2.tiff", width = 6, height = 6, units = "in", res = 300,compression = "lzw")
windowsFonts(
  A=windowsFont("Times New Roman"))
options(scipen = 20)
par(mar = c(8,5,8,5),family='A',cex = 0.8)           
# 添加背景网格
# 添加背景网格
bb.p1 <- plot(lr_roc.t, col="#015493", legacy.axes=T, asp=1)
grid()  # 显示背景网格
bb.p2 <- plot(rf_roc.t, col="#019092", add=TRUE, legacy.axes=T)
bb.p3 <- plot(xgb_roc.t, col="#F4A99B", add=TRUE, legacy.axes=T)

# 绘制图例并添加边框
legend("bottomright", legend = c("LR", "RF", "XGBoost"), col = c("#015493", "#019092", "#F4A99B"), lwd = 2, bty = "o", box.lwd = 2)
dev.off()
install.packages("reportROC")
library(reportROC)
res.num<- reportROC(gold=test_data$Status,#金标准
                    predictor=test_data$pc[,2],#预测变量
                    important="sp",#  选择截断值时候哪个更重要
                    plot=TRUE,#绘制ROC曲线
                    xlab = "1-specificity",
                    ylab = "sensitivity")
res.num

###校准曲线
library(ggplot2)
library(caret)
library(pROC) # 用于 AUC
names(P1)[names(P1) == "NC"] <- "0"
names(P1)[names(P1) == "Dementia"] <- "1"
train_data$Status <- factor(train_data$Status , 
                            levels = c("NC", "Dementia"), 
                            labels = c("0", "1"))
test_data$Status <- factor(test_data$Status , 
                           levels = c("NC", "Dementia"), 
                           labels = c("0", "1"))
# 定义函数：计算校准曲线数据
P1<-predict(lr_model,train_data, type = "prob")
lr_roc<-roc(Status~p1[,2],train_data)
bb.p1<-plot(lr_roc,col="#015493",legacy.axes=T,print.auc=T,print.thres=TRUE)##添加截点和95%CI
train_data$p2<-predict(rf_model,train_data, type = "prob")
rf_roc<-roc(Status~p2[,2],train_data)
bb.p2<-plot(rf_roc,col="#019092",legacy.axes=T,print.auc=T,print.thres=TRUE)##添加截点和95%CI
###
train_data$p3<-predict(xgb_model,train_data, type = "prob")
xgb_roc<-roc(Status~p3[,2],train_data)
###
library(rms)
install.packages("xfun")
dd<-datadist(tmp)
options(datadist="dd")
fit2<-lrm(Status~Age+Education+Stroke+AD8+GDSF+EGFR+Hearing+Drink,
          data=train_data,x=T,y=T)
cal2<-calibrate(fit2,method='boot',B=500)
fit3<-lrm(Status ~ Age+GDSF+AD8+Stroke+Hearing+BMI+Education+Diabetes+CHDaddecg+Hypertension,
          data=train_data,x=T,y=T)
cal3<-calibrate(fit3,method='boot',B=500)
fit4<-lrm(Status ~ Age+AD8+GDSF+Stroke+EGFR+Education+Hearing+Diabetes+Drink+Hypertension,
          data=train_data,x=T,y=T)
cal4<-calibrate(fit4,method='boot',B=500)
tiff("CAL-NEW.tiff", width = 6, height = 6, units = "in", res = 300,compression = "lzw")

windowsFonts(
  A=windowsFont("Times New Roman"))
options(scipen = 20)
par(mar = c(8,5,8,5),family='A',cex = 0.8)    

plot(1,
     type = "n",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8,
)
grid() 
lines(cal2[1:50,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 2, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "#015493",
) #连线的颜色#2166AC
lines(cal3[1:50,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="#019092")
lines(cal4[,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="#F4A99B")

abline(0,1,
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444")#对角线的颜色
legend(0.7,0.4,
       c("Apparent","LR","RF","XGBoost"), 
       lty = c(2,1,1,1), 
       lwd = c(2,3,3,3), 
       col = c("#224444","#015493","#019092","#F4A99B"), 
       bty = "O",
       xpd=TRUE
)
dev.off()
##测试集
fitz<-lrm(Status~Age+Education+Stroke+AD8+GDSF+EGFR+Hearing+Drink,
          data=test_data,x=T,y=T)
calz<-calibrate(fitz,method='boot',B=500)
fitx<-lrm(Status ~ Age+GDSF+AD8+Stroke+Hearing+BMI+Education+Diabetes+CHDaddecg+Hypertension,
          data=test_data,x=T,y=T)
calx<-calibrate(fitx,method='boot',B=500)
fitc<-lrm(Status ~ Age+AD8+GDSF+Stroke+EGFR+Education+Hearing+Diabetes+Drink+Hypertension,
          data=test_data,x=T,y=T)
calc<-calibrate(fitc,method='boot',B=500)
tiff("CAL-NEWtest.tiff", width = 6, height = 6, units = "in", res = 300,compression = "lzw")

windowsFonts(
  A=windowsFont("Times New Roman"))
options(scipen = 20)
par(mar = c(8,5,8,5),family='A',cex = 0.8)    

plot(1,
     type = "n",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8,
)
grid() 
lines(calz[1:50,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 2, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "#015493",
) #连线的颜色#2166AC
lines(calx[1:50,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="#019092")
lines(calc[,c("predy","calibrated.corrected")],type="l",pch=16,lwd=2,col="#F4A99B")

abline(0,1,
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444")#对角线的颜色
legend(0.7,0.4,
       c("Apparent","LR","RF","XGBoost"), 
       lty = c(2,1,1,1), 
       lwd = c(2,3,3,3), 
       col = c("#224444","#015493","#019092","#F4A99B"), 
       bty = "O",
       xpd=TRUE
)
dev.off()
# 计算Brier得分
predicted_probs <- predict(fitc, newdata = test_data, type = "fitted")
actual_values <- test_data$Status
predicted_probs <- as.numeric(as.character(predicted_probs))
actual_values <- as.numeric(as.character(actual_values))
brier_score <- mean((predicted_probs-actual_values)^2)
print(brier_score)
##计算校准后的
library(isotone)
library(ModelMetrics)
brier_original <- brier(test_data$Status, predicted_probs)
brier_calibrated <- brier(test_data$Status, test_data$pz[,2])
library(pROC)
Lgs <- glm(Status~ Age+Education+Drink+Stroke+AD8+GDSF+EGFR+Hearing, data =train_bb,family=binomial,x=T,y=T  )
test_bb$pre<-predict(Lgs,newdata = test_bb,type='response')
test_bb<-test_bb[order(test_bb$pre), ]
train_bb_standardized <- train_bb_standardized[order(train_bb_standardized$pre), ]
isotone_model <- isoreg(train_data$p1[,2], train_data$Status)

# 3. 对x值添加微小扰动，避免重复值
unique_x <- jitter(isotone_model$x, factor = 1e-7)  # 添加极小的扰动
# 执行等值回归（等值回归对预测概率进行单调校准）
isotone_model <- isoreg(train_bb_standardized$pre, train_bb_standardized$Status)
# 对预测概率进行校准
pre_cal <- approx(
  x = unique_x, 
  y = isotone_model$yf, 
  xout = train_data$p1[,2]
)$y
rm(pre_cal)
# 将校准后的概率保存到数据集中
train_data$pre.post<-pre_cal
train_bb_standardized$pre_calibrated <- pre_cal
# 查看校准前后的概率对比
head(data.frame(
  原始预测概率 = train_data$p1[,2], 
  校准后概率 = train_data$pre.post
))
# 去重x值，确保唯一性
unique_index <- !duplicated(isotone_model$x)
unique_x <- isotone_model$x[unique_index]
unique_y <- isotone_model$yf[unique_index]
# 检查unique_x和unique_y
print(data.frame(unique_x, unique_y))
# 执行插值
pre_cal <- approx(
  x = unique_x,
  y = unique_y,
  xout = train_data$p1[,2],
  rule = 2  # 允许外推，防止边界值问题
)$y
# 检查校准后的概率
summary(pre_cal)
train_data$pre.post <- pre_cal
# 检查预测概率和真实标签
summary(train_bb_standardized$pre)
table(train_bb_standardized$Status)

# 检查是否有缺失值
sum(is.na(train_bb_standardized$pre))
sum(is.na(train_bb_standardized$Status))
# 原始概率 vs 校准后概率
plot(train_bb_standardized$pre, train_bb_standardized$pre_calibrated,
     xlab = "原始预测概率", ylab = "校准后概率",
     main = "等值回归校准效果")
abline(0, 1, col = "red", lty = 2)  # 理想校准线


# 安装必要的包
install.packages("ModelMetrics")
library(ModelMetrics)

# 计算Brier得分
brier_original <- brier(test_bb$Status, test_bb$pre)
brier_calibrated <- brier(train_data$Status, train_data$pre.post)
library(dcurves)
library(ggplot2)
test_data$LR<-test_data$pz[,2]
test_data$RF<-test_data$px[,2]
test_data$XGBoost<-test_data$pc[,2]
tiff("DCA-NEWtest.tiff", width = 6, height = 6, units = "in", res = 300,compression = "lzw")

windowsFonts(A=windowsFont("Times New Roman"))
par(family="A") 
dcurves::dca(Status ~ LR + RF+XGBoost,
             data = test_data                      
)%>% 
  plot(smooth = T,
       show_ggplot_code = T # 显示ggplot2代码，方便大家自己调整
  ) +scale_color_manual(values = c(
    "Treat All" = "#d62728", # 红色（Treat ALL）
    "Treat None" = "black" ,# 紫色（Treat None）
    "LR" = "#015493",        # 蓝色
    "RF" = "#019092",        # 橙色
    "XGBoost" = "#F4A99B" ))+   # 绿色# 设置线条的粗细，size控制线条粗细
  theme(text = element_text(family = "A"))  # 更改字体为Newman
dev.off()
formanceation
