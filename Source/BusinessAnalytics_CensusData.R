#Setting the Working Directory

setwd("D:\\data analytics\\R_Business_Analytics-master")

library(rpart)
library(ROCR)

Data = read.csv("D:\\data analytics\\R_Business_Analytics-master\\Source\\Dataset\\CensusData.csv")

Data[Data == ' ?'] = NA         

GoodData = na.omit(Data) 

GoodData = data.frame(GoodData)

head(GoodData)
tail(GoodData)
str(GoodData)
summary(GoodData)

Bp1 = boxplot(GoodData$age ~ GoodData$Income, xlab = "Income", ylab = "Age")
Bp2 = boxplot(GoodData$education.num ~ GoodData$Income, xlab = "Income", ylab = "No. of Educated People")
Bp3 = boxplot(GoodData$fnlwgt ~ GoodData$Income, xlab = "Income", ylab = "No. of Census Takers")
Bp4 = boxplot(GoodData$hours.per.week ~ GoodData$Income, xlab = "Income", ylab = "No. of hours per week")

Bp1$stats 
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "age"] > 73)
  {
    GoodData[i, "age"] = 73
  }
}


Mean = mean(GoodData$education.num)
Sd = sd(GoodData$education.num)
MinLimit = Mean - 2 * Sd
MaxLimit = Mean + 2 * Sd
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "education.num"] > MaxLimit)
  {
    GoodData[i, "education.num"] = MaxLimit
  }
  else if(GoodData[i, "education.num"] < MinLimit)
  {
    GoodData[i,"education.num"] = MinLimit
  }
}


Mean = mean(GoodData$fnlwgt)
Sd = sd(GoodData$fnlwgt)
MinLimit = Mean - 2 * Sd
MaxLimit = Mean + 2 * Sd
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "fnlwgt"] > MaxLimit)
  {
    GoodData[i, "fnlwgt"] = MaxLimit
  }
  else if(GoodData[i, "fnlwgt"] < MinLimit)
  {
    GoodData[i,"fnlwgt"] = MinLimit
  }
}


Mean = mean(GoodData$hours.per.week)
Sd = sd(GoodData$hours.per.week)
MinLimit = Mean - 2 * Sd
MaxLimit = Mean + 2 * Sd
for(i in 1:nrow(GoodData))
{
  if(GoodData[i, "hours.per.week"] > MaxLimit)
  {
    GoodData[i, "hours.per.week"] = MaxLimit
  }
  else if(GoodData[i, "hours.per.week"] < MinLimit)
  {
    GoodData[i,"hours.per.week"] = MinLimit
  }
}

GoodData_0 = subset(GoodData, Income == " <=50K")
GoodData_1 = GoodData[GoodData$Income == ' >50K',]

round(nrow(GoodData_0)*0.8,0)
round(nrow(GoodData_1)*0.8,0)

TrainData_0 = GoodData_0[1:(round(nrow(GoodData_0)*0.8,0)),]
TestData_0 = GoodData_0[((round(nrow(GoodData_0)*0.8,0))+1):nrow(GoodData_0),]

TrainData_1 = GoodData_1[1:(round(nrow(GoodData_1)*0.8,0)),]
TestData_1 = GoodData_1[((round(nrow(GoodData_1)*0.8,0))+1):nrow(GoodData_1),]

TestData = rbind(TestData_0, TestData_1)
TrainData = rbind(TrainData_0, TrainData_1)

table(TestData$Income)
table(TrainData$Income)
table(GoodData$Income)


DecisionTree = rpart(Income ~ age + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours.per.week + native.country, data = TrainData)

DecisionTree_Predict = predict(DecisionTree, TestData, type = "class")

CombinedTree = cbind(TestData$Income, DecisionTree_Predict)
colnames(CombinedTree) = c("Actual", "Predicted")
CombinedTree = data.frame(CombinedTree)

table(CombinedTree$Actual, CombinedTree$Predicted)

Prediction_Tree = prediction(as.numeric(DecisionTree_Predict), as.numeric(TestData$Income))
Performance_Tree = performance(Prediction_Tree, "tpr", "fpr")
plot(Performance_Tree)

LogisticReg = glm(Income ~ age + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours.per.week + native.country, data = TrainData, family="binomial")
LogisticReg_Predict = predict(LogisticReg, TestData, type = "response")
LogisticReg_Predict = round(LogisticReg_Predict,0)

CombinedData = cbind(TestData$Income, LogisticReg_Predict)
colnames(CombinedData) = c("Actual", "Predicted")
CombinedData = data.frame(CombinedData)

table(CombinedData$Actual, CombinedData$Predicted)

Prediction_Reg = prediction(as.numeric(LogisticReg_Predict), as.numeric(TestData$Income))
Performance_Reg = performance(Prediction_Reg, "tpr", "fpr")
plot(Performance_Reg)
