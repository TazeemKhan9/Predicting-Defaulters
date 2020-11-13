library(ISLR)
library(MASS)
library(car)
library(caTools)
library(leaps)
library(glmnet)
library(caret)
data<-read.csv("C:/Users/Tazeem/Documents/Notes/SEM 4/SM-1/DATA/Bank train.csv")
str(data)
data$ED<-ordered(data$ED)
data$default<-as.factor(data$default)
sum(is.na(data))
lg.fit<-glm(default~Employ+Address+Debtinc+Creddebt+Age,data=data,family = "binomial")
summary(lg.fit)

set.seed(1)
split=sample.split(data$default,SplitRatio = 0.6)
training_set=subset(data,split==T)##60%
test_set=subset(data,split==F)##40%

lg.fit1<-glm(default~Employ+Address+Debtinc+Creddebt,data=data,family = "binomial")
summary(lg.fit1)
exp<-exp(lg.fit1$coefficients)=
exp ##if this is more than 1 than chanes

y_pred=predict(lg.fit1,newdata = test_set,type = "response")
y_pred

y_pred1<-ifelse(y_pred>0.5,"Good","Bad")
y_pred1<-as.factor(y_pred1)
table(y_pred1,test_set$default)

 ##log odd ratios if there is no response



library(e1071)
trc<-trainControl(method = "repeatedcv",number = 10,savePredictions = TRUE)
mod.fit<-train(default~Employ+Address+Debtinc+Creddebt,data=data,method="glm",family="binomial",trControl=trc)
mod.fit

y_pred<-predict(mod.fit,newdata = test_set)
y_pred
y_pred2<-ifelse(y_pred==1,"Good","Bad")
y_pred2<-as.factor(y_pred2)
table(y_pred,test_set$default)
m1<-confusionMatrix(y_pred,test_set$default)
m1

