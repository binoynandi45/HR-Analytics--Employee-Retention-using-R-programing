#Loading Files
setwd("C:\\Users\\aditya\\Desktop\\Logit Training\\Attrition")
Attrition<-read.csv("Attrition_Data Import.csv")
View(Attrition)

# CreCreditScore$MonthlyIncome,na.rm = TRUE))
# CreditScore$NumberOfDependents[is.na(CreditScore$NumberOfDependents)]<- round(mean(CreditScditScore$MonthlyIncome[is.na(CreditScore$MonthlyIncome)]<- round(mean(ore$NumberOfDependents,na.rm = TRUE))


install.packages("summarytools")
library(summarytools)
dfSummary(Attrition)

colnames(Attrition)
library(stringr)
colnames(Attrition)

colnames(Attrition)<-str_replace_all(colnames(Attrition),"[.]","")

#colnames(Attrition)<-gsub("[[:punct:]]","",colnames(Attrition))

#install.packages("caret")

colnames(Attrition)
install.packages("dummies")
library(dummies)
dummified<-dummy.data.frame(Attrition[,c(5:9)])
View(dummified)
CombinedSet<-cbind(dummified,Code=Attrition$Code,Experience=Attrition$ExperienceMonths)
View(CombinedSet)
colnames(CombinedSet)
dfSummary(CombinedSet)


library(caret)
Train<-createDataPartition(CombinedSet$Code,p = 0.30,list = FALSE)
FinalTrain<-CombinedSet[Train,]
FinalTest<-CombinedSet[-Train,]
View(FinalTrain)
View(FinalTest)
colnames(FinalTrain)

library(nnet)
Logit1<- multinom(Code~.,family = binomial,data = FinalTrain)
summary(Logit1)

install.packages("ROCR")
library(ROCR)

FinalTrain$Code<-as.numeric(FinalTrain$Code)

predict1<-predict(Logit1,FinalTrain,type = "prob")
predict2<-ifelse(predict1>0.,1,0)

pred1<-prediction(predict1,FinalTrain$Code)

perf<-performance(pred1,"tpr","fpr")

plot(perf)

Table<-table(predict2,FinalTrain$Code)

install.packages("e1071")
library(e1071)
confusionMatrix(Table,positive = "1")

Validate<-read.csv("Validation Set.csv")
View(Validate)
colnames(Validate)<-str_replace_all(colnames(Validate),"[.]","")

dummified1<-dummy.data.frame(Validate[,c(5:9)])
CombinedSet1<-cbind(dummified1,Code=Validate$Code,Experience=Validate$ExperienceMonths)

Valipredict<-predict(Logit1,CombinedSet1,type = "prob")
Valipredict1<-ifelse(Valipredict>=0.5,"Attrite","Stay")
