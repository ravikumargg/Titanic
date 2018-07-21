setwd("C:/Users/Ravi/Desktop/Kaggle case-study/titanic")
train=read.csv("train.csv",header = T,sep = ",")
View(train)

###### Remove outliers from data set
#remove outliers by doing h clustering
# run this model again and again untill you remove outliers
sub=data.frame(scale(train[,c(2,3,5,6,7,8,10,12)]))
hcls=hclust(dist(sub))
plot(hcls)
train$cluster=cutree(hcls,2)
# remove outliers
train=train[train$cluster!=2,]


#getmode= function(v,na.rm = TRUE){
#uniqv = unique(v)
#uniqv[which.max(tabulate(match(v, uniqv)))]
#}

train$Age=round(train$Age)

#library(modeest)
#mlv(train$Age, method = "mfv",na.rm = TRUE)

# mode function
myMode <- function(x, na.rm =T) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
myMode(train$Age)

train$Age[is.na(train$Age)] =myMode(train$Age)
train$Embarked[is.na(train$Embarked)] =myMode(train$Embarked)


newtrain= train[,c('Pclass','Sex','Age','SibSp','Survived','Parch','Fare','Embarked')]
View(newtrain)

newtrain$Survived = as.factor(newtrain$Survived)


#####Decision Tree model

library(rpart)
rmodel=rpart(Survived~.,data=newtrain)
plot(rmodel) 
text(rmodel)

library(randomForest)
tpred=predict(rmodel,data = newtrain,type = "class")
tpred
train$tpred=tpred


table(tpred,train$Survived)


test=read.csv("test.csv",header = T,sep = ",")
test$Age[is.na(test$Age)] = round(mean(test$Age, na.rm = TRUE))
View(test)
newtest= test[,c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked')]
View(newtest)

test$Age=round(test$Age)
myMode <- function(x, na.rm =T) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
myMode(test$Age)

test$Age[is.na(test$Age)] =myMode(test$Age)
test$Fare[is.na(test$Fare)] =myMode(test$Fare)

pred=predict(rmodel,newdata = newtest,type = "class")
pred
test$Survived=pred 

summary(pred)
 


write.csv(test,"C:/Users/Ravi/Desktop/Kaggle case-study/titanic/result/final2.csv")
   
library(rattle)  
rattle()


##### svm model

library(e1071)
svmmodel=svm(Survived~.,data=newtrain,kernel="radial",type="C-classification")
summary(svmmodel)

# tune data
tunesvm=tune(svm,Survived~.,data=newtrain,kernel="radial",type="C-classification",ranges = list(gamma=c(.01,.1,.2,0),cost=(1:10)))
summary(tunesvm)

# to see missing values
Num_NA= sapply(htrain,function(y)length(which(is.na(y)==T)))
NA_Count= data.frame(Item=colnames(htrain),Count=Num_NA)
NA_Count

# caret package
library(caret)
trctrl=trainControl(method = "repeatedcv",number = 10,repeats = 3)
set.seed(3233)

#svm 
svm_model=train(Survived~.,data=newtrain,method="svmploynomial",trControl=trctrl,preProcess=c("center","scale"),tuneLenth=10)
svm_model
summary(svm_model)
predtic=predict(svm_model,data=newtrain)
predtic

#Accuracy
confusionMatrix(factor(predtic),factor(train$Survived))


testpred=predict(svm_linear,newdata =test,na.rm=T)
test$Survived=testpred


best_model=tunesvm$best.model   
best_model

pred_titanic=predict(svmmodel,data=train)
train$pred_titanic=pred_titanic

library(MASS)
library(Matrix)
library(Metrics)
rmse(train$Survived,pred_titanic)

test$pred_titanic=0
#newdata = factor(newdata, levels = c(std levels))
testpred=predict(best_model,newdata=test)

test$Survived=testpred
write.csv(test,"C:/Users/Ravi/Desktop/Kaggle case-study/titanic/result/svmfinal1.csv")
