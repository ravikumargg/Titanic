setwd("C:/Users/Ravi/Desktop/Kaggle case-study/titanic")
train=read.csv("train.csv",header = T,sep = ",")
View(train)

train$Age=round(train$Age)

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


newtrain= train[,c('Pclass','Sex','Age','SibSp','Survived')]
#View(newtrain)
#,'Parch','Fare','Embarked'

newtrain$Survived = as.factor(newtrain$Survived)

# Logistic Reggresion

fit=glm(Survived~.,data=newtrain, family = binomial())
fit
summary(fit)

library(MASS)
sfit=stepAIC(fit)

train$pred=predict(fit,type='response')
train$survivedp=ifelse(train$pred>0.4556,1,0)

# confusion matrix
table(train$survived)
table(train$survivedp,train$Survived)

library(ROCR)
predrocr=prediction(train$pred,train$Survived)

roc.perf=performance(predrocr,measure = "tpr",x.measure = "fpr")
plot(roc.perf,colorize=T)
auc=performance(predrocr,measure="auc")
auc@y.values[[1]]

dist=rep(9999,length(roc.perf@x.values[[1]]))
for (i in 1:length((roc.perf@x.values[[1]]))){
  cur_x=roc.perf@x.values[[1]][i]
  cur_y=roc.perf@y.values[[1]][i]
  #  cur_alpha=roc.perf@alpha.values[[1]][i]
  dist[i]=(0-cur_x)*(0-cur_x)+(1-cur_y)*(1-cur_y)
}

ideal_cutoff=roc.perf@alpha.values[[1]][dist==min(dist)]  
ideal_cutoff

#Loistic reggression model result

test=read.csv("test.csv",header = T,sep = ",")
test$Age[is.na(test$Age)] = getmode(test$Age, na.rm = TRUE)
test$Embarked[is.na(test$Embarked)] =getmode(test$Embarked)

View(test)
newtest= test[,c('Pclass','Sex','Age','SibSp','Parch','Fare','Embarked')]
View(newtest)


test$predict=pred 

table(pred,train$Survived)  

summary(pred)


library(write.csv)
write.csv(test,"C:/Users/Ravi/Desktop/titanic/result/final1.csv",header = T,sep = ",")

