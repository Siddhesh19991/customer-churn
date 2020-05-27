churn <- read.csv("~/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv")

churn$gender<-as.factor(churn$gender)
churn$SeniorCitizen<-as.factor(churn$SeniorCitizen)
churn$Partner<-as.factor(churn$Partner)
churn$Dependents<-as.factor(churn$Dependents)
churn$PhoneService<-as.factor(churn$PhoneService)
churn$MultipleLines<-as.factor(churn$MultipleLines)
churn$InternetService<-as.factor(churn$InternetService)
churn$OnlineBackup<-as.factor(churn$OnlineBackup)
churn$OnlineSecurity<-as.factor(churn$OnlineSecurity)
churn$DeviceProtection<-as.factor(churn$DeviceProtection)
churn$TechSupport<-as.factor(churn$TechSupport)
churn$StreamingTV<-as.factor(churn$StreamingTV)
churn$StreamingMovies<-as.factor(churn$StreamingMovies)
churn$Contract<-as.factor(churn$Contract)
churn$PaperlessBilling<-as.factor(churn$PaperlessBilling)
churn$PaymentMethod<-as.factor(churn$PaymentMethod)
churn$Churn<-as.factor(churn$Churn)


summary(churn)
library(DataExplorer)
plot_intro(churn)

#no outliers

#missing values
library(mice)
impute<-mice(churn)
churn<-complete(impute,3)

#EDA
ggplot(churn,aes(Churn))+geom_bar()+facet_grid(~gender)
ggplot(churn,aes(Churn))+geom_bar()+facet_grid(~SeniorCitizen)
ggplot(churn,aes(Churn))+geom_bar()+facet_grid(~Partner)
ggplot(churn,aes(Churn))+geom_bar()+facet_grid(~Dependents)
prop.table(table(churn$Dependents,churn$Churn),1)
prop.table(table(churn$OnlineSecurity,churn$Churn),1)
prop.table(table(churn$InternetService,churn$Churn),1)
ggplot(churn,aes(tenure))+geom_histogram(bins=30)+facet_grid(~Churn)
ggplot(churn,aes(Churn,MonthlyCharges))+geom_boxplot()
ggplot(churn,aes(Churn,TotalCharges))+geom_boxplot()
#from visualizing the data we can make a customer profile for those who are most likey to churn
#old citizens,no partners,not dependent people , those whose use fibre optics and no internet service.






#split the data
library(caret)
intrain<-createDataPartition(y=churn$Churn,p=0.7,list=FALSE)
train<-churn[intrain,]
test<-churn[-intrain,]
train<-train[,-1]

#model
model1<-glm(Churn~.,data = train,family = "binomial")
pred1<-predict(model1,newdata = train,type="response")
pred1_<-ifelse(pred1>0.5,1,0)

test$Churn<-ifelse(test$Churn=="Yes",1,0)
train$Churn<-ifelse(train$Churn=="Yes",1,0)
train$Churn<-as.factor(train$Churn)
confusionMatrix(pred1_,train$Churn)

library(ROCR)
a<-prediction(pred1,train$Churn)
eva<-performance(a,"acc")
#0.5 is a good cutoff

library(rpart.plot)
library(rpart)
model2<-rpart(Churn~.,data=train,method="class")
pred2<-predict(model2,newdata = train,type = "class")
confusionMatrix(as.factor(pred1_),train$Churn)

library(randomForest)
model3<-randomForest(Churn~.,data=train)
pred3<-predict(model3,newdata = train)
confusionMatrix(pred3,train$Churn)#best model

final<-predict(model3,newdata = test)
confusionMatrix(final,as.factor(test$Churn))
#accuracy of 80% 