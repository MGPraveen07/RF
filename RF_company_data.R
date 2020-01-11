library(readr)
Company_Data <- read_csv("C:/Users/Admin/Desktop/Assignments/Decision_tree/Company_Data.csv")
View(Company_Data)
summary(Company_Data)
attach(Company_Data)
Company_Data$ ShelveLoc<- as.factor(Company_Data$ ShelveLoc)
Company_Data$Urban<-as.factor(Company_Data$Urban)
Company_Data$US<- as.factor(Company_Data$US)

Company_Data$Sales1<- cut(Company_Data$Sales,seq(0,16,5),right=FALSE, labels=c("low","medium","high"))
Company_Data$Sales1
Company_Data<- as.data.frame(Company_Data)
str(Company_Data)

Company_Data_train<- Company_Data[2:300,]
Company_Data_test<- Company_Data[301:400,]

library(randomForest)
Company_Data_forest <- randomForest(Sales1 ~ .,Company_Data_train[,-1], na.action=na.roughfix,importance=TRUE)


mean(Company_Data_train$Sales1==predict(Company_Data_forest,Company_Data_train))

predc5.0_test <- predict(Company_Data_forest,newdata=Company_Data_test) # 
mean(predc5.0_test==Company_Data_test$Sales1)

# training accurecy
CM=table(predc5.0_test,Company_Data_test$Sales1)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
library(gmodels)
# Cross tablez
CrossTable(Company_Data_test$Sales1,predc5.0_test)
