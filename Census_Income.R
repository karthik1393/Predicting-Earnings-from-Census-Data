income<-read.csv("census.csv")
str(income)
set.seed(2000)
split<-sample.split(income$over50k,SplitRatio = 0.6)
income_train<-subset(income,split==TRUE)
income_test<-subset(income,split==FALSE)
##lg
income_lg<-glm(over50k~.,data=income_train,family=binomial)
summary(income_lg)
income_lg_pre<-predict(income_lg,newdata=income_test,type="response")
table(income_test$over50k,income_lg_pre>0.5)
(9051+1888)/nrow(income_test)
table(income_test$over50k)
incomer<-prediction(income_lg_pre,income_test$over50k)
incroc<-performance(incomer,'tpr','fpr')
plot(incroc,colorize = TRUE, text.adj = c(-0.2,1.9))
auc<-as.numeric(performance(incomer,"auc")@y.values)
auc
##CART
income_cart<-rpart(over50k~.,data=income_train,method="class")
prp(income_cart)
income_cart_pre<-predict(income_cart,newdata=income_test)

table(income_test$over50k,income_cart_pre)
(9243+1596)/nrow(income_test)
##ROC
ROC<-prediction(income_cart_pre[,2],income_test$over50k)
iROC<-performance(ROC,'tpr','fpr')
auc=print(as.numeric(performance(ROC,"auc")@y.values))
plot(iROC)
#RF
set.seed(1)
train<-income_train[sample(nrow(income_train),2000),]
set.seed(1)
train_rf<-randomForest(over50k~.,data=train)
train_pre<-predict(train_rf,newdata=income_test)
table(income_test$over50k,train_pre)
(9614+1050)/nrow(income_test)
##to find how many times a variable is used for splitting in RF
va<-varUsed(train_rf,count=TRUE)
va_sort<-sort(va,decreasing = FALSE,index.return=TRUE)
dotchart(va_sort$x,names(train_rf$forest$xlevels[va_sort$ix]))
##impurity
varImpPlot(train_rf)
###CV
set.seed(2)
trc<-trainControl(method = "cv",number=10)
grid<-expand.grid(.cp=seq(0.002,0.1,0.002))
train(over50k~.,data=income_train,method="rpart",trControl=trc,tuneGrid=grid)
##CART with cp value
income_cart1<-rpart(over50k~.,data=income_train,method="class",cp=0.002)
prp(income_cart1)
income_cart_pre1<-predict(income_cart1,newdata=income_test,type="class")

table(income_test$over50k,income_cart_pre1)
(9178+1838)/nrow(income_test)

