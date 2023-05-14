install.packages('ROCR')

#dataset preparation
data_forest_per<-perConvictions %>% 
  filter((Town=='Avon and Somerset' | Town=='Bedfordshire'))

data_forest <- data_forest_per
data_forest$Town <- as.factor(data_forest$Town) 
dim(data_forest)
str(data_forest)
library(randomForest)
set.seed(71)
rf <-randomForest(Town~.,data=data_forest, ntree=500) 
print(rf)
floor(sqrt(ncol(data_forest) - 1))

#Checking the Mtry value to select the best
mtry <- tuneRF(data_forest[-1],data_forest$Town, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(71)
rf <-randomForest(Town~.,data=data_forest, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)


pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], data_forest$Town)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

rf_confusion<-rf$confusion

#Accuracy calculation
classify_acc<- sum(diag(rf_confusion)/sum(rf_confusion)*100)
classify_acc
