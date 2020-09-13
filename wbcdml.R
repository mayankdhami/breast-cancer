library(class)
library(dplyr)
library(gmodels)
library(caret)
library(randomForest)

cancer = read.csv("data/data.csv", stringsAsFactors = F)
str(cancer)
cancer = cancer[,c(-1,-33)]
table(cancer$diagnosis)
summary(cancer)
cancer$diagnosis = factor(cancer$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(cancer$diagnosis))*100, 1)
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5))
cancer_n = as.data.frame(lapply(cancer[2:31], normalize))
summary(cancer_n$area_mean)
cancer_n$diagnosis = cancer$diagnosis
#Making training and test data
set.seed(123)
cancer.survival <- cancer_n$diagnosis %>% 
  createDataPartition(p = 0.70, list = FALSE)
cancer_train = cancer_n[cancer.survival,]
cancer_test = cancer_n[-cancer.survival,]
round(prop.table(table(cancer_train$diagnosis))*100, 1)
round(prop.table(table(cancer_test$diagnosis))*100, 1)
cancer_train_labels = cancer_train[,31]
cancer_test_labels = cancer_test[,31]
cancer_test_pred = knn(cancer_train[,-31], cancer_test[,-31], cancer_train_labels, 3)
CrossTable(cancer_test_labels, cancer_test_pred, prop.chisq = F)
confusionMatrix(cancer_test_pred,cancer_test_labels)

rf1 = randomForest(diagnosis~.,cancer_train, method = "class", mtry = 2)
rf1
importance(rf1)
rfpred = predict(rf1,cancer_test[,-31])
confusionMatrix(rfpred,cancer_test_labels)

library(e1071)
svmodel = svm(diagnosis~.,cancer_train)
svmodel
svmpred = predict(svmodel,cancer_test[,-31])
confusionMatrix(rfpred,cancer_test_labels)
