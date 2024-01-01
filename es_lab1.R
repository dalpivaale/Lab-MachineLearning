setwd("C\Users\dpale\Desktop\Lab_MachineLearning\CellType")
getwd()
ct <- read.table("CellType")
library(e1071) 
nb.ct <- naiveBayes(Celltype~.,data= ct) # works with NAs!
head(ct)
summary(ct)


library(caret)
#?train
trn.ctrl <- trainControl(method="LOOCV")
trn.ctrl.10 <- trainControl(method="cv",number=10)
nb.train <- train(Celltype~., data=trn,method="nb",trControl=trn.ctrl.10)
nb.train
summary(nb.train)
names(nb.train)
nb.train$bestTune
nb.final <- nb.train$finalModel
pred.final<-predict(nb.final,tst)$class
x <- table(pred.final,tst$diabetes)
confusionMatrix(x)

