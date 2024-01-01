ct<- read.table("Celltype.dat")

idx = sample(nrow(ct),130)#divido il dataset in training e testset
trn = ct[idx,]
tst = ct[-idx,]
library(e1071) #carico il package nel workspace
nb1 <- naiveBayes(CellType~.,data= trn)
pred <-predict(nb1,newdata=tst)
x <- table(pred,tst$CellType)
confusionMatrix(x)#Accuracy 0.88, Kappa 0.82, No information rate = 0.54 non male


library(caret)
trn.ctrl <- trainControl(method="LOOCV")
trn.ctrl.10 <- trainControl(method="cv",number=10)
nb.train <- train(CellType~., data=trn,method="nb",trControl=trn.ctrl.10)#fa una 10 fold cross validation costruendo il nostro modello. tutti e 10 i modelli sono in nb.train
nb.train
nb.train$bestTune
nb.final <- nb.train$finalModel 
pred.final<-predict(nb.final,tst)$CellType #
x <- table(pred.final,tst$CellType)
confusionMatrix(x)

#KNN
library(FNN)
chance <- table(tst$CellType)/nrow(tst)
chance
set.seed(3)
pred2 <- knn(trn[,4:9],tst[,4:9],trn$CellType,k=1) # not scaled! Non mi prendo tutte le covariate perchè lavora solo con quelle numeriche
accuracy = sum(pred2==tst$CellType)/nrow(tst)
accuracy #0.57
x <- table(pred2,tst$CellType)
confusionMatrix(x)
pred3 <- knn(scale(trn[,4:9]),scale(tst[,4:9]),trn$CellType,k=1)
accuracy = sum(pred==tst$CellType)/nrow(tst)
accuracy #0.65  meglio!!
#faccio il tuning del K per capire quale è meglio
KK=nrow(tst)
accuracy=rep(0,KK)

for(k in 1:KK){
  pred <- knn(scale(trn[,4:9]),scale(tst[,4:9]),trn$CellType,k=k)
  accuracy[k] = sum(pred==tst$CellType)/nrow(tst)
}

plot(1:KK,accuracy,type='b',ylim=c(0.5,0.8),xlab='k')#per K = 5 c'è un accuracy di 0.77 ed è quello migliore dal plot
abline(h=max(chance),lty=2) # a 0.4 c'è la linea

# same but not scaling the data. Qua fa la stessa cosa ma coi dati non scalati ed infatti l'accuracy è peggiore. Scala i dati con KNN!!!!
accuracy=rep(KK)
for(k in 1:KK){
  pred <- knn(trn[,4:9],tst[,4:9],trn$CellType,k=k)
  accuracy[k] = sum(pred==tst$CellType)/nrow(tst)
}
lines(1:KK,accuracy,type='l',col='red')

