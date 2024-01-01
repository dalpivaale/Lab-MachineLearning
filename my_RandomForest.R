ct<- read.table("BreastCancer_complete.dat",stringsAsFactors=TRUE)
library(randomForest)
set.seed(123)
idx = sample(nrow(ct),400)#divido il dataset in training e testset
trn = ct[idx,]
tst = ct[-idx,]

rf <- randomForest(class~.-id , data=trn, importance=T, ntree=2000) # does not work with missing data. Dei 500 trees assegna la classe del soggetto prendendo la maggioranza dei voti di ogni albero
rf #si noti l'out of error pari al 2.75%
varImpPlot(rf)#bare nuclei unifsize unifshape sono le features più importanti
pred_rf <- predict(rf,tst)
x<-table(pred_rf,tst$class)#confusion matrix
library(caret)
confusionMatrix(x)#spettacolo accuracy 0.96 e specificity,sensitivity al top
plot(rf$err.rate[,1], type='l',xlim=c(0,100)) # OOB error vs. number of trees. Tra 30 e 50 best number of tree
#pred_rf <- predict(rf,tst, type="response")

#qua faccio la ROC curve
library(pROC)
predll <- predict(rf,tst, type="prob")
lroc <- roc(tst$class ~ predll[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.993 specificity 0.969, sensitivity 1.00 !!!

#qua faccio un pò di tuning cmq l'algoritmo lavora da dio con mtry e ntree qualsiasi praticamente. Come plot gli puoi mettere l'OOB comparato a ntree e la ROC curve
rf <- randomForest(class~.-id , data=trn, importance=T, ntree=2000, mtry = 3)
pred_rf <- predict(rf,tst)
x<-table(pred_rf,tst$class)#confusion matrix
confusionMatrix(x) # accuracy 0.97, sensitivity 0.97, specificity = 0.95
plot(rf$err.rate[,1], type='l',xlim=c(0,1000))
predll <- predict(rf,tst, type="prob")
lroc <- roc(tst$class ~ predll[,2],  plot=T, print.auc=T, print.thres="best") #0.994 ancora meglio dell'altro. con nTree = 200 e mtry = 1 ancora meglio.

#SUPER FIGO in ste righe trova direttamente il best mtry e ntree! Evita un ciclo for. fa direttamente un tuning dei parametri di RandomForest
tctrl <- trainControl("oob")#è una corssvalidation consideranrdo l'OOB
rft <- train(class~., data=trn, method='rf', trControl=tctrl, tuneLength=9)
pred_rft <- predict(rft$finalModel, tst)
rft
rft$finalModel #ntree = 500 e mtry = 2 sono il meglio
confusionMatrix(pred_rft, tst$class) # 0.987
#e poi puoi predirre con tale top modello. 
pred_rft_p <- predict(rft, tst, type="prob")
myroc <- roc(tst$class ~ pred_rft_p[,2], plot=T, print.auc=T) # AUC 0.993
auc(myroc) #0.9925
##Vediamo se il CLASSIFICATION TREE fa meglio
library(tree)

set.seed(123)
idx = sample(nrow(ct), floor(0.7*nrow(ct)))#prende il 70% per il trainig data, 30% per il testset
trn = dat[idx,]
tst = dat[-idx,]

#tr <- tree(diab~., data=trn) altri due modelli da confrontare con l'istruzione sotto suggerita dal prof che è uguale ma controlli altri parametri
tr <- tree(class~. -id, data=trn, split="gini")
#tr <- tree(diab~., data=dat, control=tree.control(nobs=nrow(dat),mindev=0.001))#nobs è il numero di osservazioni ovvero quelle di dat. minsize numero minimo di nodi di default è 10 sennò puoi anche cambiarlo tu. ALlena il modello su dat che su trn non sò perchè ma effettivamente è moooolto meglio sul intero dataset
tree_pred <- predict(tr, tst, type="response")#predico il testset col mio albero (il mio modello)
library(caret)
confusionMatrix(tree_pred, tst$class, positive = "malignant") #accuracy 0.966, sensitivity e specificity altissime molto bene anche lui

library(pROC)
myroc <- roc(tst$class ~ pred[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.969 !!! tanta roba ottimo il primo modello. pred[,2] si becca la seconda colonna ovvero le predizioni predette
roc(tst$diab ~ pred[,2],  plot=T, print.auc=F, print.thres=0.15, add=T)
myroc.p <- roc(tst$diab ~ pred.p[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.82 l'albero potato non è top come il primo


#Logistic-regression
ll <- glm(class~., family=binomial, data=trn)
summary(ll)
predll <- predict(ll,tst, type="response")
pred_log_class <- as.factor(ifelse(predll>0.5,"malignant","benign")) #ti serve per fare la confusion matrix
confusionMatrix(pred_log_class, tst$class)
lroc <- roc(tst$class ~ predll,  plot=T, print.auc=T, print.thres="best")#AUC 0.997 forse lui ancora meglio del random Forest


logfit <- glm(class ~ .,data=trn, family=binomial)
summary(logfit)
pred_log<- predict(logfit,tst, type="response")
pred_log_class <- as.factor(ifelse(pred_log>0.5,"malignant","benign"))
confusionMatrix(pred_log_class, tst$class) #0.97
auc(roc(tst$class ~ pred_log, plot=T, add=T, col="blue")) # AUC 0.9935


## Naive Bayes
library(e1071)
nb <- naiveBayes(class~. -id, data=trn)
pred_nb <- predict(nb,tst,type="raw")
confusionMatrix(pred_nb, tst$class, positive="malignant")#accuracy = 0.961, sensitivity = 0.978,specificity = 0.952
nbroc <- roc(tst$class ~ pred_nb[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.987 







