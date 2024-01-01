#lab 11 Esercizio Random Forest
###Prima parte del esercizio
data <- read.table("BreastCancer_complete.dat", stringsAsFactors=TRUE)

library(caret)
library(randomForest)
set.seed(3)
idx =sample(nrow(data),450)
trn <- data[idx,]
tst <- data[-idx,]
tr <- randomForest(class~. , data=trn, importance=T, ntree=500)
tr #OOB = 3.33%, bare.nuclei,thickness,unif.size are the most important features
pred <- predict(tr, tst)
pred
confusionMatrix(pred, tst$class) # 0.982 di accuracy kappa quasi a uno specificity e sensituvuty  top
varImpPlot(tr)
# ROC curve
library(pROC)
pred_rf_p <- predict(tr, tst, plot=T, type="prob")
myroc <- roc(tst$class ~ pred_rf_p[,2], plot=T, print.thres="best",print.auc=T) # AUC 0.994
auc(myroc) #0.9942

###Punto3 Compare the results with others models


## logistic model for diabetes
ll <- glm(class~., family=binomial, data=trn)
summary(ll)
predll <- predict(ll,tst, type="response")
lroc <- roc(tst$class ~ predll,  plot=T, print.auc=T, print.thres="best") # AUC=0.996 !!!
auc(lroc)#0.9965

## Naive Bayes
library(klaR)
nb <- NaiveBayes(class ~ .,data=trn)
pred_nb<- predict(nb,tst)
confusionMatrix(pred_nb$class, tst$class) #0.9914
auc(roc(tst$class ~ pred_nb$posterior[,2])) # AUC 0.9957

##Decision Tree
library(tree)
dec_tree <- tree(class~., data=trn)#costruisci il tuo albero, con tutte le possibili covariate
tree_pred <- predict(dec_tree, tst)#predico il testset col mio albero (il mio modello)
library(caret)
confusionMatrix(tree_pred, tst$class)#0.96 devi mettere
library(pROC)
myroc <- roc(tst$class ~ tree_pred[,2], plot=T, print.thres="best",print.auc=T) # AUC 0.994
auc(myroc)#AUC = 0.9841
