#soluzione decision tree lab9 professore
data <- read.table("bbd_data.dat", stringsAsFactors=T)

detach(data)
attach(data)
summary(data)
plot(data)
nrow(data)

dat = na.omit(data)


### Classification tree
library(tree)

set.seed(123)
idx = sample(nrow(dat), floor(0.7*nrow(dat)))#prende il 70% per il trainig data, 30% per il testset
trn = dat[idx,]
tst = dat[-idx,]

#tr <- tree(diab~., data=trn) altri due modelli da confrontare con l'istruzione sotto suggerita dal prof che è uguale ma controlli altri parametri
#tr <- tree(diab~., data=trn, split="gini")
tr <- tree(diab~., data=dat, control=tree.control(nobs=nrow(dat),mindev=0.001))#nobs è il numero di osservazioni ovvero quelle di dat. minsize numero minimo di nodi di default è 10 sennò puoi anche cambiarlo tu. ALlena il modello su dat che su trn non sò perchè ma effettivamente è moooolto meglio sul intero dataset
tree_pred <- predict(tr, tst, type="class")#predico il testset col mio albero (il mio modello)
library(caret)
confusionMatrix(tree_pred, tst$diab, positive = "yes") #tutto molto buono, sensitiviy un pò bassa (capacità individuare soggetti malati "yes") poichè ce ne sono pochi

#?tree.control
###Pruning dell'albero
summary(trn$diab) #1565 sani e 153 positivi al diabete
plot(tr)#plot del mio albero da potare
text(tr)
tr
tr.cv <- cv.tree(tr, FUN=prune.tree)#funzione tree della cross validation, considerando la devianza
plot( tr.cv )#size = 6 in avanti la devianza è sempre quella ti conviene prendere una dimensione bassa
plot(cv.tree(tr, FUN=prune.misclass) )#vediamo la size è migliore con il missclass error
tr.cv
#?cv.tree

tr.p <- prune.tree(tr, best=6)#Potiamo l'albero scegliendo size = 6
plot(tr.p)
text(tr.p)
tree_pred2 <- predict(tr.p, tst, type="class")#predico il testset col mio albero potato
library(caret)
confusionMatrix(tree_pred2, tst$diab, positive = "no") #bah in realtà con size = 6 kappa e accuracy perdono..

pred <- predict(tr, tst)
summary(pred)

pred.p <- predict(tr.p,tst)
summary(pred.p)

library(pROC)
myroc <- roc(tst$diab ~ pred[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.969 !!! tanta roba ottimo il primo modello. pred[,2] si becca la seconda colonna ovvero le predizioni predette
roc(tst$diab ~ pred[,2],  plot=T, print.auc=F, print.thres=0.15, add=T)
myroc.p <- roc(tst$diab ~ pred.p[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.82 l'albero potato non è top come il primo

#Salto
pred.cl <- predict(tr, tst, type="class")
table(pred.cl, tst$diab) #fa le confusion matrix dei due modelli, quella dell'albero non potato è meglio

pred.p.cl <- predict(tr.p,tst, type="class")
table(pred.p.cl, tst$diab)

cbind(myroc$thresholds, myroc$sensitivities, myroc$specificities)

#Assa stare sta robetta
pred.cl.best <- as.factor(ifelse(pred[,2]>0.15, "yes", "no"))
table(pred.cl.best, tst$diab)
tst[41:50,]
pred[41:50,] # compare with the tree
tr

library(caret)
confusionMatrix(pred.cl.best, tst$diab, positive="yes")
confusionMatrix(pred.cl, tst$diab, positive="yes")#non guardare solo accuracy e kappa per capire il migliore ma anche specificity sensitivity e accuracy balanced
confusionMatrix(pred.p.cl, tst$diab, positive="yes")


### blood pressure - regression tree: prediciamo una variabile continua della pressione sanguigna
library(tree)

set.seed(123)
idx = sample(nrow(dat), floor(0.7*nrow(dat)))
trn = dat[idx,]
tst = dat[-idx,]

tr <- tree(sysBP~.-diab, data=trn)#costruisci il tuo albero come prima, senza la feature diab: questo è l'albero più piccolo e fa le considerazioni
tr <- tree(sysBP~.-diab, data=trn, control=tree.control(nobs=nrow(dat),mindev=0.001))#poi ritorna qui e usa sto albero che è più grande del precedente giochi sui due parametri di prima

plot(tr)
text(tr)
tr

pred <- predict(tr, tst)
summary(pred)
plot(pred~tst$sysBP, xlim=c(0,120), ylim=c(0,120), col=4)#plotta i valori predetti contro quelli reali (in ascissa), se son predetti bene seguono un retta inclinata a 45 gradi
abline(a=0,b=1)#somma non predice prorpio bene
sqrt(mean((pred-tst$sysBP)^2)) # RMSE 11.4 albero piccolo // 12.9 albero grande: il piccolo predice meglio ha RMSE più basso staltro overfitting

tr.cv <- cv.tree(tr, FUN=prune.tree)
plot( tr.cv, xlim=c(0,10) )
tr0=prune.tree(tr,best=6)#stessa roba di prima best size = 5 o 6
plot(tr0)
text(tr0)
pred0 <- predict(tr0, tst)
summary(pred0)
plot(pred0~tst$sysBP, xlim=c(0,120), ylim=c(0,120))
abline(a=0,b=1)
sqrt(mean((pred0-tst$sysBP)^2)) # RMSE 11.33 dopo aver potato RMSE leggermente più basso 


#### BELOW: Other methods. Try yourself first! Predice con gli atri modelli e guardando AUC o RMSE guarda se fan peggio o meglio
## linear model Qua predice la pressione sanguigna 
lin <- lm(formula(tr), trn)
summary(lin)
pred.lin <- predict(lin,tst)
plot(pred.lin ~tst$sysBP, xlim=c(0,120), ylim=c(0,120))#plot di prima delle predizioni versus i valori reali
abline(a=0,b=1)
sqrt(mean((pred.lin-tst$sysBP)^2)) # RMSE 11.74

# Try to improve 


## logistic model for diabetes Con questi predice la classe
ll <- glm(diab~., family=binomial, data=trn)
summary(ll)
predll <- predict(ll,tst, type="response")
lroc <- roc(tst$diab ~ predll,  plot=T, print.auc=T, print.thres="best") # AUC=0.796 !!!

# Try to improve 


### k-NN
library(caret)
knn_diab <- train(diab~ ., data=trn, method='knn', tuneLength=20)
knn_diab
pred_knn_diab <- predict(knn_diab$finalModel,tst[,-7])
knnroc <- roc(tst$diab ~ pred_knn_diab[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.78 !!!

knn_sysbp <- train(sysBP~ .-diab, data=trn, method='knn', tuneLength=40)#qua lo usa per predirre la pressione sanguigna, sopra per la classe
knn_sysbp
pred_knn_sysbp <- predict(knn_sysbp$finalModel,tst[,-c(7,8)])
sqrt(mean((pred_knn_sysbp-tst$sysBP)^2)) # RMSE 11.27


## Naive Bayes
library(e1071)
nb <- naiveBayes(diab~., data=trn)
pred_nb.cl <- predict(nb,tst,type="class")
pred_nb <- predict(nb,tst,type="raw")
confusionMatrix(pred_nb.cl, tst$diab, positive="yes")
nbroc <- roc(tst$diab ~ pred_nb[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.8 
roc(tst$diab ~ pred_nb[,2],  plot=T, print.auc=F, print.thres=0.5, add=T) 
roc(tst$diab ~ pred_nb[,2],  plot=T, print.auc=F, print.thres=0.1, add=T) 

