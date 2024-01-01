#Lez 12 Boosting teoria
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data


##### Boosting
library(gbm)
head(pima.data)
boost <- gbm(glucose ~.-diabetes, data=pima.data)#vogliamo predirre il glucosio dalle altre features tranne l'ultima colonna
summary(boost)#figo mostra le features più importanti
plot(boost, i.var="insulin", xlim=c(0,1000), ylim=c(50,200))#plotta la risposta y (il glucosio) in funzione dell'insulina, all'aumentare dell'insulina aumenta quasi esponenzialmente il glucosio
points(glucose~insulin, data=pima.data, pch=2,xlim=c(0,1000), ylim=c(50,200)) # must plot first next line, then repeat this and previous line
plot(glucose~insulin, data=pima.data, pch=2)#plotta i triangolini che sono il dataset originale e notiamo che il modello con l'insulina interpola bene. Assa stare ste righe de codice va

boost2 <- gbm(pressure ~.-diabetes, data=pima.data)# questo modello predice la pressione invece
summary(boost2)
gbm.perf(boost2, oobag.curve=F)#è MSE in relazione al numero di tree e ti tira la linea sul possibile numero di tree top, Se metto l'ultimo parametro T mi plotta l'OOB in funzione del numero di alberi e ti suggerisce il punto ottimo
#Oerò R ti dice che sta funzione sottostima sto numero e conviene fare la cross validation per capire il numero migliore
boost2 <- gbm(pressure ~.-diabetes, data=pima.data, train.fraction=0.7, n.trees=500)#fa il boosting prendendo il 70% dei dati come training e gli altri 30% come test set
gbm.perf(boost2)#nella console ti butta fuoir il best number of trees. la curva sotto nera è il trainig error quello rosso il test error. L'istruzione di prima mi suggeriva 23 come top numero invece questa che è più affidabile 99

boost2.0 <- gbm(pressure ~.-diabetes, data=pima.data, n.trees=500, cv.folds=10)#stessa cosa ma con la CV che è più lenta anziche fare l'oob 
boost2.1 <- gbm(pressure ~.-diabetes, data=pima.data, n.trees=500, cv.folds=10, bag.fraction=1)#uguale ma bag.fraction = 1 significa che ad ogni iterazione riusa gli stessi dati, se < 1 invece c'è più casualità nel prelevare i dati ad ogni iterazione
 summary(boost2.0)
gbm.perf(boost2.0, method="cv")
lines(boost2.1$train.error, col="red")#la linea nera è il training error della ten fold cross-validation,la linea rossa uguale ma senza bagging (boost2.1 bag.fraction =1) infatti è meno efficiente il modello ci vogliono più iterazioni per abbassare MSE (training error goes down more rapidly col bagging)
#linea verde cv error trovato con la cross validation col bagging, quea blu  ma senza bagging (peggio) quando risale indica overfitting da quel punto in poi!

lines(boost2.1$cv.error, col="blue")

boost2.2 <- gbm(pressure ~.-diabetes, data=pima.data, n.trees=5000, cv.folds=10, bag.fraction=1, shrinkage=0.01)#qua giochiccia col lambda che di deafult è 0.1, occio a bag.fraction = 1 quindi No bagging. costruiscei il modello facendo la cross validation
gbm.perf(boost2.2, method="cv")
boost2.3 <- gbm(pressure ~.-diabetes, data=pima.data, n.trees=5000, cv.folds=10, bag.fraction=0.5, shrinkage=0.01)
lines(boost2.3$train.error, col="red", lty=2)
lines(boost2.3$cv.error, col="blue",lty=2)#in rosso e blu (blu  cross validation) training error col bagging, staltre senza bagging

set.seed(3)
idx = sample(nrow(pima.data), floor(0.7*nrow(pima.data)))
trn = pima.data[idx,]
tst = pima.data[-idx,]

boost2 <- gbm(pressure ~.-diabetes, data=trn, n.trees=500)
gbm.perf(boost2)
pred <- predict(boost2,tst)
mean((pred-tst$pressure)^2) # MSE 150.2
sqrt(mean((pred-tst$pressure)^2)) # RMSE 12.25
predvec <- predict(boost2,tst, n.trees=1:500)
msevec = apply(predvec, 2, function(x) mean((x-tst$pressure)^2) )
lines(msevec, col="blue")

#lines(boost2$train.error, col=)
boost2 <- gbm(pressure ~.-diabetes, data=trn, n.trees=500, cv.folds=10)
boost2.1 <- gbm(pressure ~.-diabetes, data=trn, n.trees=500, cv.folds=10, bag.fraction=1)
#summary(boost2)
gbm.perf(boost2, method="cv")
lines(boost2.1$train.error, col="red")
lines(boost2.1$cv.error, col="blue")
predvec <- predict(boost2,tst, n.trees=1:500)
msevec = apply(predvec, 2, function(x) mean((x-tst$pressure)^2) )
lines(msevec, lty=2,col="green")
predvec.1 <- predict(boost2.1,tst, n.trees=1:500)
msevec.1 = apply(predvec.1, 2, function(x) mean((x-tst$pressure)^2) )
lines(msevec.1, lty=2, col="blue")

boost2.2 <- gbm(pressure ~.-diabetes, data=trn, n.trees=5000, cv.folds=10, bag.fraction=1, shrinkage=0.01)
gbm.perf(boost2.2, method="cv")
predvec.2 <- predict(boost2.2,tst, n.trees=1:5000)
msevec.2 = apply(predvec.2, 2, function(x) mean((x-tst$pressure)^2) )
lines(msevec.2, lty=2, col="green")


boost2.3 <- gbm(pressure ~.-diabetes, data=trn, n.trees=5000, cv.folds=10, bag.fraction=0.5, shrinkage=0.01)
lines(boost2.3$train.error, col="red")
lines(boost2.3$cv.error, col="blue")
predvec.3 <- predict(boost2.3,tst, n.trees=1:5000)
msevec.3 = apply(predvec.3, 2, function(x) mean((x-tst$pressure)^2) )
lines(msevec.3, lty=2, col="blue")

sqrt(min(msevec.3)) # 11.37 better than lm, knn

library(randomForest)
rf <- randomForest(pressure ~.-diabetes, data=trn, ntrees=1000)
pred.rf <- predict(rf,tst)
sqrt(mean((pred.rf-tst$pressure)^2)) #11.86




# classification
boost3 <- gbm(diabetes ~., data=trn) # fails: gbm lavora solo con 0 o 1 non con "pos" e "neg"
boost3 <- gbm(ifelse(diabetes=="pos",1,0) ~., data=trn, cv.folds=5, n.trees=500)#quindi qua risolve sto problema, se "pos" dammi 1 altrimenti 0.
summary(boost3)
gbm.perf(boost3, method="cv")

pred.db <- predict(boost3,tst, type="response")
library(pROC)
myroc<- roc(tst$diabetes~pred.db,plot=T)
auc(myroc)

