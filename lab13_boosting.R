#lab13 Boosting
#5 classi da 0 a 4 inerenti la costrizione delle arterie classe 0 sei totalmente ammalato arteria chiusa in pratica
hd <- read.table("HeartDisease.dat",stringsAsFactors=TRUE)
hd$num <- as.factor(hd$num)
hd$slope <- as.factor(hd$slope)
summary(hd)
str(hd) #check that factors are actually factors

###RandomForest
library(randomForest)
set.seed(3)
idx = sample(nrow(hd), floor(0.7*nrow(hd)))
trn <- hd[idx,]
tst <- hd[-idx,]
rf <- randomForest(num~. , data=trn, importance=T, ntree=1000) # does not work with missing data. Dei 500 trees assegna la classe del soggetto prendendo la maggioranza dei voti di ogni albero
rf #si noti l'out of error pari al 48.31%.. uno schifo
varImpPlot(rf)
pred_rf <- predict(rf,tst)
x<-table(pred_rf,tst$num)#confusion matrix
x
sum(diag(x))/nrow(tst) #0.62 accuratezza calcolata a mano. 

plot(rf$err.rate[,1], type='l') # OOB error vs. number of trees, vicino a 50 hai l'OOb più basso

###Gradient Boosting
library(gbm)
boost2 <- gbm(num ~.,data=hd)# questo modello predice la pressione invece
summary(boost2)
gbm.perf(boost2, oobag.curve=F)##è il MSE in funzione delle iterazioni grafichetto sul quaderno, all'aumentare delle iterazioni ovviamente MSE si riduce (con curve = T).13 iterazioni sembrerebbe il numero migliore ma non è affidabile leggi sotto

boost2 <- gbm(num ~.,data=hd, train.fraction=0.7, n.trees=500)#Se curve = F ti caga fuori il miglior numero di iterazioni per ottenere L'OOB error più basso ma ti suggerisce ti fare la cross validation xk è un numero poco affidalbile
gbm.perf(boost2)#proviamo dunque a seguire il consglio e migliorare l'OOB error. Quindi costruisce il modello facendo la cross-validation tenendo il 70% del dataset come trainig data e predice il restante 30%
#il grafico ha la linea nera che è il trainig error del training set mentre in rosso del test set, in funzione del numero delle iterazioni, la linea tratteggiata intercetta il valore top con l'errore minimo
boost2.0 <- gbm(num ~., data=hd, n.trees=500, cv.folds=10)#sempre cross validation ma con folds grandi 10, è più lenta della precedente però ottimizzata di 10, staltra era una LOOCV
boost2.1 <- gbm(num ~., data=hd, n.trees=500, cv.folds=10, bag.fraction=1)#uguale ma bag.fraction = 1 significa che ad ogni iterazione riusa gli stessi dati, se < 1 invece c'è più casualità nel prelevare i dati ad ogni iterazione
# summary(boost2.0)
gbm.perf(boost2.0, method="cv")
lines(boost2.1$train.error, col="red")
lines(boost2.1$cv.error, col="blue")#la linea nera è la ten fold cross-validation,la linea rossa uguale ma senza bagging (boost2.1) infatti è meno efficiente il modello ci vogliono più iterazioni per abbassare MSE (training error goes down more rapidly col bagging)
#linea verde training error trovato con la cross validation col bagging, quea blu uguale ma senza bagging (peggio) quando risale indica overfitting da quel punto in poi!


boost2.2 <- gbm(num ~., data=hd, n.trees=5000, cv.folds=10, bag.fraction=1, shrinkage=0.01)#qua giochiccia col lambda che di deafult è 0.1, occio a bag.fraction = 1 quindi No bagging
gbm.perf(boost2.2, method="cv")#in blu e rosso col bagging, staltre (nero e verde) senza bagging infatti 
boost2.3 <- gbm(num ~., data=hd, n.trees=5000, cv.folds=10, bag.fraction=0.5, shrinkage=0.01)
lines(boost2.3$train.error, col="red", lty=2)#Domanda da esame qual è il numero di trees migliore e giocare col shinkage e depth per vedere se migliora o peggiora il modello
lines(boost2.3$cv.error, col="blue",lty=2)
