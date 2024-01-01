hd <- read.table("HeartDisease.dat",stringsAsFactors=TRUE)
hd$num <- as.factor(hd$num)#facendo as.factor ha reso num e slope delle classi prima invece erano delle features lo vedi dal summary dopo questa operazione scompare media moda mediana ecc..
hd$slope <- as.factor(hd$slope)
summary(hd)
str(hd) #check that factors are actually factors

######### Random Forest
library(randomForest)
set.seed(1001)
rf <- randomForest(num~., data=hd, importance=TRUE)
rf 
# True\Predicted
# FN = 1-TP 0.069 for num=0
# FP
x=rf$confusion
sum(x[2:5,1])/sum(x[2:5,])#si calcola a mano l'accuratezza e si nota che è bassa quindi Random forest fa schifo 
# FP = 0.38

rf$importance
varImpPlot(rf)

### unite num 1-4 Non so cosa abbia fatto
rf.n <- randomForest(as.factor(num==0)~., data=hd, importance=T)#rende "true" tutti i soggetti con num=0 e "false"tutti gli altri in questo modo crea due classi
rf.n #higher FN (13%) but lower FP (23%) - preferable in this context

varImpPlot(rf.n)

### Boosting
library(gbm)

gbm1 <- gbm(num~., data=hd, train.fraction = 0.5, cv.folds = 5, bag.fraction=0.5, shrinkage=0.1, n.tree=500) #train.fraction =0.5 significa che la prima metà dei dati è il training set mentre la restante metà testset, potrebbe essere pericoloso se hai dati ordinati prima tutti positive e poi tutti negative ad esempio
# Check performance using the out-of-bag (OOB) error; the OOB error typically                                  #n.tree massimo numero di alberi ammesso
# underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1, method = "OOB")#non è il migliore.. usa la cross validation e basta all'esame così sei sicuro sia il top. curva nera e rossa nel grafico
print(best.iter)#stampa nella console miglior numero di iterazioni
#metod indicate the method used to estimate the optimal number of boosting iterations. Quindi serve per capire il miglior numero di iterazioni all'esame usa CV va  in teoria il metodo migliore è quello col minimo più basso nella curva
# Check performance using the 50% heldout test set
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)#minimo delle due curve molto simili

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1, method = "cv")#in verde nel grafico
print(best.iter)

# tuning wrt. shrinking parameter vediamo al variare del lambda il risultato migliore 
shr.vec <- c(0.0001,0.001,0.01,0.1)#costruisce un vettore di lambda
res = cbind(shr.vec,rep(0,length(shr.vec)),rep(0,length(shr.vec)))#Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively. 
res=data.frame(res)
names(res) <- c("shrinkage", "n.trees", "error")#nomina le colonne
res #crea sta tabella figa per vedere alla fine il risultato migliore

for(i in 1:length(shr.vec)){#fa un tuning dei lambda valutando l'errore della cv e il numero di alberi complìilando la tabella
  shr <- shr.vec[i]#prende il primo lambda e fa il boosting
  gbm2 <- gbm(num~., data=hd, train.fraction = 1, cv.folds = 5, bag.fraction=0.5, shrinkage=shr, n.tree=5/shr)#n.tree al variare del lambda cambia il numero di trees perchè per ottenere risultati comparabili se shr è piccolo avrò bisogno di più alberi..
  error <- min(gbm2$cv.error)#cv.error sono i numeri che mi danno la curva verde del grafico, perciò mi prenderò il minimo di sta curva verde
  res[i,] <- c(shr, 
               gbm.perf(gbm2, method = "cv",plot.it=F), 
               error)#nella riga della tabella res[i,] ci metto sto vettore contente: il lambda,numero di alberi e il relativo errore nell'ultima colonna. praticamente modifico la tabella res
}
res # not a big difference, maybe shr=0.001 slightly better so we'll use this value below
#Una volta che abbiamo predetto col nostro gbm otteniamo le 4 probabilità del dato di appartenere a ciascuna classe noi dobbiamo predere la probabilità più alta e assegnarli quella classe

##
set.seed(1001)
idx = sample(nrow(hd),200)
trn = hd[idx,]
tst = hd[-idx,]

gbm3 <- gbm(num~., data=trn, train.fraction = 1, cv.folds = 10, bag.fraction=0.5, shrinkage=0.001, n.tree=5000)#train.fraction vuol dire che una l'intero dataset come training set che ha senso dato che data = trn
best.iter <- gbm.perf(gbm3, method = "cv")
print(best.iter)



predgbm <- predict(gbm3, newdata=tst, n.trees = best.iter, type="response")#usiamo il modello per predirre sul testset
predgbm # class probabilities ad ogni dato è associata sono associate le probabilità di appartenere alle 5 classi 0-4

predgbm.m <- matrix(predgbm,ncol=5)#trasformiamo tutte ste prob in una matrice di 5 colonne noi dobbiamo prenderci l'indice del max e sottrarci 1 per assegnarli una delle classi da 0-4
predclass <- factor(max.col(predgbm.m)-1, levels=0:4) #sottrae 1 perchè le classi sono da 0-4 e non da 1-5.si prende il max di ogni riga (la probabilità più alta) e ci assegna la label sottraendo 1 per il motivo di prima
predclass # predicted classes. factor/as.factor rende una feature come una classe praticamente

# alternative way to get classes 
# see ?apply -- an alternative to loops
predclass.1 <- apply(predgbm, 1, function(x){which.max(x)-1})


library(caret)
confusionMatrix(predclass,as.factor(tst$num))#accuracy del 57% schifetto
confusionMatrix(as.factor(predclass==0),as.factor(tst$num==0))#il prf si è fermato qui!

#qua unisce le 5 classi in due
# unite classes 1-4
gbm4 <- gbm((num==0)~., data=trn, train.fraction = 1, cv.folds = 10, bag.fraction=0.5, shrinkage=0.001, n.tree=10000)
best.iter <- gbm.perf(gbm4, method = "cv")
print(best.iter)

predgbm <- predict(gbm4, newdata=tst, n.trees = best.iter, type="response")
predclass <- as.factor(predgbm>0.5)
predclass

library(caret)
confusionMatrix(predclass,as.factor(tst$num==0))
# FP = 14/43 = 0.32 =1-sensitivity (for num>0)
# FN = 7/54 = 0.13 = 1-specificity
# Better than above

###
# tuning with caret, note use of expand.grid
fitControl <- trainControl(method="cv")
tune.grid <- expand.grid( n.trees=c(50, 100, 200, 500,1000), interaction.depth=1, shrinkage=c(0.001,0.01,0.1), n.minobsinnode=10)
set.seed(825)
gbmFit <- train(as.factor(num)~., data=trn, method="gbm", trControl=fitControl, tuneGrid=tune.grid)
gbmFit

pred.train <- predict(gbmFit,newdata=tst) # predicts classes!
confusionMatrix(pred.train,as.factor(tst$num))
