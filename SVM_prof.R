bc <- read.table("BreastCancer_complete.dat", stringsAsFactors=TRUE)
detach(bc)
attach(bc)
summary(bc)

## SVM

library(e1071)
svm.fit <- svm(class ~ ., data = bc, kernel="linear",cost=100)
svm.fit1 <- svm(class ~ ., data = bc, kernel="radial",gamma=0.1)
svm.fit2 <- svm(class ~ ., data = bc, kernel="polynomial",cost=1,degree=2)
summary(bc)

plot(svm.fit,data=bc,thickness~bare.nuclei,grid=100)#lui sceglie quelle due feature per il plot, cmq per vedere che siano le più importanti su Randomforest c'è varImp per vedere quali sono le feature più importanti.

#TUTTO sto ambaradam è per confrontare nattimo gli svm qui sopra
slice=list(unif.size=mean(bc$unif.size), unif.shape=3, adhesion=10, epith.size=median(bc$epith.size), chromatin=5, normal.nucleoli=1, mitoses=2)#crea questa lista prendendo la media di ogni feature
slice = list(mitoses=10)
plot(svm.fit,data=bc,thickness~bare.nuclei,grid=100, slice=slice)#praticamente ha fatto svm con tutte le features però in 2D puoi plottarne solo due, qua plotta thickness vs bare.nuclei che sono quelle più rappresentative
#però con slice puoi regolare i valori di altre features e vedere come varia il plot. si nota che per alti valori di thickness e bare nuclei il rischio che sia malignant è maggiore
plot(svm.fit1,data=bc,thickness~bare.nuclei,grid=100)
plot(svm.fit1,data=bc,thickness~bare.nuclei,grid=100, slice=slice)

plot(svm.fit2,data=bc,thickness~bare.nuclei,grid=100)
plot(svm.fit2,data=bc,thickness~bare.nuclei,grid=100, slice=slice)

set.seed(22)
tune_out = tune(svm, class ~ ., data = bc, kernel = "linear", ranges = list(cost = c(0.001,0.01,0.1,0.5,1,2,5,10))) #fa un tuning del lambda (cost è lambda della formula hinge loss function)
plot(tune_out)
summary(tune_out)#prima clicchi sul tune e poi su summary per vedere il risultato, nel caso del polinomiale il migliore è degree=1 polinomio di grado uno quindi lineare cost = 0.1. Più alto è error peggio è
tune_out = tune(svm, class ~ ., data = bc, kernel = "polynomial", ranges = list(cost = c(0.001,0.01, 0.1,1,10,100), degree = c(1,2,3,4)))#cost molto basso da overfitting ha detto

tune_out = tune(svm, class ~ ., data = bc, kernel = "radial", ranges = list(cost = c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))
tune_out = tune(svm, class ~ ., data = bc, kernel = "radial", ranges = list(cost = c(0.2,0.5,0.8,1,2,3,4), gamma = c(0.0001,0.001,0.01,0.1,0.5)))

summary(tune_out)

bestmod = tune_out$best.model#SI BECCA IL MODELLO MIGLIORE FRA TUTTI QUELLI TUNNATI!!
summary(bestmod)#il migliore fra tutti i precedenti è quello radiale dal plot sembra lineare perchè gamma è basso
plot(bestmod,data=bc,thickness~bare.nuclei,grid=100, slice=slice)#I PLOT TI DANNO UN'IDEA SE HA SENSO SVM O SE C'è OVERFITTING, per capire il modello migliore devi fare col train come sotto che ti dice l'accuracy
#oppure col summary qua sopra

library(caret)
train.control = trainControl("cv")
svm.cv <- train(class~., data = bc, method = "svmLinear", trControl = train.control)
svm.cv #con questo package però non puoi tunnare il parametnro cost, non lo tunna come fanno quelli qui sotto

svm.cv0 <- train(class~., data = bc, method = "svmLinear2", trControl = train.control) # uses e1071 linear2 è uguale a quello del package e1071 però fa il tuning di cost
svm.cv0


svm1.cv <- train(class~., data = bc, method = "svmRadial", trControl = train.control,  preProcess = c("center","scale"))
svm1.cv #

svm1.tune <- train(class~., data = bc, method = "svmRadial", trControl = train.control,  preProcess = c("center","scale"), tuneLength = 10) #C=4 0.9575403  0.9085283. tuneLrngth = 10 tune the parameter cost per 10 volte, sigma rimane costante al valore ottimale 
svm1.tune

svm11.tune<- train(class~., data = bc, method = "svmRadial", trControl = train.control, tuneGrid=expand.grid(C = c(0.5,0.8,1.2), sigma=seq(0.01,1,by=0.1)))
svm11.tune

svm2.tune <- train(class~., data = bc, method = "svmPoly", trControl = train.control,  preProcess = c("center","scale"), tuneLength = 3) 
svm2.tune #  2       0.100  1.00  0.9721816  0.9392653

svm3.tune<- train(class~., data = bc, method = "svmPoly", trControl = train.control, tuneGrid=expand.grid(C = c(0.5,0.8,1.2), degree=c(1,2,3), scale=1))
svm3.tune