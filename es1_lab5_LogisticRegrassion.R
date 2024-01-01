#es1 lab5 Logistic regression
ct <- read.table("BreastCancer_complete.dat", stringsAsFactors=TRUE)
summary(ct)
logistic1 <- glm(class~thickness+bare.nuclei, binomial(link = "logit"), data=ct)
summary(logistic1)#ile feature con gli asterischi sono quelle da tenere d'occhio lo riscontri anche nel odds ratio
pred1 <- predict(logistic1, ct, type="response")#abbiamo il nostro modello/classificatore, prediciamo con esso ct e vediamo se predice bene, con type = response ti da la probabilità per ogni individuo
summary(pred1)

#cross validation
library(caret)#fa la cross validation per non usare lo stesso dataset
train.control <- trainControl(method = "LOOCV")#live one out cross validation
train.control <- trainControl("cv",number=10)#number = 10 indica che la fold è lunga 10 se lo elimino è lunga uno
logis.cv <- train(class~., data = ct, method = "glm", family="binomial",trControl = train.control)
logis.cv
#accuratezza del 96% e kappa altissimo quindi spettacolare
#conviene farla nel modo qui sotto la crossvalidation, sono solo due righe di codice e salvo le prendizioni in "pred"
train.control <- trainControl(method = "cv", savePredictions=T)#se non specifico un numero di default le fold sono 10

#altro modo per fare la crossvalidation
fit0 <- train(formula(logistic1), data=ct, method="glm",family="binomial", trControl=train.control)
fit0 
pred0 <- fit0$pred #si prende la predizione grazie al savepredictions = T
pred0#hai un dataframe con le predizioni, obs è il valore corretto ("yes" è una betacell "no" non lo è)
confusionMatrix(pred0$pred,pred0$obs, positive="malignant")#classifica molto bene sto modello 

#### Ora provo il modello dividendo in training e test data e valutiamo quanto è buono, compresa la ROC curve

set.seed(3)#split the data in training and test
idx = sample(nrow(ct),430)
trn = ct[idx,]
tst = ct[-idx,]
logistic <- glm(class~thickness+bare.nuclei, binomial(link = "logit"), data=trn)
pred <- predict(logistic, tst, type="response")
pred.class=as.factor(ifelse(pred>0.5,"malignant","benign"))#siccome voglio fare la confusion matrix, classifichiamo le prob. del modello, se > 0.5 classifichiamo maligno altrimenti no.
confusionMatrix(pred.class,tst$class, positive="malignant")#funzione if-else, pred è la predizione, si poteva anche non mettere factor..
#comparo la classificazione venuta fuori dalle prob. del modello con la vera classe dei pima data. La specificità fa cagare, accuratezza e sensibilità buone dai. 
#il succo è che la soglia 0.5 è determinante nella classificazione bisogna scegliere la soglia più top possibile

###ROC curve
library(pROC)
# ?roc
myroc <- roc(tst$class ~ pred)#compare true value(testset valori veri) with model's prediction del testset (facciamo quello scritto nella slide 8)

plot(myroc)#plotta la ROC curve grezza
plot(myroc, print.auc = T, print.thres="best")#plotta anche l'AUC (0.981) grazie al secondo parametro, il terzo parametro gli dici di plottare anche il best alpha che massimizza specificity and sensitivity (è il punto pi+ distante dalla diagonale)
plot(myroc, print.auc = T, print.thres=0.5)#AUC quasi a 1 infatti la sensitivity e specificity del calssificatore sono alte, ROC curve quasi perfetta

###Esercizio2 lab5
library(nnet)
ct <- read.table("HeartDisease.dat", stringsAsFactors=TRUE)
summary(ct)
set.seed(3)#split the data in training and test
idx = sample(nrow(ct),200)
trn = ct[idx,]
tst = ct[-idx,]
multi <- multinom(num~., family="binomial", data=trn)
summary(multi)
#?multinom
pred.multi <- predict(multi,tst)
confusionMatrix(pred.multi,as.factor(tst$num))

multi1 <- multinom(CellType~Acurrent+TailCurrent+Ccell+V2h, family="binomial", data=trn)
summary(multi1)
#?multinom
pred.multi1 <- predict(multi1,tst)
confusionMatrix(pred.multi1,as.factor(tst$CellType))

