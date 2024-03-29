data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data

### LOGISTIC REGRESSION

logis0 <- glm(diabetes~.,family=binomial,data=pima.data)#qua faccio la logistic regression
summary(logis0)#plotta il sommario, coefficients sono i beta, asterischi sulle feature pi� significative
pred0 <- predict(logis0, pima.data, type="response")#abbiamo il nostro modello/classificatore, prediciamo con esso sempre il pima data e vediamo se predice bene, con type = response ti da la probabilit� per ogni individuo
summary(pred0)#con type = response mi caga le probabilit� del mio modello  di ogni individuo di essere diabetico

library(caret)#fa la cross validation per non usare lo stesso dataset
train.control <- trainControl(method = "LOOCV")#live one out cross validation
train.control <- trainControl("cv",number=10)#number = 10 indica che la fold � lunga 10 se lo elimino � lunga uno
logis.cv <- train(diabetes~., data = pima.data, method = "glm", family="binomial",trControl = train.control)
logis.cv # esce un'accuratezza del 0.7780612  e un kappa del 0.4717145 buono ma niente di spettacolare

set.seed(3)#split the data in training and test
idx = sample(nrow(pima.data),250)
trn = pima.data[idx,]
tst = pima.data[-idx,]

logis <- glm(diabetes~.-age,family=binomial(),data=trn)#costruisco il mio modello solo con training data
summary(logis)#roba de prima
pred <- predict(logis, tst, type="response")# probabilit� calcolate nel testset, prediciamo sul test data che non avevamo usato nella costruzione del modello

pred.class=as.factor(ifelse(pred>0.5,"pos","neg"))#siccome voglio fare la confusion matrix, classifichiamo le prob. del modello, se > 0.5 classifichiamo diabetico altrimenti no.
confusionMatrix(pred.class,tst$diabetes, positive="pos")#funzione if-else, pred � la predizione, si poteva anche non mettere factor..
#comparo la classificazione venuta fuori dalle prob. del modello con la vera classe dei pima data. La specificit� fa cagare, accuratezza e sensibilit� buone dai. 
#il succo � che la soglia 0.5 � determinante nella classificazione bisogna scegliere la soglia pi� top possibile
# ROC curve

library(pROC)
#Roc curve
# ?roc
myroc <- roc(tst$diabetes ~ pred)#compare true value(testset valori veri) with model's prediction del testset (facciamo quello scritto nella slide 8)
plot(myroc)#plotta la ROC curve grezza
plot(myroc, print.auc = T, print.thres="best")#plotta anche l'AUC (0.78) grazie al secondo parametro, il terzo parametro gli dici di plottare anche il best alpha che massimizza specificity and sensitivity (� il punto pi+ distante dalla diagonale)
plot(myroc, print.auc = T, print.thres=0.5)


logis.ins <- glm(diabetes~insulin,family=binomial(),data=trn)#plotta la curva considerando come feature la sola Insulin e si nota che la curva � pi� brutta
summary(logis.ins)
pred.ins <- predict(logis.ins, tst, type="response")
myroc.ins <- roc(tst$diabetes ~ pred.ins)
plot(myroc.ins, print.auc = T, add=T, col="green")

#qua fa la stessa cosa ma sul trainig data
pred.trn <- predict(logis, trn, type="response")
myroc.trn <- roc(trn$diabetes ~ pred.trn)
plot(myroc.trn, print.auc = F, print.thres="best",add=T,col="blue")

logis.1 <- glm(diabetes~pregnant+glucose+pedigree+mass, family=binomial(),data=trn)
summary(logis.1)
pred.1 <- predict(logis.1, tst, type="response")
myroc.1 <- roc(tst$diabetes ~ pred.1)
myroc.1
plot(myroc.1, print.auc = F, print.thres="best", add=T, col="red")

# ROC for NB  
library(e1071)
# Note formula() call
nb_roc <- naiveBayes(formula(logis),data= trn,na.action=na.omit) #formula va sul oggetto logis e si becca l'argometno diabetes~.
nb_prob = predict(nb_roc2, newdata = tst, type = "raw")#
test_roc = roc(tst$diabetes ~ nb_prob[,2], plot = TRUE, print.auc = F,col="pink", add=T,print.thres="best")#devi beccarti la seconda colonna dell'oggetto sopra � la prob. di essere pos



