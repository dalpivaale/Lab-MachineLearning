data("PimaIndiansDiabetes2", package = "mlbench")#il package mlbench contiene il dataset PrimaIndianDiabetes2

pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data

# Naive Bayes
library(e1071) #carico il package nel workspace
nb1 <- naiveBayes(diabetes~.,data= PimaIndiansDiabetes2) # works with NAs! ~.confronta stima diabates usando tutte le variabili, costruisce il modello tenendo conto di tutte le altre variabili 
nb2 <- naiveBayes(diabetes~.,data= pima.data) #tilde e punto indica che diabetes dipende da tutte le features

table(predict(nb1,newdata=PimaIndiansDiabetes2), PimaIndiansDiabetes2$diabetes)#predict predice il dataset Pima usando nb1 come classificatore... usando il classificatore nb1
table(predict(nb2,newdata=PimaIndiansDiabetes2), PimaIndiansDiabetes2$diabetes)#un pò meglio dell'altro ma fanno schifo tutti e due un pò


library(naivebayes) # yet another package - includes Poisson distribution
nb3<- naive_bayes(diabetes~., data=pima.data)
#pima.data.int <- pima.data
#pima.data.int$pregnant <- as.integer(pima.data.int$pregnant)
#nb3i<- naive_bayes(diabetes~., data=pima.data.int, usepoisson=T)
par(mfrow=c(2,5))#costruisce una window con subplot c(2,5) vettore due righe e 5 colonne
plot(nb3)
nb3

gluc <- nb3$tables[[2]]  # [[i]] to select in lists
gluc
#par(mfrow=c(1,2))
hist(pima.data$glucose[pima.data$diabetes=="neg"],freq=F,main="neg",xlab="glucose",xlim=c(50,200))#isogramma dei valori del glucosio
lines(dnorm(1:200,mean=gluc[1,1],sd=gluc[2,1]),col="blue")
hist(pima.data$glucose[pima.data$diabetes=="pos"],freq=F,main="pos",xlab="glucose",xlim=c(50,200))#mette una linea di colore blu
lines(dnorm(1:200,mean=gluc[1,2],sd=gluc[2,2]),col="blue")


table(predict(nb2,pima.data[,1:8]),pima.data$diabetes)
table(predict(nb3,pima.data[,1:8]),pima.data$diabetes)
#table(predict(nb3,pima.data[,1:8]),predict(nb3i,pima.data.int[,1:8]))
#table(predict(nb3i,pima.data.int[,1:8]),pima.data.int$diabetes)


set.seed(3)
idx = sample(nrow(pima.data),250)#divido il dataset in training e testset
trn = pima.data[idx,]
tst = pima.data[-idx,]

#?naiveBayes
nb <- naiveBayes(diabetes~.,data=trn)
pred.nb<-predict(nb,newdata=tst)#predizione del testset col classificatore appena costruito
x=table(pred.nb,tst$diabetes)
 x
 accuracy=sum(diag(x))/nrow(tst)
 accuracy #0.74
 TPR = x[2,2]/sum(x[,2])
 TPR # 0.53

# TNR = x[1,1]/sum(x[,1])
# TNR # 0.82
library(caret)
confusionMatrix(pred.nb,tst$diabetes, positive="pos")#necessità del package caret
#accuracy 0.739 non fantastica, no information rate 0.69 altino, kappa value 0.37 schifetto
## Cross-validation
library(caret)
#?train
trn.ctrl <- trainControl(method="LOOCV")
trn.ctrl.10 <- trainControl(method="cv",number=10)
nb.train <- train(diabetes~., data=trn,method="nb",trControl=trn.ctrl.10)#fa una 10 fold cross validation costruendo il nostro modello. tutti e 10 i modelli sono in nb.train
nb.train
summary(nb.train)
names(nb.train)
nb.train$bestTune
nb.final <- nb.train$finalModel #Si prende il modello migliore diciamo, quello con accuracy più alta che è tipo 0.78-0.79
pred.final<-predict(nb.final,tst)$class #predizione della classe col nuovo classificatore ottenuto con la cross validation
x <- table(pred.final,tst$diabetes)
confusionMatrix(x)#accuratezza è 0.70 fa ancora un pò schifo. #comparo le predizioni con la realtà

trn.ctrl.10p <- trainControl(method="cv",number=10, savePredictions = "final")
nb.train <- train(diabetes~., data= pima.data, method="nb", trControl=trn.ctrl.10p)
pred.train <- nb.train$pred
nb.train
confusionMatrix(pred.train$pred,pred.train$obs, positive="pos")

