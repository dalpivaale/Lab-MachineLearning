setwd("Users\dpale\Desktop\Lab_MachineLearning")

### Read data
ct <- read.table("CellType.dat", stringsAsFactors=TRUE) #Legge un file in formato tabella e crea da esso un dataframe.i vettori di carartteri sono dei fattori quindi metto TRUE
ct$Acell = as.factor(ifelse(ct$CellType=="A","yes","no"))#se ct ha CellType == A lo classifichi yes altrimenti no. Yes = si è una alpha cells. idem per cell cellule beta e delta
ct$Bcell = as.factor(ifelse(ct$CellType=="B","yes","no"))
ct$Dcell = as.factor(ifelse(ct$CellType=="D","yes","no"))
#vogliamo solo classificare le cellule come Beta cells or Non Beta cells
##vogliamo solo classificare le cellule come alpha cells or Non alpha cells
detach(ct)
attach(ct)

### solo un plot per far vedere cosa andiamo a classificare: le classi sono Betacell e Non Betacell (cellule A e D)
plot(Ccell~V2h,col=ifelse(Bcell=="yes","blue","red"),pch=ifelse(Bcell=="yes",12,2))
legend("topright",c("B","A or D"),col=c("blue","red"),pch=c(12,2))
plot(Ccell~Bcell)
plot(kh~Bcell)


logistic0 <- glm(Bcell~ Ccell +V2h , binomial(link = "logit"), data=ct)#si predice se la cellula è Bcell, costruisco il mio classificatore considerando come feature Ccell (capacità cellulare) e V2h che non so cosa sia
summary(logistic0)#si nota che all'aumentare della cell capacitance c'è maggior proba. sia una cellula beta

## Cross validation
library(caret)
# Define training control
# train.control <- trainControl(method = "LOOCV")
train.control <- trainControl(method = "cv", savePredictions=T)#se non specifico un numero di default le fold sono 10


fit0 <- train(formula(logistic0), data=ct, method="glm",family="binomial", trControl=train.control)
fit0 #0.905
confusionMatrix(fit0)
pred0 <- fit0$pred
pred0#hai un dataframe con le predizioni, obs è il valore corretto ("yes" è una betacell "no" non lo è)
confusionMatrix(pred0$pred,pred0$obs, positive="yes")#solita confusionMatrix che confronta le predizioni del modello con il valore reale
#Non abbiamo splittato in trainig e testset perchè il dataset è piccolo 200, infatti abbiamo fatto la crossvalidation

#Ok l'accuratezza è buona 90% ma non al top se includo anche le due feature Tailcurrent e Acurrent e ripeto il tutto l'accuratezza raggiunge 96%
logistic1 <- glm(Bcell~ Ccell+V2h+TailCurrent+Acurrent , binomial(link = "logit"), data=ct)
summary(logistic1)
anova(logistic1,test="Chisq")
fit1 <- train(formula(logistic1), data=ct, method="glm",family="binomial", trControl=train.control)
fit1 #0.955
confusionMatrix(fit1)
pred1 <- fit1$pred
confusionMatrix(pred1$pred,pred1$obs, positive="yes")



###
logistic2 <- glm(Bcell~ .-Acell-Dcell-CellType , binomial(link = "logit"), data=ct)
summary(logistic2)
anova(logistic2,test="Chisq")
fit2 <- train(formula(logistic2), data=ct, method="glm",family="binomial", trControl=train.control)
fit2 #0.955
confusionMatrix(fit2)
pred2 <- fit2$pred
confusionMatrix(pred2$pred,pred2$obs, positive="yes")





set.seed(7)
idx=sample(1:nrow(ct),140)
trn=ct[idx,]
tst=ct[-idx,]

fit.trn1 <- update(logistic1,data=trn)
pred1 <- predict(fit.trn1,tst,type="response")
class1 = as.factor(ifelse(pred1>0.5,"yes","no"))
confusionMatrix(class1,tst$Bcell)

fit.trn2 <- update(logistic2,data=trn)
pred2 <- predict(fit.trn2,tst,type="response")
class2 = as.factor(ifelse(pred2>0.5,"yes","no"))
confusionMatrix(class2,tst$Bcell)




### Problem with quasi-complete separation
logistic<- glm(Dcell~Acurrent+TailCurrent,binomial, data=ct)
summary(logistic)				# huge SEs 		
table(CellType,TailCurrent)
table(CellType,Acurrent)
plot(ct)
anova(logistic, test="Chisq")	# highly significant

library(logistf)
fitp = logistf(formula(logistic), family=binomial)	
summary(fitp)					# highly significant with penalized ML

pr.log <- predict(logistic,ct,type="response")
table(pr.log>0.5,Dcell)

## Cross validation
library(caret)
# Define training control
train.control <- trainControl(method = "LOOCV")
fit.train <- train(formula(logistic), data=ct, method="glm",family="binomial", trControl=train.control)
fit.train


#Multinomial logistic regression

library(nnet)
multi <- multinom(CellType~.-Acell-Bcell-Dcell, family="binomial", data=trn)
summary(multi)
#?multinom
pred.multi <- predict(multi,tst)
confusionMatrix(pred.multi,as.factor(tst$CellType))

multi1 <- multinom(CellType~Acurrent+TailCurrent+Ccell+V2h, family="binomial", data=trn)
summary(multi1)
#?multinom
pred.multi1 <- predict(multi1,tst)
confusionMatrix(pred.multi1,as.factor(tst$CellType))