

fish <- read.table("fishcatch.dat.txt")
names(fish) <- c("obs", "species", "weight","length1", "length2", "length3", "heightpct","widthpct","sex")#diverse speci3 di pesci con 3 tipi di lunghezza ciascuno, obs è il numero di osservazioni, weight il peso
head(fish)

fish=fish[fish$species==7,]#si becca solo questa specie di pesce
fish$sex = as.factor(fish$sex)
detach(fish)
attach(fish)
summary(fish)

plot(weight~length3,xlim=c(0,50))#proviamo a predirre il peso a partire da length3 con un modello lineare

fit1= lm(weight~length3,data= fish)#linear model, funzione per regressione lineare: weight è la variabile dipendente (da lenght3 in sto caso)
summary(fit1)#notiamo che l'intereccetta beta0 è a -seicento e qualcosa. mentre beta1 è 35 quindi se length3 incrementa di uno il peso incrementa di 35. residual standard error è RSS. Multiple R square è il coefficiente di determinazione 0.92 molto buono
abline(fit1, col="red")#funzione che plotta la mia retta/il mio modello più il dataset. Si nota che le mie y (dati osservati) hanno andamento parabolico. Perciò ora interpoliamo le y 
#con una parabola che è ancoraun modello lineare poichè è lineare nei coefficienti beta, aggiungiamo un termine Beta2 length3^2 il quale sarà una covariata, quindi il mio modello è weight = beta0+beta1*length3+beta2*length3^2. dove length3 è x1 e length3^2 è x2
fit2= lm(weight~length3+ I(length3^2),data= fish)#I è praticamente una matrice identica: prende come covariate length3 e length3^2. Multiple R square = 0.97 ancora meglio, infatti sistema l'intercetta sto modello
summary(fit2)

ll=seq(0,50, length.out=200)
lines(ll, coef(fit2)[1]+coef(fit2)[2]*ll+coef(fit2)[3]*ll^2 ,col="blue")

fit3= lm(weight~length3+I(length3^2)+I(length3^3),data= fish)
summary(fit3)

fit10= lm(weight~length3+I(length3^2)+I(length3^3)+I(length3^4)+I(length3^5)+I(length3^6)+I(length3^7)+I(length3^8)+I(length3^9)+I(length3^10), data= fish)
summary(fit10)#overfitting il modello fitta troppo le osservazioni e per valori sopra i 50 o sotto 10 si ottengono delle predizioni completamente errate

lines(ll, coef(fit10)[1]+coef(fit10)[2]*ll+coef(fit10)[3]*ll^2+coef(fit10)[4]*ll^3+coef(fit10)[5]*ll^4+coef(fit10)[6]*ll^5+coef(fit10)[7]*ll^6+coef(fit10)[8]*ll^7+coef(fit10)[9]*ll^8+coef(fit10)[10]*ll^9+coef(fit10)[11]*ll^10 , col=2)


### cross-validation ###  
library(caret)
# Define training control
train.control <- trainControl(method = "LOOCV")#fa la stessa cosa con la cross validation 

model1 <- train(formula(fit1), data = fish, method = "lm", trControl = train.control)
print(model1)#

model2 <- train(formula(fit2), data = fish, method = "lm", trControl = train.control)
print(model2)#il modello 2 che ha solo due coefficienti di regrassione è quello con RMSE più basso + un coefficiente di determinazione vicino a 1 quindi il migliore tra tutti. RMSE o MSE = 0 indica che le osservazioni giacciono tutte sulla retta quindi perfette

model3 <- train(formula(fit3), data = fish, method = "lm", trControl = train.control)
print(model3)

model10 <- train(formula(fit10), data = fish, method = "lm", trControl = train.control)
print(model10)



###
AIC(fit1)#il primo è il più alto dei 4: 677. 
AIC(fit2)
AIC(fit3)#il terzo e il quarto sono i più bassi 616 e 613: Quindi basandosi solo su loro uno dici beh il modello 3 e 10 sono i migliori.. in realtà il secondo è il top ma questo lo valuti con 
AIC(fit10)#considerando il coeff di determinazione RMSE AIC e  BIC.



BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit10)


### training/test - careful, the dataset is small!, riduce l'intero dataset a trainingset

rmse<- function(fit){ #costruisco una funzione a cui passiamo fit che uno dei miei 4 modelli costrutiti prima
  fit.trn <- update(fit, data=trn)#aggiorno fit con il trainingdata anzichè l'intero dataset, fit.trn mi da un modello costruito col trainigdata
  pred<-predict(fit.trn,tst)#predizione col nuovo modello sul testdata
  sqrt(mean((pred-tst$weight)^2)) #RMSE
}


set.seed(2) 
idx = sample(nrow(fish),floor(0.7*nrow(fish)))
trn = fish[idx,]
tst = fish[-idx,]
#Valutiamo RMSE sul test data usando i diversi modelli di prima
rmse(fit1)
rmse(fit2)#lui ha il RMSE più basso.. infatti è il migliore dei 4
rmse(fit3)
rmse(fit10)

#esercizio col Pima data
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data


#### LINEAR REGRESSION

plot(pressure~mass, data=pima.data)
plot(pressure~age, data=pima.data)
plot(mass~age, data=pima.data)

lin <- lm(pressure~., data=pima.data)
summary(lin)
AIC(lin)
BIC(lin)

lin1 <- lm(pressure~mass+age+glucose+insulin+pedigree, data=pima.data)#Aumentando la complessità del modello R^2 incrementa
summary(lin1)#mi par di campire che quando si ha a che fare con modelli dove si includono più covariate per sceliere il migliore fai riferimento a AIC e BIC no a R^2
AIC(lin1) # lower
BIC(lin1)

lin2 <- lm(pressure~mass+age, data=pima.data)
summary(lin2)
AIC(lin2)#solitamente AIC tende a favorire i modelli con più parametri
BIC(lin2) # lower

lin3 <- lm(pressure~mass, data=pima.data)
summary(lin3)
AIC(lin3)
BIC(lin3)

### cross-validation ###
library(caret)
# Define training control
train.control <- trainControl(method = "LOOCV")
#train.control <- trainControl(method = "cv")

model <- train(formula(lin), data = pima.data, method = "lm", trControl = train.control)
model#RMSE 11.52     R-square=0.1467

model1 <- train(formula(lin1), data = pima.data, method = "lm", trControl = train.control)
model1 #best  RMSE 11.46  R-square= 0.1577

model2 <- train(formula(lin2), data = pima.data, method = "lm", trControl = train.control)
model2 #similar  RMSE = 11.47   R-square = 0.155

model3 <- train(formula(lin3), data = pima.data, method = "lm", trControl = train.control)
model3  #RMSE = 11.967  R-square = 0.0808

set.seed(3) # change to get different splits
idx = sample(nrow(pima.data),250)
trn = pima.data[idx,]
tst = pima.data[-idx,]
#fa il RMSE a mano solo sul testset.. salto a piè pari cmq il succo è che se hai pochi dati è un rischio dividerli in training e testset

rmse<- function(lin){
  lin.trn <- update(lin, data=trn)
  pred<-predict(lin.trn,tst)
  sqrt(mean((pred-tst$pressure)^2)) #RMSE
}

rmse(lin)
rmse(lin1) #best
rmse(lin2) #best
rmse(lin3)


### kNN with cont response
library(FNN)
?knn.reg
#head(trn)
knn.pred <- knn.reg(scale(trn[,-c(3,9)]), scale(tst[,-c(3,9)]), trn[,3], k=13)
sqrt(mean((knn.pred$pred-tst$pressure)^2)) #RMSE


#### We wish to find the best model and best k using the training dataset, and then test on the test set
train.control <- trainControl(method = "repeatedcv", repeats=3)

set.seed(3)
model.knn <- train(formula(lin), data=trn, method="knn", trControl=train.control, preProcess=c("center","scale"), tuneGrid = expand.grid(k = seq(1, 201, by = 3)))
model.knn # 11.80

set.seed(3)
model1.knn <- train(formula(lin1), data= trn, method="knn", trControl=train.control, preProcess=c("center","scale"), tuneGrid = expand.grid(k = seq(1, 201, by = 3)))
model1.knn # 11.80

set.seed(3)
model2.knn <- train(formula(lin2), data= trn, method="knn", trControl=train.control, preProcess=c("center","scale"), tuneGrid = expand.grid(k = seq(1, 201, by = 3)))
model2.knn # 11.63

set.seed(3)
model3.knn <- train(formula(lin3), data= trn, method="knn", trControl=train.control, preProcess=c("center","scale"), tuneGrid = expand.grid(k = seq(1, 201, by = 3)))
model3.knn # 12.06

# model 2 was best, sue the best k for that model

plot(model2.knn)
plot(RMSE~k, data=model2.knn$results, type="b", ylim=c(11,16), xlim=c(0,200)) #same but allows adding lines
lines(RMSE~k, data=model.knn$results, type="b", col="red")
lines(RMSE~k, data=model1.knn$results, type="b", col="green")
lines(RMSE~k, data=model3.knn$results, type="b", col="blue")

final<-model2.knn$finalModel
summary(final)
final$xNames
final$k
# cols = names(tst) %in% final$xNames
# pred <- predict(model2.knn$finalModel , newdata=scale(tst[,cols]))
pred <- predict(model2.knn$finalModel , newdata=scale(tst[,c(6,8)]))
sqrt(mean((pred-tst$pressure)^2)) #11.43
abline(h=sqrt(mean((pred-tst$pressure)^2)), lty=2)
abline(v=final$k, lty=2)

rmse.knn<- function(k=3){
  knn.pred.k <- knn.reg(scale(trn[,c(6,8)]), scale(tst[,c(6,8)]), trn[,3], k=k)
  sqrt(mean((knn.pred.k$pred-tst$pressure)^2))
}
rmses<- sapply(1:200,rmse.knn)
lines(1:200, rmses, col=4, type="l")
min(rmses)
which(min(rmses)==rmse