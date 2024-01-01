data("PimaIndiansDiabetes2", package = "mlbench") # you must have the mlbench package installed
dim(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)

pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data
#QUANDO USI KNN RICORDA CHE NON CI DEVONO ESSERE NA E DEVI SCALARE I DATI PER TOGLIERE L'UNITà DI MISURA!!!!
dim(pima.data)
str(pima.data)
head(pima.data)


plot(glucose~age, data=pima.data, pch=4*as.numeric(pima.data$diabetes), col=as.numeric(pima.data$diabetes))
plot(mass~pedigree, data=pima.data, pch=4*as.numeric(pima.data$diabetes), col=as.numeric(pima.data$diabetes))
plot(insulin~pressure, data=pima.data, pch=4*as.numeric(pima.data$diabetes), col=as.numeric(pima.data$diabetes))
plot(pressure~diabetes,data=pima.data)
plot(insulin~diabetes,data=pima.data)



### k nearest neighbor
library(FNN) # you must have the FNN package installed


set.seed(3)
idx = sample(nrow(pima.data),250) #si piglia 250 indici a caso 
trn = pima.data[idx,] #250
tst = pima.data[-idx,]#gli altri 250


summary(tst)
chance <- table(tst$diabetes)/nrow(tst)
chance #pescando a caso hai il 69,7% sia neg e il 30% pos

#?knn
pred <- knn(trn[,1:8],tst[,1:8],trn$diabetes,k=1) # not scaled!
accuracy = sum(pred==tst$diabetes)/nrow(tst)
accuracy

pred <- knn(scale(trn[,1:8]),scale(tst[,1:8]),trn$diabetes,k=1) #scale!
accuracy = sum(pred==tst$diabetes)/nrow(tst)
accuracy

# confusion matrix
x<- table(pred,tst$diabetes)
x
sum(diag(x))/sum(sum(x)) #accuracy



# Vary k, the number of nearest neighbors
KK=nrow(tst)
accuracy=rep(0,KK)

for(k in 1:KK){
  pred <- knn(scale(trn[,1:8]),scale(tst[,1:8]),trn$diabetes,k=k)
  accuracy[k] = sum(pred==tst$diabetes)/nrow(tst)
}

plot(1:KK,accuracy,type='b',ylim=c(0.65,0.8),xlab='k') #Interessante sto plot, accuracy in funzione di K
abline(h=max(chance),lty=2)

# same but not scaling the data
accuracy=rep(KK)
for(k in 1:KK){
  pred <- knn(trn[,1:8],tst[,1:8],trn$diabetes,k=k)
  accuracy[k] = sum(pred==tst$diabetes)/nrow(tst)
}
lines(1:KK,accuracy,type='l',col='red') #il comando lines aggiunge una linea al plot

## change unit of measurement to show the risk when not scaling data


set.seed(3)
idx = sample(nrow(pima.data),250)
trn = pima.data[idx,]
tst = pima.data[-idx,]

trn$age=365*trn$age # age in days
tst$age=365*tst$age # age in days

summary(tst)
chance <- table(tst$diabetes)/nrow(tst)
chance # same as before

#K tuning
KK=nrow(tst)
accuracy=rep(0,KK) #rep funzione utile per costruire un vettore velocemente con gli stessi valori ripetuti
for(k in 1:KK){
  pred <- knn(scale(trn[,1:8]),scale(tst[,1:8]),trn$diabetes,k=k)
  accuracy[k] = sum(pred==tst$diabetes)/nrow(tst)
}
plot(1:KK,accuracy,type='b',ylim=c(0.65,0.8),lty=2) # same as before because of scaling


accuracy=rep(KK)
for(k in 1:KK){
  pred <- knn(trn[,1:8],tst[,1:8],trn$diabetes,k=k)
  accuracy[k] = sum(pred==tst$diabetes)/nrow(tst)
}
lines(1:KK,accuracy,type='b',col='blue') #!!!
abline(h=max(chance),lty=2)