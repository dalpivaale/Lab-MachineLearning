#SVM laboratory lab15

data0 <- read.table("BreastCancer_complete.dat", stringsAsFactors=TRUE)

### SVM
library(e1071)
svm0 <- svm(class~bare.nuclei+unif.shape,data= data0,  kernel="linear") #kernel lineare quindi separa con una linea
plot(svm0,bare.nuclei~ unif.shape,data=data0)#le croci sono i support vector machines, quelli più vicino alla linea sono coloro che contribuiscono maggiormente alla costruzione del decision boundary

svm1 <- svm(class~bare.nuclei+unif.shape,data= data0,  kernel="polynomial")#meglio
plot(svm1,bare.nuclei~ unif.shape,data=data0)

svm2 <- svm(class~bare.nuclei+unif.shape,data= data0,  kernel="radial") #il peggiore fino ad ora
plot(svm2,bare.nuclei~ unif.shape,data=data0)

svm3 <- svm(class~.,data= data0,  kernel="radial") #considerando tutte le features mi sembra molto bene
plot(svm3,bare.nuclei~ unif.shape,data=data0)
#se fai -id per escluderlo dalle feature svm non lavora.. è fatto così devi creare un nuovo dataframe rimuovendo la colonna id

###tuning

#per il tuning esiste prorpio una funzione, qui sotto fa il tuning di svm passandogli come lista i degree da tunnare (cost è la funzione costo col lambda dentro la quale cambia anch'essa in 4 step) sono sequneze di numeri
tune1 <- tune(svm, class~ bare.nuclei+thickness+unif.size+unif.shape+normal.nucleoli, data=data0, kernel="polynomial", ranges = list(degree=1:4, cost=2^(-1:2)))
plot(tune1)#plotta l'errore si vede che quello più basso è color chiaro degree=1
plot(tune1$best.model,bare.nuclei~thickness,data=data0)

tune2 <- tune(svm, class~ bare.nuclei+thickness+unif.size+unif.shape+normal.nucleoli, data=data0, kernel="radial", ranges = list(gamma=seq(0.1,2,by = 0.1), cost=2^(-1:2)))
plot(tune2)
plot(tune2$best.model,bare.nuclei~thickness,data=data0)

library(caret)
train.control <- trainControl(method = "cv")
svm0.cv <- train(class~.-id, data = data0, method = "svmLinear", trControl = train.control ) #accuracy 0.97 Kappa = 0.93
svm0.cv

svm.cv <- train(class~.-id, data = data0, method = "svmRadial", trControl = train.control ) #accuracy 0.96 kappa = 0.91 con costo 1
svm.cv

svm.tune<- train(class~.-id, data = data0, method = "svmPoly", trControl = train.control, tuneGrid=expand.grid(C = c(0.5,0.8,1.2), degree=c(1,2,3), scale=1))
svm.tune #accuracy 0.966 kappa = 0.92 cost = 0.8
