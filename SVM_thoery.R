#Lez 14 SVM theory
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data

### VISUALIZATION
#

logis0 <- glm(diabetes~.,family=binomial,data=pima.data)
summary(logis0)

logis <- update(logis0, diabetes~glucose+mass)#aggioran il mio modello considerando solo glucose e body mass come features
#logis <- update(logis0, diabetes~glucose*mass)
summary(logis)

glc=seq(50,200,by=2) #50:5:200 in MATLAB syntax si becca questo valori di glucosio da 50 a 200. variabile glucosio 
bmi=seq(18,68,by=1)#valori bmi 
gr <- expand.grid(mass=bmi, glucose = glc)#combina questi valori delle deu features in una griglia la quale vuole dare in pasto al modello logistico

pp <- predict(logis,gr, type="response")#predice usando il modello la grglia 
pp.class <- (pp>0.5)
pp.class <- matrix(pp.class, nrow=length(bmi))
image(bmi, glc, pp.class, xlab="BMI", ylab="Glucose")
points(glucose~mass, data=pima.data, col=pima.data$diabetes, pch=19)

library(tree)
tree0 <- tree(formula(logis),data=pima.data)
plot(tree0)
text(tree0, pretty=0)
# plot(cv.tree(tree0))
# tree1<- prune.tree(tree0, best=2)
# plot(tree1)
# text(tree1)
pp.class0 <- predict(tree0,gr)[,2]>0.5 # must be logical
pp.class0 <- matrix(pp.class0, nrow=length(bmi))
image(bmi, glc, pp.class0, xlab="BMI", ylab="Glucose")
points(glucose~mass, data=pima.data, col=pima.data$diabetes, pch=19)

library(randomForest)
rf <- randomForest(formula(logis),data=pima.data, importance=F) 
pred_rf <- predict(rf,gr)
pp.class <- (pred_rf=="pos")
pp.class <- matrix(pp.class, nrow=length(bmi))
image(bmi, glc, pp.class, xlab="BMI", ylab="Glucose")
points(glucose~mass, data=pima.data, col=pima.data$diabetes, pch=19)

library(gbm)
boost <- gbm(ifelse(pima.data$diabetes=="pos",1,0)~glucose+mass, data=pima.data, cv.folds=5, n.trees=200)
best.iter <- gbm.perf(boost, method = "cv")
print(best.iter)
pred_gbm <- predict(boost,gr, type="response" ,n.trees=200) #best.iter)
pp.class <- (pred_gbm>0.5)
pp.class <- matrix(pp.class, nrow=length(bmi))
image(bmi, glc, pp.class, xlab="BMI", ylab="Glucose")
points(glucose~mass, data=pima.data, col=pima.data$diabetes, pch=19)


### SVM
library(e1071)
svm0 <- svm(diabetes~glucose+mass,data=pima.data,  kernel="linear") 
plot(svm0,glucose~ mass,data=pima.data) #il paramentro cost è il lambda della hinge loss function

svm1 <- svm(diabetes~glucose+mass,data=pima.data,  kernel="polynomial") 
plot(svm1,glucose~ mass,data=pima.data)

svm1 <- svm(diabetes~glucose+mass,data=pima.data,  kernel="polynomial", degree=15) #degree è il grado del polinomio che uso per andare nello spazio di dimensioni maggiori, di default è 3.
plot(svm1,glucose~ mass,data=pima.data)#fai il tuning di degree  per vedere qual è il migliore, se hai forme strane c'è overfitting

svm2 <- svm(diabetes~glucose+mass,data=pima.data,  kernel="radial") 
plot(svm2,glucose~ mass,data=pima.data)

svm2 <- svm(diabetes~glucose+mass,data=pima.data,  kernel="radial", gamma=0.1) 
plot(svm2,glucose~ mass,data=pima.data)

svm3 <- svm(diabetes~.,data=pima.data,  kernel="radial") 
plot(svm3,glucose~ mass,data=pima.data)

# tuning tune è una funzione che ti permette di fare il tuning dei parametri. COL TUNING VEDI L'ERRORE E CAPISCI QUALE è MEGLIO oppure dal plot

tune1 <- tune(svm, diabetes~glucose+mass, data=pima.data, kernel="polynomial", ranges = list(degree=1:4, cost=2^(-1:2)))#quindi gli dai una lista dei parametri de tunnare e il range, cost dsarebbe lambda che di default è 1.
plot(tune1)#plot dell'errore in fuznione del degree e di cost.
plot(tune1$best.model,glucose~ mass,data=pima.data)#plotta il modello migliore cioè quello con errore più basso.dal colore si vede che l'errore (barra a destra 0.23) è più basso con degree=1 quinid il polinomio grado 1 è il migliore ovvero lineare

tune2 <- tune(svm, diabetes~glucose+mass, data=pima.data, kernel="radial", ranges = list(gamma=seq(0.001,0.1,by=0.003), cost=2^(-1:2)))#GUARDI IL COLORE, dove è blu scuro l'errore è più basso e ti guardi il valore dei parametri
plot(tune2)
plot(tune2$best.model,glucose~ mass,data=pima.data)


library(caret)
train.control <- trainControl(method = "cv")
svm0.cv <- train(diabetes~., data = pima.data, method = "svmLinear", trControl = train.control ) 
svm0.cv

svm.cv <- train(diabetes~., data = pima.data, method = "svmRadial", trControl = train.control ) 
svm.cv#C sarebbe lambda presumo

svm.tune<- train(diabetes~., data = pima.data, method = "svmPoly", trControl = train.control, tuneGrid=expand.grid(C = c(0.5,0.8,1.2), degree=c(1,2,3), scale=1))
svm.tune

