#esercizio laboratorio Linear Regression
#Use the Pima data set to regress blood pressure ("pressure") from other features
#Use different criteria (see above) to choose the best linear regression model for predicting blood pressure
#You may also wish to try predicting blood pressure using k-NN regression (see ?knn.reg in the FNN package and "method=knn" in the train function of the caret package)

data("PimaIndiansDiabetes2", package = "mlbench") # you must have the mlbench package installed
pima.data <- na.omit(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)
dim(pima.data)#392 soggetti e 9 variabili inerenti
str(pima.data)

detach(pima.data)
attach(pima.data)
summary(pima.data)

plot(pressure~glucose)
fit1= lm(pressure~glucose,data= pima.data)
summary(fit1)
abline(fit1, col="black")
fit2= lm(pressure~glucose+mass+insulin+age+pedigree,data= pima.data)
summary(fit2)
#coefficiente di determinazione 0.187 quindi na merda ma d'altronde la relazione pressure glucose+le altre feature fa pellagra
fit3= lm(pressure~glucose+mass+insulin+age+pedigree+I((glucose+mass+insulin+age+pedigree)^2)+I((glucose+mass+insulin+age+pedigree)^3),data= fish)
summary(fit3)
fit10= lm(pressure~glucose+mass+insulin+age+pedigree+I((glucose+mass+insulin+age+pedigree)^2)+I((glucose+mass+insulin+age+pedigree)^3)+I((glucose+mass+insulin+age+pedigree)^4)+I((glucose+mass+insulin+age+pedigree)^5)+I((glucose+mass+insulin+age+pedigree)^6)+I((glucose+mass+insulin+age+pedigree)^7)+I((glucose+mass+insulin+age+pedigree)^8)+I((glucose+mass+insulin+age+pedigree)^9)+I((glucose+mass+insulin+age+pedigree)^10), data= pima.data)
summary(fit10)

AIC(fit1)#il primo è il più alto dei 4: 677. 
AIC(fit2)
AIC(fit3)#il terzo e il quarto sono i più bassi 616 e 613: Quindi basandosi solo su loro uno dici beh il modello 3 e 10 sono i migliori.. in realtà il secondo è il top ma questo lo valuti con 
AIC(fit10)#considerando il coeff di determinazione RMSE AIC e  BIC.



BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit10)
###

fit= lm(pressure~.,data= pima.data)
summary(fit)

