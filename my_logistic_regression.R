ct<- read.table("BreastCancer_complete.dat",stringsAsFactors=TRUE)

idx = sample(nrow(ct),400)#divido il dataset in training e testset
trn = ct[idx,]
tst = ct[-idx,]

library(caret)
logis0 <- glm(class~. -id,family = binomial(),data=trn)#qua faccio la logistic regression
summary(logis0)#plotta il sommario, coefficients sono i beta, asterischi sulle feature più significative
pred0 <- predict(logis0, tst, type="response")#abbiamo il nostro modello/classificatore, prediciamo con esso sempre il pima data e vediamo se predice bene, con type = response ti da la probabilità per ogni individuo
summary(pred0)
pred.class=as.factor(ifelse(pred0>0.5,"malignant","benign"))#siccome voglio fare la confusion matrix, classifichiamo le prob. del modello, se > 0.5 classifichiamo diabetico altrimenti no.
confusionMatrix(pred.class,tst$class, positive="malignant")#accuracy 0.96, kappa 0.92, specificity e sensitivity molot alte

#CROSS VALIDATION
train.control <- trainControl(method = "LOOCV")#live one out cross validation
train.control <- trainControl("cv",number=10)#number = 10 indica che la fold è lunga 10 se lo elimino è lunga uno
logis.cv <- train(class~. -id, data = trn, method = "glm", family="binomial",trControl = train.control)
logis.cv
pred1 <- predict(logis.cv, tst, type="raw") #qua se metti type = raw ti mette in pred1 le predizioni già come "benign" o "malignant", se metti "prob" poi ti tocca trasformarli in label con l'istruzione sotto e beccarti solo la seconda colonna!
summary(pred1)
#pred.class1 =as.factor(ifelse(pred>0.5,"malignant","benign"))#siccome voglio fare la confusion matrix, classifichiamo le prob. del modello, se > 0.5 classifichiamo diabetico altrimenti no.
confusionMatrix(pred1,tst$class, positive="malignant")#mi viene esattamente come prima quindi la cross validation non aumenta le performance o le diminuisce

#ROC CURVE
library(pROC)
#Roc curve
# ?roc
myroc <- roc(tst$class ~ pred0)#compare true value(testset valori veri) with model's prediction del testset (facciamo quello scritto nella slide 8)
plot(myroc)#plotta la ROC curve grezza
plot(myroc, print.auc = T, print.thres="best")#Roc curve spettacolare, la soglia migliore è con alpha = 0.207. AUC = 0.993
plot(myroc, print.auc = T, print.thres=0.5)


#Vediamo se naive bayes fa meglio o peggio come performance
library(e1071)
# Note formula() call
nb_roc <- naiveBayes(formula(logis0),data= trn,na.action=na.omit) #formula va sul oggetto logis e si becca l'argometno diabetes~.
nb_prob = predict(nb_roc, newdata = tst, type = "raw")
test_roc = roc(tst$class ~ nb_prob[,2], plot = TRUE, print.auc = F,col="pink", add=T,print.thres="best")
plot(test_roc, print.auc = T, print.thres="best")#AUC = 0.997 quindi naive bayes fa leggermente meglio, infatti ha la 

#Per la multinomial logist regression vedi lab5_logistiRegression basta caricare la libreria e c'è il comando apposta