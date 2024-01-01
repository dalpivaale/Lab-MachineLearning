data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data


#####
library(randomForest)
set.seed(3)
idx = sample(nrow(pima.data), floor(0.7*nrow(pima.data)))
trn <- pima.data[idx,]
tst <- pima.data[-idx,]
rf <- randomForest(diabetes~. , data=trn, importance=T, ntree=2000) # does not work with missing data. Dei 500 trees assegna la classe del soggetto prendendo la maggioranza dei voti di ogni albero
rf #si noti l'out of error pari al 19%.. si può far meglio
varImpPlot(rf)#plotta l'immportanza delle variabili (quanto va a puttane l'accuratezza media se togli quella variabile), se tolgo glucose l'accuratezza cala molto
pred_rf <- predict(rf,tst)
x<-table(pred_rf,tst$diabetes)#confusion matrix
library(caret)
confusionMatrix(x)
x
sum(diag(x))/nrow(tst) #0.73 accuratezza calcolata a mano

plot(rf$err.rate[,1], type='l') # OOB error vs. number of trees

pred_rf_all <- predict(rf,tst, predict.all=T) # see ?predict.randomForest. Qua predice mantenedo le predizioni di ciascun albero per ciascun individuo (credo)
dim(pred_rf_all$individual)
pred_ind <- pred_rf_all$individual
table(pred_ind)
# tsterr=rep(NA,2000)
# for(n in 2:2000){
# npos <- apply(pred_ind[,1:n]=="pos",1,sum)
# pr <- ifelse(npos>n/2,"pos","neg")
# x <- table(as.factor(pr),tst$diabetes)
# tsterr[n] <- 1-sum(diag(x))/nrow(tst) 
# }
# lines(tsterr, type="l", col="blue")

npos<-0 
tsterr2=rep(NA,2000)
for(n in 1:2000){
  npos <- npos+ ifelse(pred_ind[,n]=="pos",1,0)
  prr <- ifelse(npos>n/2,"pos","neg")
  x <- table(as.factor(prr),tst$diabetes)
  tsterr2[n] <- 1-sum(diag(x))/nrow(tst) 
}
lines(tsterr2, type="l", col="red")


pred_rf_prob <- predict(rf,tst,type="prob")#mette la percentuale di alberi che hanno votato neg o pos
head(pred_rf_prob)#per il soggeto 4 il 99,45% degli alberi ha votato neg lo 0.0055 pos.. eccetera
pred_rf_votes <- predict(rf,tst,type="vote")
head(pred_rf_votes) # same

nfeat <- (ncol(pima.data)-1) # no. of features
ooberr <- rep(NA,nfeat)
tsterr <- rep(NA,nfeat)

set.seed(111)
for(m in 1:nfeat){
  rf_tmp <- randomForest(diabetes~. , data=trn, importance=T, mtry=m, ntree=1000)
  ooberr[m] <- rf_tmp$err.rate[nrow(trn),1]
  x <- table(predict(rf_tmp,tst),tst$diabetes)
  tsterr[m] <- 1-sum(diag(x))/nrow(tst) 
}
plot(ooberr, type='b',ylim=c(0.1, 0.3))
lines(tsterr, type='b', col="blue")


###
library(caret)
tctrl <- trainControl("cv", number=10)
trf <- train(diabetes~., data=trn, method='rf', tuneLength=7, trControl=tctrl)
trf

