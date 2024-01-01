
data <- read.table("bbd_data.dat", stringsAsFactors=T)
data$gender=as.factor(data$gender)
detach(data)
attach(data)
summary(data)
plot(data)
nrow(data)

dat = na.omit(data) # remove all rows with missing data
nrow(dat)

## missing data
library(mice)
md.pattern(data, plot=T)

# predict sysBP/diabetes from other features using "dat" (omitted NAs) (your choice of method)

lfit0 <- glm(diab~., data=dat, family=binomial)
summary(lfit0)

lfit <- glm(diab~., data=data, family=binomial)
summary(lfit) # same, glm removes NAs (na.rm=T) by default

# could choose subset of predictors based e.g. on AIC, BIC
lfit1 <- glm(diab~gender+age+waist+sysBP+BMI,data=data, family=binomial)
summary(lfit1)
BIC(lfit)
BIC(lfit1)

# sysBP
lmfit <- lm(sysBP~., data=data)
summary(lmfit)
AIC(lmfit)
BIC(lmfit)

lmfit1 <- lm(sysBP~age+weight+BMI+waist+diab, data=data)
summary(lmfit1)
AIC(lmfit1)
BIC(lmfit1)


# perform single imputation and confront

imp1 <- mice(data,m=1, seed=123) #m = 1 fa una singola imputataion. seed è per la riproducibilità
lfit.imp <- update(lfit, data=complete(imp1))#aggiorna il modello di prima che era glm logistic model (i cui Na erano stati rimossi) con il mio nuovo datset completo poichè contiene le imputazioni
# lfit.imp <- glm(diab~., data=complete(imp1), family=binomial)
summary(lfit.imp)#confrontando i due modelli si nota che sono simili solo che uno l'hai costruito senza Nan e staltro con il dataset con le imputazioni
summary(lfit)

lfit1.imp <- update(lfit1, data=complete(imp1))
summary(lfit1.imp)
summary(lfit1)

# perform multiple imputation and pool your results

imp <- mice(data,m=5, seed=123)#mice fa imputazioni sul mio dataset colmando i Na e ottengo il mio nuovo datset pieno che chiamo imp. in sto caso costruisce 5 dataset con le imputazioni
lfits <- with(imp, glm(diab ~ gender + age + height + weight + BMI + waist + sysBP, family=binomial))#con with e pool combini tutti e 5 i modelli
pool.lfit <- pool(lfits) #pool.lfit è il modello finale che ottieni dalla combinazione degli altri 5
summary(pool.lfit)
summary(lfit)

lfits1 <- with(imp, glm(diab ~ gender + age + waist + sysBP + BMI, family=binomial))
pool.lfit1 <- pool(lfits1)
summary(pool.lfit1)
summary(lfit1)  #praticamente costruisce i datset senza Na facendo le imputazioni, con questi dataset costruisci i modelli e li comnbini assieme con with e pool. Poi confronta sto modello ottenuto con gli originali che erano stati costruiti rimuovendo i NA
#valutando se fanno peggio o meglio
lmfits <- with(imp, lm(sysBP ~ gender + age + height + weight + BMI + waist + diab))
pool.lmfit <- pool(lmfits)
summary(pool.lmfit)
summary(lmfit)

lmfits1 <- with(imp, lm(sysBP ~ age + weight + BMI + waist + diab))
pool.lmfit1 <- pool(lmfits1)
summary(pool.lmfit1)
summary(lmfit1)

# simply removing the NAs gives the same results!

# trees, RF, svm?

# split into training/test data and perform analysis with multiple imputation to test your predictions accuracy
# I choose test set to be data with missing observations + some more for diabetes

# build multiple imputation from training set only and use this model to predict for test set
set.seed(123)
idxNA  <- which(is.na(data$diab)) #145
idxComplete <- (1:nrow(data))[-idxNA] # work with
idxTst  <-  sample(idxComplete, 750) #tst
idxTstNA <- c(idxNA,idxTst)
length(idxTstNA)== length(unique(idxTstNA)) #check: no replicates

tst <- data[idxTst,]
dataNA <- data[idxNA,]
trn <- data[-idxTstNA,]

# dataComplete <- data[idxComplete,]

ign <- rep(FALSE,nrow(data))
ign[idxTstNA] <- TRUE # not used to build imputation model
summary(ign)

predMatr <- imp$predictorMatrix
predMatr[,7] <-0
predMatr
imp.ign <- mice(data, 
                method = c("","","pmm","pmm","pmm","pmm","","pmm"),
                predictorMatrix=predMatr,
                m = 5, maxit = 5, seed=123, ignore=ign)  
summary(complete(imp.ign,2)[idxTstNA,])

# without MI
lfit.trn <- update(lfit, data=trn)
summary(lfit.trn)
lfit.pred <- predict(lfit.trn, tst, type="response")
library(pROC)
roc(tst$diab, lfit.pred, plot=T, print.auc=T, print.thres="best") #AUC=0.81
lfit.class <- ifelse(lfit.pred>0.11,"yes","no")
x=print(table(lfit.class,tst$diab))
sum(diag(x))/sum(sum(x)) #0.765 - but 119 individuals not predicted!
md.pattern(tst, plot=F)

# with MI
accuracy=rep(NA,imp.ign$m)
pred.class = matrix(data=NA, nrow=nrow(tst), ncol=imp.ign$m)

for(i in 1:(imp.ign$m)){
  tst.m <- complete(imp.ign,i)[idxTst,]
  trn.m <- complete(imp.ign,i)[-idxTstNA,]
  fit.m <- glm(formula(lfit),data=trn.m, family=binomial)
  # print(summary(fit.m))
  pred<- predict(fit.m, tst.m, type="response")
  pred.class[,i] <- ifelse(pred>0.11,1,0)
  x<- table(pred.class[,i],tst$diab)
  accuracy[i] <- sum(diag(x))/sum(sum(x))
}

print(accuracy)
pred.class.vote<- ifelse(rowSums(pred.class)> imp.ign$m/2,"yes","no")
xx <-print(table(pred.class.vote, tst$diab))
sum(diag(xx))/sum(sum(x)) #0.72

table(pred.class.vote[is.na(lfit.class)],tst$diab[is.na(lfit.class)])
75/119

# smaller model 1
lfit1.trn <- update(lfit1, data=trn)
summary(lfit1.trn)
lfit1.pred <- predict(lfit1.trn, tst, type="response")
library(pROC)
roc(tst$diab, lfit1.pred, plot=T, print.auc=T, print.thres=0.11) #AUC=0.81
lfit1.class <- ifelse(lfit1.pred>0.11,"yes","no")
x=print(table(lfit1.class,tst$diab))
sum(diag(x))/sum(sum(x)) #0.739


accuracy=rep(NA,imp.ign$m)
pred.class = matrix(data=NA, nrow=nrow(tst), ncol=imp.ign$m)
for(i in 1:(imp.ign$m)){
  tst.m <- complete(imp.ign,i)[idxTst,]
  trn.m <- complete(imp.ign,i)[-idxTstNA,]
  fit.m <- glm(formula(lfit1),data=trn.m, family=binomial)
  # print(summary(fit.m))
  pred<- predict(fit.m, tst.m, type="response")
  pred.class[,i] <- ifelse(pred>0.11,1,0)
  x<- table(pred.class[,i],tst$diab)
  accuracy[i] <- sum(diag(x))/nrow(tst)
}
print(accuracy)
pred.class.vote<- ifelse(rowSums(pred.class)>imp.ign$m/2,"yes","no")
xx <-print(table(pred.class.vote, tst$diab))
sum(diag(xx))/nrow(tst) #0.72
table(pred.class.vote[is.na(lfit1.class)],tst$diab[is.na(lfit1.class)])

# without voting. Works immediately here because of linearity
pred.log = matrix(data=NA, nrow=nrow(tst), ncol=imp.ign$m)
for(i in 1:(imp.ign$m)){
  tst.m <- complete(imp.ign,i)[idxTst,]
  trn.m <- complete(imp.ign,i)[-idxTstNA,]
  fit.m <- glm(formula(lfit1),data=trn.m, family=binomial)
  # print(summary(fit.m))
  pred.log[,i]<- predict(fit.m, tst.m) # on linear predictor (logit) scale
}
pool.pred <- rowMeans(pred.log) # this is how to pool (neglecting SEs)
pool.pred <- 1/(1+exp(-pool.pred)) # change to probabilities
roc(tst$diab, pool.pred, plot=T, print.auc=T, print.thres="best") #AUC=0.812

pool.class <- ifelse(pool.pred>0.11, "yes", "no") # change to class
x<- print(table(pool.class,tst$diab))
sum(diag(x))/sum(sum(x)) #0.72
table(pool.class[is.na(lfit1.class)],tst$diab[is.na(lfit1.class)])

########## sysBP ##########

# build multiple imputation from training set only and use this model to predict for test set

set.seed(123)
idxNA  <- which(is.na(data$sysBP)) #372
idxComplete <- (1:nrow(data))[-idxNA] # work with
idxTst  <-  sample(idxComplete, 750) #tst
idxTstNA <- c(idxNA,idxTst)
length(idxTstNA)== length(unique(idxTstNA)) #check: no replicates

tst <- data[idxTst,]
dataNA <- data[idxNA,]
trn <- data[-idxTstNA,]

# dataComplete <- data[idxComplete,]

ign <- rep(FALSE,nrow(data))
ign[idxTstNA] <- TRUE # not used to build imputation model
summary(ign)

predMatr <- imp$predictorMatrix
predMatr[,8] <-0
predMatr
imp.ign1 <- mice(data, 
                 method = c("","","pmm","pmm","pmm","pmm","logreg",""),
                 predictorMatrix=predMatr,
                 m = 5, maxit = 5, seed=123, ignore=ign)  
summary(complete(imp.ign1,2)[idxTstNA,])


lmfit1.trn <- lm(sysBP~age+weight+BMI+waist+diab, data=trn)
summary(lmfit1.trn)
pred.lm <- predict(lmfit1, tst)
msqe <- print(mean((pred.lm-tst$sysBP)^2, na.rm=T)) #136

# without voting. Works immediately here because of linearity
pred.lm1 = matrix(data=NA, nrow=nrow(tst), ncol=imp.ign$m)
for(i in 1:(imp.ign$m)){
  tst.m <- complete(imp.ign1,i)[idxTst,]
  trn.m <- complete(imp.ign1,i)[-idxTstNA,]
  fit.m <- lm(formula(lmfit1.trn),data=trn.m)
  # print(summary(fit.m))
  pred.lm1[,i]<- predict(fit.m, tst.m) 
  print(mean((pred.lm1[,i]-tst$sysBP)^2))
}
pool.pred.lm <- rowMeans(pred.lm1) # this is how to pool (neglecting SEs)
pool.msqe <- print(mean((pool.pred.lm-tst$sysBP)^2)) #138.7
pool.msqe <- print(mean((pool.pred.lm[is.na(pred.lm)]-tst$sysBP[is.na(pred.lm)])^2)) 
rbind(pool.pred.lm[is.na(pred.lm)], tst$sysBP[is.na(pred.lm)])

plot(pool.pred.lm~ tst$sysBP, xlim=c(30,140), ylim=c(30,140), pch=4)
# plot(pool.pred.lm~ tst$sysBP, xlim=c(30,140), ylim=c(50,90), pch=4)
points(pool.pred.lm[is.na(pred.lm)]~ tst$sysBP[is.na(pred.lm)], pch=19, col="blue")
points(pred.lm~tst$sysBP,col="red")
abline(1,1,col="gray", lwd=2)

tst[is.na(pred.lm),