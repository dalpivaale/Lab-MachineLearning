#DAL PIVA ALESSANDRO   2014703

data<- read.table("raisin.dat", stringsAsFactors=T)

library(NbClust)
library(factoextra)
library(cluster)

no.diab.data <- scale(data[,-8])#Remove last column about Class 
k.diab2 <- kmeans(no.diab.data, 2)
k.diab2$centers 
k.diab2$tot.withinss
k.diab2$tot.withinss/k.diab2$totss
k.diab2$betweenss/k.diab2$totss

x=table(k.diab2$cluster,data$Class)#Comparing cluster with their labels
x
acc <- print(1-sum(diag(x))/nrow(ra))
accuracy # accuracy = 0.23. 

sil<-silhouette(k.diab2$cluster,dist(no.diab.data)) #sihlouette index
fviz_silhouette(sil)

fviz_cluster(k.diab2, no.diab.data[,c(6,2)],ellipse.type="norm")#clusters figure

#TUNING K
K=9
wss=rep(NA,K)
for(k in 1:K){
  k.diab <- kmeans(no.diab.data, k)
  wss[k] <- k.diab$tot.withinss
}
plot(1:K,wss, type='b')#

#?fviz_nbclust
fviz_nbclust(no.diab.data,kmeans,method="wss",k.max=9)#K = 2 there is the knee! 

fviz_nbclust(no.diab.data,kmeans,method="silhouette",k.max=30)#Average silhouette width vs K, for K = 2 silhouette is max so K = 2 is the best choice

NbClust(no.diab.data, method="kmeans")#Either this function suggest K = 2

k <- kmeans(no.diab.data,centers=2,nstart=10) #repeat - different results!
table(k$cluster,data$Class)#compara con cl  valori veri
k$tot.withinss
sil<-silhouette(k.diab2$cluster,dist(no.diab.data)) #sihlouette index
fviz_silhouette(sil)#uguale 

## LOGISTIC REGRESSION
set.seed(3)
idx = sample(nrow(data),600)#divido il dataset in training e testset
trn = data[idx,]
tst = data[-idx,]

library(caret)
logis0 <- glm(Class~., binomial(link = "logit"),data=trn)
summary(logis0)#AIC = 432.1
pred0 <- predict(logis0, tst, type="response")
summary(pred0)
pred.class=as.factor(ifelse(pred0>0.5,"Kecimen","Besni"))
confusionMatrix(pred.class,tst$Class, positive="Besni")#accuracy 0.86, Specificity = 0.87 sensitivity = 0.84  Good model
BIC(logis0)#467.27
AIC(logis0)
library(pROC)
#Roc curve
myroc <- roc(tst$Class ~ pred0)#compare true value
plot(myroc)
plot(myroc, print.auc = T, print.thres="best")#best alpha = 0.322, sennsitivity = 0.948, specifcity = 0.782. AUC = 0.926 good model is distant from chance classification. So it predict well true positive but true negative no so well because sensitivity = 0.782
plot(myroc, print.auc = T, print.thres=0.5)

##PCA Analysis
bc.pca <- prcomp(trn[,1:7],scale=T, center=T)#. 
plot(bc.pca)#eigenvalues plot
bc.pca$sdev
head(bc.pca$x) # the coordinates of the observations on the PCs

print(bc.pca)

fviz_eig(bc.pca)#69% of variance in explained by only first component, the second one explained about 21.1% of the variance
fviz_pca_ind(bc.pca, habillage=trn$Class)#only with the first and second component there is no perfect distinction of data
fviz_pca_ind(bc.pca, habillage=trn$Class, axes=c(1,3))
fviz_pca_ind(bc.pca, habillage=trn$Class, axes=c(2,3))

fviz_pca_ind(bc.pca, habillage=trn$Class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)

fviz_pca_var(bc.pca, axes=c(1,2), repel=T)#Feature "Extent" follows principally second component, "Eccentricity" follows a lot secondo component but a little either the first component. The others feature follows mainly first component
fviz_pca_var(bc.pca, axes=c(1,2), col.var="contrib", repel=T) "Area,Convex Area, Perimeter, MajorAxisLength are the features that determine the first PC"
fviz_pca(bc.pca, habillage=trn$Class, axes=c(1,2),label="var")
fviz_pca_var(bc.pca,axes=c(2,3), repel=T)
fviz_pca(bc.pca, habillage=trn$Class, axes=c(1,3),label="var")

fviz_eig(bc.pca, geom="line",ncp=64)#knee at 2 components so i suggest use the first two components to rapresent data.

#PCA + LOGISTIC REGRESSION
pc.data <- as.data.frame(bc.pca$x)
pc.data$Class <- trn$Class
pc.trn <- pc.data[idx,]
pc.tst <- pc.data[-idx,]
head(pc.trn)

pclfit <- glm(Class~PC1+PC2, data=pc.data, family=binomial)#SI BECCA COSì LE PRIME DUE COMPONENTI!!!
summary(pclfit)
pred.pc <- predict(pclfit,pc.tst, type="response")
pred.class.pc <- as.factor(ifelse(pred.pc<0.5,"Besni","Kecimen"))
confusionMatrix(table(pred.class.pc,pc.tst$Class),positive="Kecimen") #0.844
#Roc curve
myroc <- roc(tst$Class ~ pred.pc)#compare true value
plot(myroc)
plot(myroc, print.auc = T, print.thres="best")#best alpha = 0.322, sennsitivity = 0.948, specifcity = 0.782. AUC = 0.926 good model is distant from chance classification. So it predict well true positive but true negative no so well because sensitivity = 0.782
plot(myroc, print.auc = T, print.thres=0.5)



###RANDOM FOREST
library(randomForest)
set.seed(3)
tr <- randomForest(Class~. , data=trn, importance=T, ntree=500)
tr #OOB = 13.33%, No. of variables tried at each split: 2. Confusion matrix not so good
pred <- predict(tr, tst)
pred
confusionMatrix(pred, tst$class) # 0.982 di accuracy kappa quasi a uno specificity e sensituvuty  top
varImpPlot(tr)#from plot Perimeter, Eccentricity,MajorAxisLength are the most important features. Area and Convex Area not so important


rf <- randomForest(Class~., data=bc.pca$x[, 1:2], importance=T, ntree=2000) # does not work with missing data. Dei 500 trees assegna la classe del soggetto prendendo la maggioranza dei voti di ogni albero
rf #si noti l'out of error pari al 15.33
varImpPlot(rf)#bare nuclei unifsize unifshape sono le features più importanti
pred_rf <- predict(rf,tst)
x<-table(pred_rf,tst$Class)#confusion matrix
library(caret)
confusionMatrix(x)#spettacolo accuracy 0.87 e specificity,sensitivity al top 0.88-0.86
plot(rf$err.rate[,1], type='l',xlim=c(0,100)) # OOB error vs. number of trees. 36-38 best number of trees
#pred_rf <- predict(rf,tst, type="response")

#qua faccio la ROC curve
library(pROC)
predll <- predict(rf,tst, type="prob")
lroc <- roc(tst$Class ~ predll[,2],  plot=T, print.auc=T, print.thres="best") # AUC=0.927 specificity 0.844, for aplha 0.457 !!!




library(e1071)
svm0 <- svm(cl~., data=data)
pred <- predict(svm0,data)
table(pred,cl)

pca.data = as.data.frame(cbind(cl,pr.out$x))
pca.data$cl=as.factor(pca.data$cl)
head(pca.data)
svm1 <- svm(cl~., data= pca.data )
pred <- predict(svm1, pca.data)
table(pred,cl)
plot(svm1, pca.data, PC2~PC1)


svm2 <- svm(cl~., data=pr.out$x[, 1:2])
svm2 <- svm(cl~., data=pr.out$x[, 1:2], kernel="linear")
pred <- predict(svm2,pr.out$x[, 1:2])
table(pred,cl)
plot(svm2,as.data.frame(pr.out$x[, 1:2]), PC2~PC1)

            