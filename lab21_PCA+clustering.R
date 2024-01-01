data<- read.table("ALL_data.data", stringsAsFactors=T)
dat <- data[,1:(ncol(data)-1)]	
cl <- as.factor(data$cl)

library(factoextra)
library(cluster)
library(FNN)

pr.out <- prcomp(dat, scale=F)
fviz_eig(pr.out)#la prima componente spiega quasi il 15% della varianza del dataset, la seconda 12%.. Le prime 3-4 componenti racchiudono la maggior parte della varianza, le altre poco
fviz_pca_ind(pr.out, habillage=cl, axes=c(1,2))#bene
fviz_pca_ind(pr.out, habillage=cl, axes=c(1,3))#schifo
fviz_pca_ind(pr.out, habillage=cl, axes=c(2,3))#not bad

#Clustering gerarchico con PCA anche al prof viene così
hc.pca <- hclust(dist(pr.out$x[, 1:2])) #fa il clustering gerarchico beccandosi le prime 2 componenti
plot(hc.pca, labels=cl,main = "Hier. Clust. on First Seven Score Vectors") 
rect.hclust(hc.pca, k=4)#spettacolo
table(cutree(hc.pca, 4),cl)

#clustering K-means con PCA
km.pca <- kmeans(dist(pr.out$x[, 1:2]), 4, nstart=20) #stessa cosa ma col k means
table(km.pca$cluster, cutree(hc.pca, 4))#compara i due clustering
table(km.pca$cluster, cl)#top con due componenti viene da dio

#SVM senza PCA vedi anche lab21 prof per scopiazzare svm. 
library(e1071)
pca.data = as.data.frame(cbind(cl,pr.out$x[,1:2]))
pca.data$cl=as.factor(pca.data$cl)
head(pca.data)
svm1 <- svm(cl~., data= pca.data )
pred <- predict(svm1, pca.data)
table(pred,cl)
library(caret)
train.control = trainControl("cv")
svm.cv <- train(cl~., data = pca.data, method = "svmLinear", trControl = train.control)
svm.cv #con questo package però non puoi tunnare il parametnro cost


svm1.tune <- train(cl~., data = pca.data, method = "svmRadial", trControl = train.control,  preProcess = c("center","scale"), tuneLength = 10) #C=4 0.9575403  0.9085283. tuneLrngth = 10 tune the parameter cost per 10 volte, sigma rimane costante al valore ottimale 
svm1.tune#accuracy = 1 con c >= 0.5
pred <- predict(svm1.tune,pr.out$x[, 1:2])
table(pred,cl)


svm11.tune<- train(cl~., data = pca.data, method = "svmRadial", trControl = train.control, tuneGrid=expand.grid(C = c(0.5,0.8,1.2), sigma=seq(0.01,1,by=0.1)))
svm11.tune
pred <- predict(svm11.tune,pr.out$x[, 1:2])
table(pred,cl)

svm2.tune <- train(cl~., data = pca.data, method = "svmPoly", trControl = train.control,  preProcess = c("center","scale"), tuneLength = 3) 
svm2.tune #  2       0.100  1.00  0.9721816  0.9392653

svm3.tune<- train(cl~., data = pca.data, method = "svmPoly", trControl = train.control, tuneGrid=expand.grid(C = c(0.5,0.8,1.2), degree=c(1,2,3), scale=1))
svm3.tune
## A me viene che con 1 componente l'accuracy di tutti e 3 gli SVM viene circa 75%, mentre con 2 componenti praticamente sempre 100%. Con 5 per forza 100%

library(e1071)
svm0 <- svm(cl~., data=dat)#SVM senza PCA
pred <- predict(svm0,dat)
table(pred,cl)#perfetto...

pca.data = as.data.frame(cbind(cl,pr.out$x))
pca.data$cl=as.factor(pca.data$cl)
head(pca.data)
svm1 <- svm(cl~., data= pca.data )#SVM considerando tutte le componenti della PCA
pred <- predict(svm1, pca.data)#perfetto anche lui
table(pred,cl)
plot(svm1, pca.data, PC2~PC1)


svm2 <- svm(cl~., data=pr.out$x[, 1:2])
svm2 <- svm(cl~., data=pr.out$x[, 1:2], kernel="linear")
pred <- predict(svm2,pr.out$x[, 1:2])
table(pred,cl)
plot(svm2,as.data.frame(pr.out$x[, 1:2]), PC2~PC1)

