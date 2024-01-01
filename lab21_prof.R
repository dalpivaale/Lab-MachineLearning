### Acute lymphoblastic leukemia (ALL) data
data<- read.table("ALL_data.data", stringsAsFactors=T)
dat <- data[,1:(ncol(data)-1)]	# the gene expression
cl <- as.factor(data$cl) 					# the true cell type
dim(dat)


### PCA
dat.pca <- prcomp(dat)
# dat.pca <- prcomp(dat, center=T, scale=T)
summary(dat.pca)
str(dat.pca)
plot(dat.pca, npcs=50)
plot(dat.pca, npcs=50, type="l")

library(factoextra)
library(cluster)

get_eig(dat.pca)
fviz_eig(dat.pca, ncp=30)




### PCA
library(factoextra)
library(cluster)

pr.out <- prcomp(dat, scale=F)
fviz_eig(pr.out, geom="line",ncp=128)
fviz_eig(pr.out, geom="line",ncp=20)
summary(pr.out)


fviz_pca_ind(pr.out, habillage=cl) 
fviz_pca_ind(pr.out, habillage=cl, axes=c(1,3)) 
fviz_pca_ind(pr.out, habillage=cl, axes=c(2,3)) 
fviz_pca_ind(pr.out, habillage=cl, axes=c(3,4)) 


sd.data <- dat

par(mfrow=c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "", labels = cl, main = "Complete Linkage") 
plot(hclust(data.dist, method = "average"), labels = cl, main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = cl, main = "Single Linkage", xlab = "", sub = "", ylab = "")

hc.out <- hclust(dist(sd.data),method = "complete")
par(mfrow = c(1, 1))
plot(hc.out, labels = cl) 
rect.hclust(hc.out, k=4)
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, cl)

plot(pr.out$x[, 1:2], col=hc.clusters, pch=20, cex=1.5)#plot delle prime due componenti + cluster, i triangoli sono le T cell mentre i cerchi le B cell. Si nota che con le prime due componenti esce un buon clustering
points(pr.out$x[, 1:2], pch=as.integer(cl),cex=2)

set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20) 
km.clusters <- km.out$cluster#il K means forse fa un pò meglio, differenzia molto bene i dati. OCCHIO non ha clusterizzato direttamente il datset Non con le componenti
table(km.clusters, hc.clusters)
table(km.clusters, cl)
fviz_cluster(km.out,pr.out$x[, 1:2])
plot(pr.out$x[, 1:2], col=km.clusters, pch=20, cex=1.5)
points(pr.out$x[, 1:2], pch=as.integer(cl),cex=2)

hc.pca <- hclust(dist(pr.out$x[, 1:2])) 
plot(hc.pca, labels = cl, main = "Hier. Clust. on First Two PCs") 
rect.hclust(hc.pca, k=3)
table(cutree(hc.pca, 4), cl)
plot(pr.out$x[, 1:2], col=cutree(hc.pca, 3), pch=20, cex=1.5)
points(pr.out$x[, 1:2], pch=as.integer(cl),cex=2)

km.pca <- kmeans(pr.out$x[, 1:2], 3, nstart=20, iter.max=20) 
table(km.pca$cluster, cutree(hc.pca, 3))
table(km.pca$cluster, cl)
fviz_cluster(km.pca,pr.out$x[, 1:2])#, ylim=c(-3,3), xlim=c(-3,3))
plot(pr.out$x[, 1:2], col=km.pca$cluster, pch=20, cex=1.5)
points(pr.out$x[, 1:2], pch=as.integer(cl),cex=2)

hc.pca <- hclust(dist(pr.out$x[, 1:7])) #Da qua fa la stessa cosa ma con 7 componenti e si nota che il clustering è migliore utilizzando solo le prime due componenti, poichè separa meglio i dati
plot(hc.pca, labels = cl, main = "Hier. Clust. on First 7 PCs") 
rect.hclust(hc.pca, k=4)
table(cutree(hc.pca, 4), cl)
plot(pr.out$x[, 1:2], col=cutree(hc.pca, 4), pch=20, cex=1.5)
points(pr.out$x[, 1:2], pch=as.integer(cl),cex=2)

km.pca <- kmeans(pr.out$x[, 1:7], 3, nstart=20) 
table(km.pca$cluster, cutree(hc.pca, 3))
table(km.pca$cluster, cl)
fviz_cluster(km.pca,pr.out$x[, 1:2])
plot(pr.out$x[, 1:2], col=km.pca$cluster, pch=20, cex=1.5)
points(pr.out$x[, 1:2], pch=as.integer(cl),cex=2)

# SVM lavora bene anche SVM con PCA
library(e1071)
svm0 <- svm(cl~., data=data)
pred <- predict(svm0,data)
table(pred,cl)#predice perfettamente tutte e 128 righe

pca.data = as.data.frame(cbind(cl,pr.out$x))#per buttare dentro la PCA ad svm devi trasformare il dataset in dataframe perchè altrimenti non funziona quindi qui trasforma il dataset (matrice) in dataframe
pca.data$cl=as.factor(pca.data$cl)
head(pca.data)#ho 128 principal components
svm1 <- svm(cl~., data= pca.data )
pred <- predict(svm1, pca.data)
table(pred,cl)
plot(svm1, pca.data, PC2~PC1)


svm2 <- svm(cl~., data=pr.out$x[, 1:2])
svm2 <- svm(cl~., data=pr.out$x[, 1:2], kernel="linear")
pred <- predict(svm2,pr.out$x[, 1:2])
table(pred,cl)
plot(svm2,as.data.frame(pr.out$x[, 1:2]), PC2~PC1)


## Qua xe roba in più no go fatto
sort(pr.out$rotation[,1]) # contribution of genes to PC1

# fviz_nbclust warning!
fviz_nbclust(dat, kmeans, method="silhouette") # calculates (probably) just one kmeans (nstart=1)

dd=dist(dat)
avg.sil <- rep(0, 10)
for(k in 2:10){
  kmk <- kmeans(dat, k, nstart = 20) 
  sil <- silhouette(kmk$cluster,dmatrix=as.matrix(dd))
  avg.sil[k] <- mean(sil[,3])
}
dev.new()
plot(avg.sil, type="b") # compare - best k=2, not k=3. However, very similar