bc <- read.table("BreastCancer_complete.dat", stringsAsFactors=TRUE)


### PCA
library(factoextra)
library(cluster)
library(FNN) # for k-NN

bc.pca <- prcomp(bc[,2:10],scale=T, center=T)
head(bc.pca$x) # the coordinates of the observations on the PCs

print(bc.pca)

fviz_eig(bc.pca)#la prima componente spiega il 65% della varianza totale
fviz_pca_ind(bc.pca, habillage=bc$class)
fviz_pca_ind(bc.pca, habillage=bc$class, axes=c(1,3))
fviz_pca_ind(bc.pca, habillage=bc$class, axes=c(2,3))

fviz_pca_ind(bc.pca, habillage=bc$class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)

fviz_pca_var(bc.pca, axes=c(1,2), repel=T)
fviz_pca_var(bc.pca, axes=c(1,2), col.var="contrib", repel=T)
fviz_pca(bc.pca, habillage=bc$class, axes=c(1,2),label="var")

fviz_pca_var(bc.pca,axes=c(2,3), repel=T)
fviz_pca(bc.pca, habillage=bc$class, axes=c(1,3),label="var")

### k-NN with PCA
# split in trn and tst - do kNN analysis 
# investigate how the prediction accuracy varies with k and no. of PCs included
set.seed(2)
idx = sample(nrow(bc),500)
trn = bc[idx,]
tst = bc[-idx,]

trn.pca <- prcomp(trn[,2:10], scale=T, center=T)
trn.PC <- trn.pca$x#proiezioni delle componenti
summary(trn.PC)
tst.PC <- predict(trn.pca,tst[,2:10]) # projections of tst on PCs. con queste componenti predice sul test data
head(tst.PC)

str(trn.pca)
fviz_eig(trn.pca)
fviz_pca_ind(trn.pca, habillage=trn$class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)


knn.full=knn(trn[,2:10],tst[,2:10],trn$class,k=3)#fa il K nearrest con k = 3, il trn$class serve per beccarsi i valori dei 3 vicini
table(knn.full,tst$class)#confusion matrix, direi molto bene. con K = 3 predice molto bene

knn.pca=knn(trn.PC[,1:2],tst.PC[,1:2] , trn$class,k=3) #Qua fa il K nearest con le prime due componenti della PCA, allena l'algoritmo con le prime deu componenti che riassumono il trn e il tst
table(knn.pca,tst$class)#minchia si nota che il risultato è quasi eccellente come prima, quindi le sole prime due componenti riassumono molto bene il dataset

knn.pca=knn(t(t(trn.PC[,1:1])),t(t(tst.PC[,1:1])) , trn$class,k=3) # with only 1 PC we need to transpose to get column vectors. Twice for mysterious reasons...
table(knn.pca,tst$class)

trn.error <- tst.error <- rep(0,30)
nPC <- 2 #number of PCs
for(k in 1:30){
  knn.pca.trn <- knn(trn.PC[,1:nPC],trn.PC[,1:nPC],trn$class,k=k)#fa il knn buttandogli dentro le prime due componenti del trainigset anzichè l'intero trainigset
  trn.error[k] <-sum(knn.pca.trn != trn$class)/length(trn$class)#trainigerror: si becca la percentuale di predizioni sbagliate
  knn.pca.tst <- knn(trn.PC[,1:nPC],tst.PC[,1:nPC],trn$class,k=k)
  tst.error[k] <-sum(knn.pca.tst != tst$class)/length(tst$class)
}
plot(1:30, trn.error, col="red", type="b",ylim=c(0,0.1), ylab="error")#K sull'asse delle x. per K = 2 o 3 l'errore è più basso
points(1:30, tst.error, col="blue", type="b")#in rosso l'errore del training in blu quello del test

trn.error1 <- tst.error1 <- rep(0,30)
for(k in 1:30){#qua fa la stessa cosa ma non usa le componenti per rappresentare il trainigset ma gli butta dentro esso stesso
  knn.trn <- knn(scale(trn[,2:10]),scale(trn[,2:10]),trn$class,k=k)
  trn.error1[k] <-sum(knn.trn != trn$class)/length(trn$class)
  knn.tst <- knn(scale(trn[,2:10]),scale(tst[,2:10]),trn$class,k=k)
  tst.error1[k] <-sum(knn.tst != tst$class)/length(tst$class)
}
# trn.error1 <- tst.error1 <- rep(0,30)
# for(k in 1:30){
# knn.trn <- knn(trn[,2:10], trn[,2:10], trn$class,k=k)
# trn.error1[k] <-sum(knn.trn != trn$class)/length(trn$class)
# knn.tst <- knn(trn[,2:10], tst[,2:10], trn$class,k=k)
# tst.error1[k] <-sum(knn.tst != tst$class)/length(tst$class)
# }
points(1:30, trn.error1, col="orange", type="b", pch=4)#croci arancioni è il trainig erroe mentre in nero il test error. In conclusione il trainig error e il test error del dataset rappresentato con le prime due componenti è quasi uguale al fulldataset che do in pasto al KNN, anzi per K = 2-4 l'algoritmo lavora meglio con le sole prime due componenti evidentemente il resto è rumore
points(1:30, tst.error1, col="black", type="b", pch=4)



# repeat this excercise on the ALL data set
