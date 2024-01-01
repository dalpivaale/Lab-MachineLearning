bc <- read.table("BreastCancer_complete.dat", stringsAsFactors=TRUE)


### PCA
library(factoextra)
library(cluster)
library(FNN) # for k-NN

bc.pca <- prcomp(bc[,2:10],scale=T, center=T)#si becca le ultime 9 feature perchè la prima è l'ID mentre l'undicesima sono le classi. sdev è praticamente gli autovalori della matrice di covarianza
plot(bc.pca)#plotta gli eigenvalues. scale = T vuol dire che scala i dati prima di fare la PCA!!
bc.pca$sdev#autovalori. La prima componente è importantissima, spiega un sacco della varianza
head(bc.pca$x) # the coordinates of the observations on the PCs

print(bc.pca)

fviz_eig(bc.pca)#circa il 65%della varianza è spiegata dalla prima componente ovvero in una direzione delle 9 totali
fviz_pca_ind(bc.pca, habillage=bc$class)#plot con le prime due componenti la prima asse x e la seconda asse y e noti che con sole queste due si distinguono bene i maligni dai benigni
fviz_pca_ind(bc.pca, habillage=bc$class, axes=c(1,3))#qua setta roba ma prendo la prima e la terza componente
fviz_pca_ind(bc.pca, habillage=bc$class, axes=c(2,3))#qua male, separati male, seconda e terza componente

fviz_pca_ind(bc.pca, habillage=bc$class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)#introduce ellissi per fare il clustering plottando le prime due componenti

fviz_pca_var(bc.pca, axes=c(1,2), repel=T)# le frecce sono le features e noti che puntano nella direzione dell'asse x circa ovvero la prima componente, ciò significa che sono spiegate per lo più dalla prima componente, mentre mitosis segue di più la seconda componente (dim2)
fviz_pca_var(bc.pca, axes=c(1,2), col.var="contrib", repel=T)#tutti plot analoghi e fighi per vedere le fetures quali componenti seguono di più
fviz_pca(bc.pca, habillage=bc$class, axes=c(1,2),label="var")#plot molto figo sia delle features (frecce) che dei dati

fviz_pca_var(bc.pca,axes=c(2,3), repel=T)#thickness segue perlopiù la terza componente mentre mitoses la seconda, staltre un pò caotiche
fviz_pca(bc.pca, habillage=bc$class, axes=c(1,3),label="var")

### k-NN with PCA
# split in trn and tst - do kNN analysis 
# investigate how the prediction accuracy varies with k and no. of PCs included

set.seed(2)
idx = sample(nrow(bc),500)
trn = bc[idx,]
tst = bc[-idx,]

trn.pca <- prcomp(trn[,2:10], scale=T, center=T)
trn.PC <- trn.pca$x
summary(trn.PC)
tst.PC <- predict(trn.pca,tst[,2:10]) # projections of tst on PCs
head(tst.PC)

str(trn.pca)
fviz_eig(trn.pca)
fviz_pca_ind(trn.pca, habillage=trn$class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)


knn.full=knn(trn[,2:10],tst[,2:10],trn$class,k=3)#fa KNN normale
table(knn.full,tst$class)

knn.pca=knn(trn.PC[,1:2],tst.PC[,1:2] , trn$class,k=3) #fa Knn con le prime due componenti della PCA
table(knn.pca,tst$class)

knn.pca=knn(t(t(trn.PC[,1:1])),t(t(tst.PC[,1:1])) , trn$class,k=3) # with only 1 PC we need to transpose to get column vectors. Twice for mysterious reasons...
x = table(knn.pca,tst$class)
x
accuracy=sum(diag(x))/nrow(tst)
accuracy

trn.error <- tst.error <- rep(0,30)
nPC <- 2 #no. of PCs
for(k in 1:30){ #calcola training error e test error facendo knn con le prime due componenti. poi ne fa il plot. SENZA SCALARE!!
  knn.pca.trn <- knn(trn.PC[,1:nPC],trn.PC[,1:nPC],trn$class,k=k)
  trn.error[k] <-sum(knn.pca.trn != trn$class)/length(trn$class)#calcolo del training e test error
  knn.pca.tst <- knn(trn.PC[,1:nPC],tst.PC[,1:nPC],trn$class,k=k)
  tst.error[k] <-sum(knn.pca.tst != tst$class)/length(tst$class)
}
plot(1:30, trn.error, col="red", type="b",ylim=c(0,0.1), ylab="error")
points(1:30, tst.error, col="blue", type="b")

trn.error1 <- tst.error1 <- rep(0,30)
for(k in 1:30){
  knn.trn <- knn(scale(trn[,2:10]),scale(trn[,2:10]),trn$class,k=k)
  trn.error1[k] <-sum(knn.trn != trn$class)/length(trn$class)
  knn.tst <- knn(scale(trn[,2:10]),scale(tst[,2:10]),trn$class,k=k)
  tst.error1[k] <-sum(knn.tst != tst$class)/length(tst$class)
}
plot(1:30, trn.error1, col="red", type="b",ylim=c(0,0.1), ylab="error")
points(1:30, tst.error1, col="blue", type="b")#scalando l'errore è quasi uguale senza scalare
# trn.error1 <- tst.error1 <- rep(0,30)
# for(k in 1:30){
# knn.trn <- knn(trn[,2:10], trn[,2:10], trn$class,k=k)
# trn.error1[k] <-sum(knn.trn != trn$class)/length(trn$class)
# knn.tst <- knn(trn[,2:10], tst[,2:10], trn$class,k=k)
# tst.error1[k] <-sum(knn.tst != tst$class)/length(tst$class)
# }
points(1:30, trn.error1, col="orange", type="b", pch=4)
points(1:30, tst.error1, col="black", type="b", pch=4)

t