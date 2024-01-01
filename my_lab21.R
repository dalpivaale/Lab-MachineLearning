#My_Lab21
library(factoextra)
library(cluster)
### Acute lymphoblastic leukemia (ALL) data
data<- read.table("ALL_data.data", stringsAsFactors=T)
dat <- data[,1:(ncol(data)-1)]	# the gene expression
cl <- as.factor(data$cl) 					# the true cell type
dim(dat)

#PCA
bc.pca <- prcomp(dat[,2:10],scale=T, center=T)#si becca le ultime 9 feature perchè la prima è l'ID mentre l'undicesima sono le classi. sdev è praticamente gli autovalori della matrice di covarianza
plot(bc.pca)#plotta gli eigenvalues. scale = T vuol dire che scala i dati prima di fare la PCA!!
bc.pca$sdev#autovalori. La prima componente è importantissima, spiega un sacco della varianza
head(bc.pca$x) # the coordinates of the observations on the PCs

print(bc.pca)

fviz_eig(bc.pca)#circa il 42%della varianza è spiegata dalla prima componente ovvero in una direzione delle 9 totali
fviz_pca_ind(bc.pca, habillage=cl, axes=c(1,2))#plot con le prime due componenti la prima asse x e la seconda asse y e noti che con sole queste due si distinguono bene i maligni dai benigni
fviz_pca_ind(bc.pca, habillage=cl, axes=c(1,3))#qua setta roba ma prendo la prima e la terza componente
fviz_pca_ind(bc.pca, habillage=cl, axes=c(2,3))#qua male, separati male, seconda e terza componente

fviz_pca_ind(bc.pca, habillage=dat$cl, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)#introduce ellissi per fare il clustering plottando le prime due componenti

fviz_pca_var(bc.pca, axes=c(1,2), repel=T)# le frecce sono le features e noti che puntano nella direzione dell'asse x circa ovvero la prima componente, ciò significa che sono spiegate per lo più dalla prima componente, mentre mitosis segue di più la seconda componente (dim2)
fviz_pca_var(bc.pca, axes=c(1,2), col.var="contrib", repel=T)#tutti plot analoghi e fighi per vedere le fetures quali componenti seguono di più
fviz_pca(bc.pca, habillage=bc$cl, axes=c(1,2),label="var")#plot molto figo sia delle features (frecce) che dei dati

fviz_pca_var(bc.pca,axes=c(2,3), repel=T)#thickness segue perlopiù la terza componente mentre mitoses la seconda, staltre un pò caotiche
fviz_pca(bc.pca, habillage=dat$cl, axes=c(1,3),label="var")



#CLUSTREING+PCA
hc.pca <- hclust(dist(pr.out$x[, 1:7])) #fa il clustering gerarchico beccandosi le prime 7 componenti
plot(hc.pca, labels = nci.labs, main = "Hier. Clust. on First Seven Score Vectors") 
rect.hclust(hc.pca, k=4)
table(cutree(hc.pca, 4), nci.labs)#non sembra male, sembra che del rumore sia stato eliminato

km.pca <- kmeans(dist(pr.out$x[, 1:7]), 4, nstart=20) #stessa cosa ma col k means
table(km.pca$cluster, cutree(hc.pca, 4))#compara i due clustering
table(km.pca$cluster, nci.labs)#tabella di come ha clusterizzato il kmeans
#il succo è vedere diversi approcci di clustering quale opera meglio e se applicandoci la PCA il clustering è ancora più efficace. Alcuni tipi di cancri sembrano più distinguibili dagli altri e meglio spiegati dalle prime componenti della pca