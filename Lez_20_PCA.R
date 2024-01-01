library(ISLR2)
library(factoextra)
library(cluster)

nci.labs <- NCI60$labs #labels
nci.data <- NCI60$data #data
dim(nci.data)#64 tipi di cellule (lines) 6830 genii (colonne)

head(nci.labs)
table(nci.labs)

pr.out <- prcomp(nci.data, scale=F)#qua fa la PCA del mio dataset senza scalare i genii per non perdere informazione su alcune cose. A volte coviene provare a scalare o mneo e vedere i due risultati

Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))]) 
}

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z3")

fviz_pca_ind(pr.out, habillage=nci.labs) # plotta i dati e le prime due componenti, dalle percdntuali si coglie che non spiegano molta varianza (14,9% la prima, 8,3% la seconda)
fviz_pca_ind(pr.out, habillage=nci.labs, axes=c(2,3)) # same. stessa roba ma confronta seconda e terza componente. i punti vicini e dello stesso gene sono clusterizzabili

plot(pr.out)#plot della varianza spiegata dalle componenti ma non si capisce meglio utilizzare l'istruzione qui sotto
fviz_eig(pr.out, geom="line",ncp=64)#SPETTACOLO QUESTA ISTRUZIONE!! anche qua su sto grafico, il numero di componenti necessarie sono quelle prima del ginocchio della curva

#ok adesso vuole fare il clustering dei dati
sd.data <- scale(nci.data)#ora si che scala i dati
sd.data <- nci.data#ci cambia nome

#GERARCHICAL CLUSTERING!!
par(mfrow=c(1,3))
data.dist <- dist(sd.data)#calcola le distanze fra i punti
plot(hclust(data.dist), xlab = "", sub = "", ylab = "", labels = nci.labs,main = "Complete Linkage") #plotta le tre tipologie di cluster. main è il titolo. il complete linkage sembra il migliore dei 3
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", sub = "", ylab = "")

hc.out <- hclust(dist(sd.data))#si becca il complete linkage e ne fa il plot del cluster. se non specifichi "method" fa il complete linkage
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs) #plot dell'albero
rect.hclust(hc.out, k=4)#taglia l'albero per formare 4 cluster (i rettangoli), il taglio lo fa con l'struzione sotto qui fa solo i rettangoli per vedere come sarebbero i cluster
hc.clusters <- cutree(hc.out, 4)#taglia l'albero in modo da fromare 4 clusters
table(hc.clusters, nci.labs)#numero di cancri per cluster, il primo contiene BREST CNS MELANOMA ecc.. il quarto cluster COLON e BREAST e così via. Puoi provare con un K diverso per vedere se fa meglio

#KMEANS
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20) #nstart ripete il K means 20 volte cambiando ogni volta i centroidi ed infine becca quelli migliori
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)#comparandoli in questa tabella si nota che non clusterizzano allo stesso modo
table(km.clusters, nci.labs)

#OK A STO PUNTO vuole fare il clustering sulle componenti della PCA, poichè prime invece lo stavamo facendo sull'intero dataset. Cambia il risultato?

hc.pca <- hclust(dist(pr.out$x[, 1:7])) #fa il clustering gerarchico beccandosi le prime 7 componenti
plot(hc.pca, labels = nci.labs, main = "Hier. Clust. on First Seven Score Vectors") 
rect.hclust(hc.pca, k=4)
table(cutree(hc.pca, 4), nci.labs)#non sembra male, sembra che del rumore sia stato eliminato

km.pca <- kmeans(dist(pr.out$x[, 1:7]), 4, nstart=20) #stessa cosa ma col k means
table(km.pca$cluster, cutree(hc.pca, 4))#compara i due clustering
table(km.pca$cluster, nci.labs)#tabella di come ha clusterizzato il kmeans
#il succo è vedere diversi approcci di clustering quale opera meglio e se applicandoci la PCA il clustering è ancora più efficace. Alcuni tipi di cancri sembrano più distinguibili dagli altri e meglio spiegati dalle prime componenti della pca