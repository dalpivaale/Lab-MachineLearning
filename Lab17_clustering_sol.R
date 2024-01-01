### Acute lymphoblastic leukemia (ALL) data
data<- read.table("ALL_data.data", stringsAsFactors=T)
dat <- data[,1:(ncol(data)-1)]	# the gene expression
cl <- as.factor(data$cl) 					# the true cell type
dim(dat)

# create new data set with only the 100 genes with highest variance
genes.var<-apply(dat,2,var) 					# variance of genes, see ?apply
genes.var.select <- order(-genes.var)[1:100] 	# see ?order
dat.s <- dat[,genes.var.select]
dim(dat.s)

# Do cluster analysis on the complete and reduced data sets. 
# Can you recover the true cell types? 
# Try both hierarchical clustering and k-means. Which k is optimal?
# Try also k-mediods. Which k is optimal?

library(NbClust)
library(factoextra)
library(cluster)

d <- dist(dat)
image(as.matrix(d))

d.s <- dist(dat.s)
image(as.matrix(d.s))

k <- kmeans(dat,centers=2) #repeat - different results!
table(k$cluster,cl)#compara con cl  valori veri
k$tot.withinss
sil <- silhouette(k$cluster,dmatrix=as.matrix(d))
fviz_silhouette(sil)

k10 <- kmeans(dat, centers=2, nstart=10) 
# nstart>1 indicates to use more random initial centers and finally choose best
table(k10$cluster,cl)



k1 <- kmeans(dat,centers=3) #con K = 3 ottieni 3 cluster in uno hai tutte T cell e negli altri due tutte B cell (mi pare) in questi due distingue magari i cecchi dagli anziani con ambedue B cell, può aver senso
table(k1$cluster,cl)
k$tot.withinss
sil <- silhouette(k1$cluster,dmatrix=as.matrix(d))
fviz_silhouette(sil)

k.s <- kmeans(dat.s,centers=2)
table(k.s$cluster,cl)
sil.s <- silhouette(k.s$cluster,dmatrix=as.matrix(d.s))
fviz_silhouette(sil.s)


fviz_nbclust(dat,kmeans, method="wss", k.max=10)#non hai una L ma sembrerebbe essere attorno a 3-4-5 il k migliore
fviz_nbclust(dat,kmeans, method="silhouette", k.max=10) #silhouette più alta per K = 3 quindi probabilmente è lui il k perfetto, ci sta per k = 3 i cluster sono molto buoni. 
# note for n=2 avg sil = 0.08 < value above!
# you could loop over k, use nstart>1 and for each k choose best based on wss or avg sil

fviz_nbclust(dat.s,kmeans, method="wss", k.max=10)#nel dataset ridotto invece K = 2 è il migliore si vede sia il WSS che nel grafico della sihlouette
fviz_nbclust(dat.s,kmeans, method="silhouette", k.max=10)

## k-medoids

kmed<- pam(dat,k=2)#k moids fa meglio del k means con sto dataset. 
table(kmed$cluster,cl)
kmed$tot.withinss

kmed3<- pam(dat,k=3)
table(kmed3$cluster,cl)

kmed.s<- pam(dat.s,k=2)
table(kmed.s$cluster,cl)

fviz_nbclust(dat,cluster::pam, method="wss")
fviz_nbclust(dat.s,cluster::pam, method="wss")
fviz_nbclust(dat,cluster::pam)
fviz_nbclust(dat.s,cluster::pam)
#Va bene anche la mia soluzione, cmq k-means con l'intero dataset il top è K=3 , con il dataset ridotto il top è 2 e clusterizza perfettamente