### Acute lymphoblastic leukemia (ALL) data
data<- read.table("ALL_data.data", stringsAsFactors=T)
dat <- data[,1:(ncol(data)-1)]				# the gene expression.DATASET COMPLETO
cl <- as.factor(data$cl) 					# the true cell type
dim(dat)


# create new data set with only the 100 genes with highest variance
genes.var<-apply(dat,2,var) 					# variance of genes, see ?apply
genes.var.select <- order(-genes.var)[1:100] 	# see ?order
dat.s <- dat[,genes.var.select]#DATASET CON SOLO 100GENI
dim(dat.s)

# Do k-means cluster analysis on the complete and reduced data sets. 
# Can you recover the true cell types? No performance troppo basse sia per K =2 che per K = 4
# Which k is optimal?
# Try also k-mediods. Which k is optimal?

no.diab.data <- scale(dat.s)#rimuove l'ultima colonna inerente le label per effettuare il clustering non supervisionato. facciamo finta quindi di non avere le labels. e scala i dati quinid ogni colonna avrà varianza 1 e media nulla. importante scalare i dati 
k.diab2 <- kmeans(no.diab.data, 2)#in tal modo puoi comparare tranquillamente i dati fra loro. fa il k-means con K = 2. noti che ci sono due cluster, per ciascuna feature ha calcolato il centro (la media)
k.diab2$centers #centroidi dei clusters per ogni feature, ce ne sono di negativi perchè abbiamo scalato prima
k.diab2$tot.withinss#totale somma dei quadrati entro i clusters, più è piccola più sono coesi i dati nei cluster
k.diab2$tot.withinss/k.diab2$totss#ste due qua sono tipo il totale della varianza possibile, la loro somma fa 1. assa stare
k.diab2$betweenss/k.diab2$totss

#IN k.diab2 trovi i sum of squares for each cluster!!!

x=table(k.diab2$cluster,cl)#compariamo i cluster con le label dei diabetici
x
accuracy=sum(diag(x))/sum(sum(x))
accuracy # 0.53 na merda con k = 2.

sil<-silhouette(k.diab2$cluster,dist(no.diab.data))#calcola la sihlouette, gli passi i cluster e con dist gli passi tutte le distanze fra i dati del dataset
fviz_silhouette(sil)#grafico indice di sihlouette dei due cluster, più sono vicini ad 1 meglio è la clusterizzazione e il k, qua notiamo che cluster1 ha addirittura valori negativi quindi fa abba schifo

fviz_cluster(k.diab2, no.diab.data[,c(6,2)],ellipse.type="norm")#Na merda

# how many clusters?
#calcola Wss per K da 1 a 20 per avere la curva col ginocchio e capire/avere un idea del k migliore
K=20
wss=rep(NA,K)#vettore lungo k con valori Nan
for(k in 1:K){
  k.diab <- kmeans(no.diab.data, k)
  wss[k] <- k.diab$tot.withinss#in wss ci mette la somma totale dei quadrati delle distanze  dei cluster
}
plot(1:K,wss, type='b')#Dal ginocchio sembrerebbe K = 4 il top

#?fviz_nbclust
fviz_nbclust(no.diab.data,kmeans,method="wss",k.max=30)#K = 4 o 5 

fviz_nbclust(no.diab.data,kmeans,method="silhouette",k.max=30)#grafico silhouette vs k, direi che il k migliore è k = 4 anche per la sihlouette

fviz_nbclust(no.diab.data,kmeans,method="gap_stat",k.max=30)

k.diab3 <- kmeans(no.diab.data, 3)
sil3<-silhouette(k.diab3$cluster,dist(no.diab.data))
fviz_silhouette(sil3)

NbClust(no.diab.data, method="kmeans")
