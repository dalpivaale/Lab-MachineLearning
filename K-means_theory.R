### CLUSTERING
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data

library(NbClust)
library(factoextra)
library(cluster)


no.diab.data <- scale(pima.data[,-9])#rimuove l'ultima colonna inerente le label per effettuare il clustering non supervisionato. facciamo finta quindi di non avere le labels. e scala i dati quinid ogni colonna avrà varianza 1 e media nulla. importante scalare i dati 
k.diab2 <- kmeans(no.diab.data, 2)#in tal modo puoi comparare tranquillamente i dati fra loro. fa il k-means con K = 2. noti che ci sono due cluster, per ciascuna feature ha calcolato il centro (la media)
k.diab2$centers #centroidi dei clusters per ogni feature, ce ne sono di negativi perchè abbiamo scalato prima
k.diab2$tot.withinss#totale somma dei quadrati entro i clusters, più è piccola più sono coesi i dati nei cluster
k.diab2$tot.withinss/k.diab2$totss#ste due qua sono tipo il totale della varianza possibile, la loro somma fa 1. assa stare
k.diab2$betweenss/k.diab2$totss

#IN k.diab2 trovi i sum of squares for each cluster!!!

x=table(k.diab2$cluster,pima.data$diabetes)#compariamo i cluster con le label dei diabetici
x
accuracy=sum(diag(x))/sum(sum(x))
accuracy # 0.745

sil<-silhouette(k.diab2$cluster,dist(no.diab.data))#calcola la sihlouette, gli passi i cluster e con dist gli passi tutte le distanze fra i dati del dataset
fviz_silhouette(sil)#grafico indice di sihlouette dei due cluster, più sono vicini ad 1 meglio è la clusterizzazione e il k, qua notiamo che cluster1 ha addirittura valori negativi quindi fa abba schifo
#ricorda che la sihlouette top è 1, -1 è patologica
plot(glucose~ mass, data=no.diab.data, col= pima.data$diabetes, pch=10*k.diab2$cluster, cex=2)#non ho capito..

fviz_cluster(k.diab2, no.diab.data[,c(6,2)],ellipse.type="norm")#figure dei due  cluster, non il massimo

# how many clusters?
#calcola Wss per K da 1 a 20 per avere la curva col ginocchio e capire/avere un idea del k migliore
K=20
wss=rep(NA,K)#vettore lungo k con valori Nan
for(k in 1:K){
  k.diab <- kmeans(no.diab.data, k)
  wss[k] <- k.diab$tot.withinss#in wss ci mette la somma totale dei quadrati delle distanze  dei cluster
}
plot(1:K,wss, type='b')#curva dei wss, non c'è un vero ginocchio per capire il k migliore conviene andare di silhouette

#?fviz_nbclust
fviz_nbclust(no.diab.data,kmeans,method="wss",k.max=30)#anzichè fare il ciclo for puoi fare direttamente così.. fa anche il plot spettacolo

fviz_nbclust(no.diab.data,kmeans,method="silhouette",k.max=30)#grafico silhouette vs k, direi che il k migliore è k = 6

fviz_nbclust(no.diab.data,kmeans,method="gap_stat",k.max=30)

k.diab3 <- kmeans(no.diab.data, 3)
sil3<-silhouette(k.diab3$cluster,dist(no.diab.data))
fviz_silhouette(sil3)

NbClust(no.diab.data, method="kmeans")#altra funzione che performa il kmeans, caga fuori un mega risultato interessante dove consiglia il k migliore

# k-medoids lo usi nei casi particolari in cui non puoi fare la media dei dati tipo se hai colori o il sesso, non è che fai la media del giallo verde e rosso..
kmed <- pam(no.diab.data, 2)#gli passi il dataset e il k
kmed$medoids#ecco i medoids che sono i centroidi "forzati" come data points
no.diab.data[131,]
