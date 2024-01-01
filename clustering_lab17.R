
### Acute lymphoblastic leukemia (ALL) data
data<- read.table("ALL_data.data", stringsAsFactors=T)#è un dataset con due tipi di cellule A e B. Dataset tipico della bioinformatica con pochi samples ma tante features
dat <- data[,1:(ncol(data)-1)]				# the gene expression. ci toglie l'ultima colonna  che è quella della classe. si prende tutte le righe e ttutte le colonne tranne l'ultima
cl <- as.factor(data$cl) 					# the true cell type. la classe true la butta dentro a cl con la quale confronteremo alla fine
dim(dat)


# create new data set with only the 100 genes with highest variance (quindi quelli con varianza più elevata) fa sto casino per estrarre un subset del dataset 
genes.var<-apply(dat,2,var) 					# variance of genes, see ?apply
genes.var.select <- order(-genes.var)[1:100] 	# see ?order seleziona i primi 100 geni con la varianza più alta (noise,outliers) ordinati dal più bassso al più alto, quindi l'ultimo sarà un outlier disumano. si becca gli indici dei dati
dat.s <- dat[,genes.var.select]
dim(dat.s)#128 cellule (righe) con solo 100 geni dei 12000

# Do k-means cluster analysis on the complete and reduced data sets. 
# Can you recover the true cell types? #In the se
# Which k is optimal?
# Try also k-mediods. Which k is optimal?

# Do SVM analysis

library(NbClust)
library(factoextra)
library(cluster)

k.dat <- kmeans(dat,2)
k.dat$tot.withinss


x=table(k.dat$cluster,cl)#compariamo i cluster con le label 
x#nel cluster 1 ci sono 54 cellule B e 16 T, nel cluster 2 41 cellule B e 17 T
accuracy=sum(diag(x))/sum(sum(x))
accuracy #0.55 schifo

###Silhouette
sil<-silhouette(k.dat$cluster,dist(dat))#calcola la sihlouette, gli passi i cluster e con dist gli passi tutte le distanze fra i dati del dataset
fviz_silhouette(sil)#poco meno di 0.25 e vi sono alcuni valori negativi non un granchè
fviz_cluster(k.dat, dat[,c(6,2)],ellipse.type="norm")#figure dei due  cluster. i due cluster sono parecchio sovrapposti, si può recuperare solo per una parte di dataset la classe di appartenenza secondo me
###
K=20
wss=rep(NA,K)#vettore lungo k con valori Nan
for(k in 1:K){
  k.dat <- kmeans(dat, k)
  wss[k] <- k.dat$tot.withinss#in wss ci mette la somma totale dei quadrati delle distanze  dei cluster
}
plot(1:K,wss, type='b')#curva dei wss, il ginocchio è a meno di 5 tra 2 e 3
fviz_nbclust(dat,kmeans, method="wss", k.max=10)#anzichè il ciclo for ti conviene usare questo comando per vedere il Wss si nota il ginocchio vicino a 3, quindi k = 3 sembrerebbe il migliore
NbClust(dat, method="kmeans")# dataset troppo grosso non runna la funzione. usa anche questa funziona (per vedere il k migliore) che fa direttamente il k means e ti dice il k migliore

###Facciamo stessa cosa per il dataset più piccolo ma varianza maggiore

k.dat <- kmeans(dat.s,2)
k.dat$tot.withinss


x=table(k.dat$cluster,cl)#compariamo i cluster con le label 
x #il cluster 1 contiene tutte le cellule T mentre il cluster2 solo le cellule B quindi sembrerebbe perfetto oppure mi sa che calssifica esattamente l'opposto non capisco
accuracy=sum(diag(x))/sum(sum(x))
accuracy #qua clusterizza perfettamente, NON importa se gli zeri sono sulla diagonale principale è uguale , basta che siano due zeri in diagonale!!

###Silhouette
sil<-silhouette(k.dat$cluster,dist(dat.s))#calcola la sihlouette, gli passi i cluster e con dist gli passi tutte le distanze fra i dati del dataset
fviz_silhouette(sil)#top neanche un valore negativo
fviz_cluster(k.dat, dat.s[,c(6,2)],ellipse.type="norm")#figure dei due  cluster
###
K=20
wss=rep(NA,K)#vettore lungo k con valori Nan
for(k in 1:K){
  k.dat <- kmeans(dat.s, k)
  wss[k] <- k.dat$tot.withinss#in wss ci mette la somma totale dei quadrati delle distanze  dei cluster
}
plot(1:K,wss, type='b')#ginocchio a 2 si vede quinid k perfetto è 2 
fviz_nbclust(dat,kmeans, method="wss", k.max=10)#comando che fa la stessa del ciclo for

NbClust(dat.s, method="kmeans")#Lui clusterizza e dice che il miglior numero di cluster è 2
##Kmedoids
kmed <- pam(dat.s, 2)#gli passi il dataset e il k
kmed$medoids#ecco i medoids che sono i centroidi "forzati" come data points
x=table(kmed$cluster,cl)#compariamo i cluster con le label dei diabetici
x
accuracy=sum(diag(x))/sum(sum(x))
accuracy#1 perfetta accuratezza clustering top da cui riesci a ricavare il tipo di cellula
sil<-silhouette(kmed$cluster,dist(dat.s))#calcola la sihlouette, gli passi i cluster e con dist gli passi tutte le distanze fra i dati del dataset
fviz_silhouette(sil)#spettacolo ottieni due cluster distinti
fviz_cluster(kmed, dat.s[,c(6,2)],ellipse.type="norm")
#Non serve scalare i dati in questo caso dice il prof xk sono già puliti.
