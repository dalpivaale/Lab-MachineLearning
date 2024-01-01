#esempio per spiegare i surrogati negli alberi decisionali LAB11
tmp_df <- data.frame(Y = as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)), weight = 10:1, height = c(10:7, 5, 6, 4:1))#crea un dataframe per condurre l'esempio
#tmp_df <- data.frame(Y = as.factor(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)), weight = 10:1, height = c(9,10,8:1))
tmp_df$weight[3] <- NA
tmp_df #questo è il dataset e stimao cercando di predirre la variabile Y, si nota che la classe splitta a weigth =  5.5


library(rpart)
tm_0 <- rpart(Y ~ weight + height, data = tmp_df,  
              control = rpart.control(minsplit = 1, minbucket=1, cp=0, maxdepth = 1, usesurrogate = 0))#costruisce l'albero senza surrogate ma con tutto il dataset

tm_00 <- rpart(Y ~ weight + height, data = tmp_df,  na.action=na.omit,
               control = rpart.control(minsplit = 1, minbucket=1, cp=0, maxdepth = 1, usesurrogate = 0))#costruisce l'albero togliendo il NAs e non ci sono surrogati

tm   <- rpart(Y ~ weight + height, data = tmp_df,  
              control = rpart.control(minsplit = 1, minbucket=1, cp=0, maxdepth = 1))#qua invece usa la variabile surrogata

summary(tm_0)#nel pirmo albero il primo split è con weight = 5.5 improve sarebbe il gini index, height = 4.5 sarebbe un altro valore possibili per il primo split (come optional) agree =0.889 ovvero 89% indica la percentuale di correttezza nello split usato il surrogato (height < 4.5) è buona sbaglia solo un dato il 6 che andrebbe a DX mentre con lo split normale andrebbe a sx essendo weigth = 5
summary(tm_00)#questo escludeva i NAs infatti  ci sono 9 osservazioni, me par de capire che xe compagno
summary(tm)#questo albero fa le predizioni usando la variabile surrogata per il dato contente il NAs


par(mfrow=c(1,2))
plot(tm_0)#non ha detto nulla qui
text(tm_0)
plot(tm)
text(tm)

#ora prendiamo questo dataset con dei NAs, fai finta si ail testset se vuoi e facciamo le sue predizioni con i tre alberi di prima
tmp_new_df <- data.frame(weight = c(rep(NA_real_, 4), 3:6), height = rep(3:6, 2))#i primi 4 dati contengono NAs mentre gli ultimi 4 sono a posto
tmp_new_df

predict(tm_0,tmp_new_df)#qua noto che l'albero non sa classificare per i primi 4 dati infatti assegna una prob del 50%, perchè non sa se andare a destra o sx (si ferma allo split) mentre per gli ultimi 4 assegna una prob. del 100% perchè sa se andare a dx o sx
predict(tm_00,tmp_new_df)
predict(tm,tmp_new_df)#qua l'albero usa la variabile surrogata height per i primi 4 dati che contengono NAs mentre per gli ultimi 4 usa il weight e si vede che predice correttamente

##
library(tree)
tr <- tree(Y ~ weight + height, data = tmp_df, control=tree.control(nobs=10,mincut=1, minsize=2, mindev=0))
tr
predict(tr,tmp_new_df) # as tm_00, removes NAs


## predict missing values with tree
prtr <- rpart(weight~height, data=tmp_df, control = rpart.control(minsplit = 1, minbucket=2, cp=0, maxdepth = 4))
summary(prtr)
plot(prtr)
text(prtr)
predict(prtr, tmp_df[3,])