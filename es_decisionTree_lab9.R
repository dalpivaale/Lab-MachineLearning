#lab9 Esercizi decision tree
data <- read.table("bbd_data.dat", stringsAsFactors=T)

detach(data)
attach(data)
summary(data)
plot(data)
nrow(data)

dat = na.omit(data)

set.seed(123)
idx = sample(nrow(dat),1718)
trn = dat[idx,]
tst = dat[-idx,]

library(tree)
#dec_tree<-tree(diab~.,data = trn)
dec_tree <- tree(diab~., data=trn, control=tree.control(nrow(trn),minsize=40,mindev=0.001))#costruisci il tuo albero, con tutte le possibili covariate
#dec_tree2 <- tree(diab~., data=trn, split="gini")
par(mfrow=c(2,1)) # Two subplots
plot(dec_tree)
text(dec_tree)
#plot(dec_tree2)
#text(dec_tree2)
par(mfrow=c(1,1)) # Back to one subplot
tree_pred <- predict(dec_tree, tst, type="class")#predico il testset col mio albero (il mio modello)
library(caret)
confusionMatrix(tree_pred, tst$diab, positive = "no") #accuracy 0.87, sensitivity una merda infatti il modello fa fatica a trovare i soggetti sani questo anche perchè sono molto pochi

##PRUNING
dec_tree.cv <- cv.tree(dec_tree, FUN=prune.misclass)#crossvalidation sul mio albero considerando il missclassification error
plot(dec_tree.cv)#se size = 1 l'errore è massimo e sto classificando come la chance classification ho un solo nodo. size = 3 molto bene! size = numero nodi albero
misclass = prune.misclass(dec_tree)
lines(misclass$size, misclass$dev,col="red", type="s")

pr_tree_cv <- prune.tree(dec_tree,best=6)
plot(pr_tree_cv)
text(pr_tree_cv)
pr_pred_cv <- predict(pr_tree_cv, tst, type="class")#predicimao il testset col nuovo albero con  solo 3 foglie ed effettivamente l'accuracy è 0.74 un pò meglio
confusionMatrix(pr_pred_cv, tst$diab)
#sto albero più piccolo predice meglio, staltro overfitting

##
sorted <- sort(dat$weight,decreasing =  FALSE)
sorted
sorted >= 70
n <- subset(dat,dat$weight > 75 & height > 170)

newdata <- subset(dat, age > 24 | age < 65, 
                  select=c(height, Weight)) #qua ti prende il subset con le sole colonne che vuoi
library(tidyverse)
library(nycflights13)

arrange(dat, dat$weight)
