data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data

set.seed(3)
idx = sample(nrow(pima.data),250)
trn = pima.data[idx,]
tst = pima.data[-idx,]

library(tree)
dec_tree <- tree(diabetes~., data=trn)#costruisci il tuo albero, con tutte le possibili covariate. di default se non specifichi usa la devianza come criterio di split
dec_tree2 <- tree(diabetes~., data=trn, split="gini") #split col criterio gini index
par(mfrow=c(2,1)) # Two subplots
plot(dec_tree)
text(dec_tree)
plot(dec_tree2)
text(dec_tree2)
par(mfrow=c(1,1)) # Back to one subplot
tree_pred <- predict(dec_tree, tst, type="class")#predico il testset col mio albero (il mio modello)
library(caret)
confusionMatrix(tree_pred, tst$diabetes) #accuracy 0.72 bene ma non chissà che

# xx_Tree <-snip.tree(dec_tree) 
# # first plot dec_tree, click once to see deviance by cutting at node, click again to remove subtree, Right-clich to end
# plot(xx_tree)


#dec_tree.cv <- cv.tree(dec_tree, FUN=prune.tree)
dec_tree.cv <- cv.tree(dec_tree, FUN=prune.misclass)#crossvalidation sul mio albero (dec_tree) considerando/valutando il missclassification error
plot(dec_tree.cv, ylim=c(20,100))#Size è il numero di nodi dell'albero. se size = 1 l'errore è massimo e sto classificando come la chance classification ho un solo nodo. size = 3 molto bene! size = numero nodi albero
misclass = prune.misclass(dec_tree)#RICORDA: se vuoi valutare la best size usando il missclassfication error devi fare la corssvalidation! questo perchè ovvio che sto errore diminuisce col size dell'albero però c'è overfitting
lines(misclass$size, misclass$dev,col="red", type="s")#la linea rossa è il missclassification senza fare la cross-validation
#dal plot si nota che la size migliore = 3 quindi qua sotto potiamo l'albero a 3 nodi
pr_tree_cv <- prune.tree(dec_tree,best=3)
plot(pr_tree_cv)
text(pr_tree_cv)
pr_pred_cv <- predict(pr_tree_cv, tst, type="class")#predicimao il testset col nuovo albero potato,  solo 3 foglie ed effettivamente l'accuracy è 0.74 un pò meglio
confusionMatrix(pr_pred_cv, tst$diabetes)
#sto albero più piccolo predice meglio, staltro overfitting

plot(glucose~mass, data=tst, col=tst$diabetes, pch=20)#plot dell'albero su un piano cartesiano. divide il dataset in box tra diabetici e sani a seconda degli split dell'albero
abline(h=127.5, col=4)#tira le linee veticali e orizzontali che sarebbero gli split dell'albero
abline(v=29.6, col=4)


## with rpart package
library(rpart)
tr<- rpart(diabetes~.,data=trn)
plot(tr)
text(tr, use.n=T)
library(rattle)
fancyRpartPlot(tr)

# pruning via caret::train Assa stare
train.control <- trainControl(method="LOOCV")
caret.tree = train(diabetes~., data=trn, method="rpart", trControl = train.control, tuneLength=10)
caret.tree
fancyRpartPlot(caret.tree$finalModel)

pred_caret <- predict(caret.tree$finalModel, tst, type="class")
confusionMatrix(pr_pred_cv, tst$diabetes)
