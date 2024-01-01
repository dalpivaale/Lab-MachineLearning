#DAL PIVA ALESSANDRO   2014703
ct<-read.table("cancer.dat", stringsAsFactors=T)
summary(ct)

library(FNN)
set.seed(3)
idx = sample(nrow(ct),400)
trn = ct[idx,]
tst = ct[-idx,]

summary(tst)
chance <- table(tst$class)/nrow(tst)
chance #chance accuray is 0.674

#KNN
pred <- knn(scale(trn[,2:11]),scale(tst[,2:11]),trn$class,k=3)
accuracy = sum(pred==tst$class)/nrow(tst)
accuracy#0.899
# confusion matrix
x<- table(pred,tst$class)
x
sum(diag(x))/sum(sum(x)) #accuracy = 0.899
# Vary k, the number of nearest neighbors
KK=50
accuracy=rep(0,KK)

for(k in 1:KK){
  pred <- knn(scale(trn[,2:11]),scale(tst[,2:11]),trn$class,k=k)
  accuracy[k] = sum(pred==tst$class)/nrow(tst)
}

plot(1:KK,accuracy,type='b',ylim=c(0.5,0.99),xlab='k')#for k = 3 the accuracy is good  maybe k = 2 is better
abline(h=max(chance),lty=2)

##PCA
library(factoextra)
library(cluster)

bc.pca <- prcomp(trn[,2:11],scale=T, center=T)#. 
plot(bc.pca)#eigenvalues plot
bc.pca$sdev
head(bc.pca$x) # the coordinates of the observations on the PCs

print(bc.pca)

fviz_eig(bc.pca)#54.8% of variance in explained by only first component, the second one explained about 24.8% of the variance
fviz_pca_ind(bc.pca, habillage=trn$class)#only with the first and second component there is no perfect distinction of data
fviz_pca_ind(bc.pca, habillage=trn$class, axes=c(1,3))
fviz_pca_ind(bc.pca, habillage=trn$class, axes=c(2,3))

fviz_pca_ind(bc.pca, habillage=trn$class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)

fviz_pca_var(bc.pca, axes=c(1,2), repel=T)#Variables-PCA
fviz_pca_var(bc.pca, axes=c(1,2), col.var="contrib", repel=T)
fviz_pca(bc.pca, habillage=trn$class, axes=c(1,2),label="var")
fviz_pca_var(bc.pca,axes=c(2,3), repel=T)
fviz_pca(bc.pca, habillage=trn$class, axes=c(1,3),label="var")

fviz_eig(bc.pca, geom="line",ncp=64)#knee at 3 components so i suggest use the first two components to rapresent data.


##CLASSIFICATION TREE
library(tree)
dec_tree <- tree(class~., data=trn, control=tree.control(nrow(trn),minsize=40,mindev=0.001))
dec_tree2 <- tree(class~., data=trn, split="gini")
par(mfrow=c(2,1)) # Two subplots
plot(dec_tree)
text(dec_tree)
plot(dec_tree2)
text(dec_tree2)#Trees plot to look splitting and most important features
par(mfrow=c(1,1)) # Back to one subplot
tree_pred <- predict(dec_tree, tst, type="class")
tree_pred2 <- predict(dec_tree2, tst, type="class")
library(caret)
confusionMatrix(tree_pred2, tst$class, positive = "M")#accuracy 0.90, sensitivity = 0.89,specificity = 0.91

pc.data <- as.data.frame(bc.pca$x)
pc.data$class <- trn$class
pc.trn <- pc.data[idx,]
pc.tst <- pc.data[-idx,]
head(pc.trn)

pclfit <- tree(class~PC1+PC2, data=pc.tst,split = "gini")
summary(pclfit)
pred.pc <- predict(pclfit,pc.tst, type="class")
#pred.class.pc <- as.factor(ifelse(pred.pc<0.5,"M","B"))
confusionMatrix(pred.pc, pc.tst$class, positive = "M")#accuracy =0.95, sensitivity = 0.97, specificity = 0.94

##PRUNING
dec_tree.cv <- cv.tree(dec_tree, FUN=prune.misclass)#crossvalidation sul mio albero considerando il missclassification error
plot(dec_tree.cv)#size = 6 is the best size looking missclassification error
misclass = prune.misclass(dec_tree)
lines(misclass$size, misclass$dev,col="red", type="s")

pr_tree_cv <- prune.tree(dec_tree,best=6)
plot(pr_tree_cv)
text(pr_tree_cv)
pr_pred_cv <- predict(pr_tree_cv, tst, type="class")#accuracy 0.911, sensitivity = 0.90, specificity = 0.927.
confusionMatrix(pr_pred_cv, tst$class)

###CLUSTERING
library(cluster)
data.dist <- dist(scale(ct))i

sd.data <- scale(ct[,2:11])

par(mfrow=c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist),main = "Complete Linkage") 
plot(hclust(data.dist, method = "average"), main = "Average Linkage")
plot(hclust(data.dist, method = "single"), main ="Single linkage")

hc.out <- hclust(dist(sd.data))
par(mfrow = c(1, 1))
plot(hc.out, labels = ct$class) 
rect.hclust(hc.out, k=4)
hc.clusters <- cutree(hc.out, 4)#cut dendogram to obtain 4 clusters
table(hc.clusters, ct$class)


