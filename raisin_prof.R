ra<-read.table("raisin.dat", stringsAsFactors=T)
summary(ra)
str(ra)
nrow(ra)
ncol(ra)
#
library(mice)
md.pattern(arr)

library(caret)


# # 0.
# # accuracy
# table(ra$Class)/nrow(ra) # 0.5

# 1.
# k-means
library(factoextra)
library(cluster)

num.ra <- ra[,-8]
k2 <- kmeans(scale(num.ra), 2,nstart=3)
x<-table(k2$cluster, ra$Class)
x
acc <- print(1-sum(diag(x))/nrow(ra))
fviz_cluster(k2, data=scale(num.ra))
sil2<-silhouette(k2$cluster, dist(scale(num.ra)))
fviz_silhouette(sil2)

k3 <- kmeans(scale(num.ra), 3, nstart=3)
x<-table(k3$cluster, ra$Class)
x
fviz_cluster(k3, data=scale(num.ra))
sil3<-silhouette(k3$cluster, dist(scale(num.ra)))
fviz_silhouette(sil3)

fviz_nbclust(scale(num.ra),kmeans,method="silhouette")
fviz_nbclust(scale(num.ra),kmeans,method="wss")

# 2.
# logistic
set.seed(333)
idx <- sample(1:900, size=0.7*nrow(ra))
trn <- ra[idx,]
tst <- ra[-idx,]

lfit <- glm(Class~., data=trn, family=binomial)
summary(lfit)
BIC(lfit)

lfit <- glm(Class~.-Extent-Eccentricity, data=trn, family=binomial)
summary(lfit)
BIC(lfit)

lfit <- glm(Class~.-Extent-Eccentricity-1, data=trn, family=binomial)
summary(lfit) # lowest AIC
BIC(lfit)

pred <- predict(lfit,tst, type="response")
pred.class <- as.factor(ifelse(pred<0.5,"Besni","Kecimen"))
confusionMatrix(table(pred.class,tst$Class),positive="Kecimen")
# 0.837




# 3.
# PCA
library(factoextra)
library(cluster)



pc <- prcomp(num.ra, scale=T, center=T)
print(pc)

fviz_eig(pc)
fviz_pca_ind(pc, habillage=ra$Class)
fviz_pca_ind(pc, habillage= ra$Class, axes=c(1,3))
fviz_pca_ind(pc, habillage= ra$Class, axes=c(2,3))

fviz_pca_ind(pc, habillage= ra$Class, axes=c(1,2), addEllipses=TRUE, ellipse.level=0.95)

fviz_pca_var(pc, axes=c(1,2), repel=T)
fviz_pca_var(pc, axes=c(1,2), col.var="contrib", repel=T)
fviz_pca(pc, habillage=ra$Class, axes=c(1,2),label="var")

fviz_pca_var(pc,axes=c(2,3), repel=T)

pc.data <- as.data.frame(pc$x)
pc.data$Class <- ra$Class
pc.trn <- pc.data[idx,]
pc.tst <- pc.data[-idx,]
head(pc.trn)

pclfit <- glm(Class~PC1+PC2, data=pc.data, family=binomial)
summary(pclfit)
pred.pc <- predict(pclfit,pc.tst, type="response")
pred.class.pc <- as.factor(ifelse(pred.pc<0.5,"Besni","Kecimen"))
confusionMatrix(table(pred.class.pc,pc.tst$Class),positive="Kecimen") #0.844



# 4.
# RF - do CV for tuning
library(randomForest)
rf <- randomForest(Class~.,data=trn, importance=T) 
rf
pred_rf <- predict(rf,tst)
confusionMatrix(table(pred_rf,tst$Class),positive="Kecimen") #.837
varImpPlot(rf)


library(caret)
tctrl <- trainControl("oob")
rft <- train(Class~., data=trn, method='rf', trControl=tctrl, tuneLength=6)
rft

rft <- train(Class~., data=trn, method='rf', trControl=tctrl, tuneLength=6, ntree=1000)
rft

rft <- train(Class~., data=trn, method='rf', trControl=tctrl, tuneLength=6, ntree=2000)
rft

pred_rft <- predict(rft$finalModel, tst)
confusionMatrix(pred_rft, tst$Class) # 0.8481

library(pROC)
pred_rft_p <- predict(rft, tst, type="prob")
myroc <- roc(tst$Class ~ pred_rft_p[,2], plot=T, print.auc=T)
auc(myroc) #0.9122

rf3 <- randomForest(Class~.,data=trn, importance=T, mtry=3, ntree=2000) 
rf3
pred_rf3 <- predict(rf3,tst)
confusionMatrix(table(pred_rf3,tst$Class),positive="Kecimen") #.8444
varImpPlot(rf3)


rft.pc <- train(Class~., data=pc.trn, method='rf', trControl=tctrl, tuneLength=9)
rft.pc
pred_rft.pc <- predict(rft.pc$finalModel, pc.tst)
confusionMatrix(pred_rft.pc, pc.tst$Class) # 0.8481


rf.pc <- randomForest(Class~.,data=pc.trn, importance=T, mtry=6, ntree=2000) 
pred_rf.pc <- predict(rf.pc,pc.tst)
confusionMatrix(table(pred_rf.pc,pc.tst$Class),positive="Kecimen") #.8444
