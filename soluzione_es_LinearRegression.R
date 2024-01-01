#Soluzione professore es_linearRegression
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2) # remove rows with missing data


plot(pressure~mass, data=pima.data)
plot(pressure~age, data=pima.data)
plot(mass~age, data=pima.data)

lin <- lm(pressure~., data=pima.data)
summary(lin) #R^2 = 0.1882
AIC(lin)#3029.6
BIC(lin)#3069.4

lin1 <- lm(pressure~mass+age+glucose+insulin+pedigree, data=pima.data)
summary(lin1)#0.187
AIC(lin1) # lower  3024
BIC(lin1)#3052

lin2 <- lm(pressure~mass+age, data=pima.data)
summary(lin2)#R^2 = 0.1708
AIC(lin2)#3025
BIC(lin2) # 3041 lower

lin3 <- lm(pressure~mass, data=pima.data)
summary(lin3)#0.0926
AIC(lin3)#3059
BIC(lin3)#3071
#Concludendo Lin1 e lin2 sono i modelli migliori o meglio i meno peggio avendo R^2 più vicino ad 1 e AIC BIC più bassi ovvero la distanza dei dati dalla retta

### cross-validation ###  
library(caret)
# Define training control
train.control <- trainControl(method = "LOOCV")#fa la stessa cosa con la cross validation 

model <- train(formula(lin), data = pima.data, method = "lm", trControl = train.control)
print(model1)#RMSE 11.54 e R^2=0.1467

model1 <- train(formula(lin1), data = pima.data, method = "lm", trControl = train.control)
print(model2)#RMSE = 11.46 e R^2 = 0.1577

model2 <- train(formula(lin2), data = pima.data, method = "lm", trControl = train.control)
print(model2)#RMSE = 11.46 e R^2 = 0.1577

#il model3 non c'ho cazzi cmq model1 e model2 sono i migliori
#
