data("PimaIndiansDiabetes2", package = "mlbench")

# MICE
library(mice)
df <- PimaIndiansDiabetes2
summary(df)#noto che insulin e triceps hanno tanti NAs
head(df)
md.pattern(df)#plotta un pattern dei NAs per ogni feature: 140 pazienti hanno un NA nella colonna dell'insulin, 192 si in insulin che in triceps ecc..

fit0 <- lm(pressure ~age+insulin, data=df)#linear model con missing values. di default rimuove tutti i NAs. infatti sono 392-1 i pazienti che considera ovvero quelli senza NAs
summary(fit0)#pressure è legata molto a age ma non a insulin
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 59.085798   1.954035  30.238  < 2e-16 ***
# age          0.357488   0.060437   5.915 7.25e-09 ***
# insulin      0.003556   0.005190   0.685    0.494   

imp <- mice(df, method="mean", m=1, maxit=1)#mean significa che assegna la media della colonna ai NAs di quella colonna, m = numero multiple imputations, maxit = numero massimo di iterazioni
complete(imp)#risultato: per l'insulina c'è il 155,82 che si ripete a manetta era tutti missing value quelli
summary(complete(imp))#vedi che non ci sono più missing valure

par(mfrow=c(1,2))
hist(df$insulin, breaks=50, ylim=c(0,400))
hist(complete(imp)$insulin, breaks=50, ylim=c(0,400))
par(mfrow=c(1,1))

sd(df$insulin, na.rm=T)
sd(complete(imp)$insulin, na.rm=T)#si abbassa un pò la varianza con le imputazioni

fit <- lm(pressure ~age+insulin, data=complete(imp))
summary(fit)#Nonostante le imputazioni dove abbiamo sostituito i NAs con la media della colonna non camnbia molto il modello questo perchè non erano così significativi i NAs

fit <- with(imp, lm(pressure ~age+insulin) )
summary(fit)

imp <- mice(df, method = "norm.predict", m = 1, maxit = 1)  # use predicted values. Usa un altro modo per fare le imputazioni
fit <- with(imp, lm(pressure ~age+insulin) )#, fitta i dati mancanti del dataset con le imputazioni e ne fa il modello in questo caso regressine linerare. Se hai m = 30 hai 30 regressioni linerari che fanno le predizioni e con pool metti assieme i risultati (predizioni) dei 30 modelli.
summary(fit) # insulin has borderline significant effect!

par(mfrow=c(1,2))
hist(df$insulin, breaks=50, ylim=c(0,400))
hist(complete(imp)$insulin, breaks=50, ylim=c(0,400))
par(mfrow=c(1,1))


plot(pressure~age, data = complete(imp), pch=4, col="red")
points(pressure~age, data=df,pch=20)
plot(pressure~insulin, data = complete(imp), pch=4, col="red")
points(pressure ~insulin, data=df,pch=20)
plot(age~insulin, data = complete(imp), pch=4, col="red")
points(age ~insulin, data=df,pch=20)

imp <- mice(df, method = "pmm", m = 1, maxit = 1)  # use predicted values
fit <- with(imp, lm(pressure ~age+insulin) )#fitta i dati mancanti con le impitazioni imp
summary(fit) # insulin has borderline significant effect!

par(mfrow=c(1,2))
hist(df$insulin, breaks=50, ylim=c(0,400))
hist(complete(imp)$insulin, breaks=50, ylim=c(0,400))
par(mfrow=c(1,1))


imp <- mice(df, method = "norm.nob", m = 1, maxit = 1)		# use predicted values + noise
fit <- with(imp, lm(pressure ~age+insulin) )
summary(fit)  

#Prova un pò di modi per fare le imputazioni ma quello top è sto qua sotto 'norm'. GUARDA IL PVALUE PER VALUTARE LA BONTà del risultato

imp <- mice(df, method = "norm", m = 1, maxit = 1)		# use predicted values + noise + uncertainty
fit <- with(imp, lm(pressure ~age+insulin) )
summary(fit)


#Ok finora abbiamo solo fatto un'imputazione per vedere nattimo come funziona mice ora proviamo con m=30
imp <- mice(df, method = "norm.predict", m = 30, maxit = 1)		# use predicted values
fit <- with(imp, lm(pressure ~age+insulin) )
summary(pool(fit))#pool mette assieme i risultati 


imp <- mice(df, method = "norm", m = 30, maxit = 1)		# use predicted values + noise + uncertainty
fit <- with(imp, lm(pressure ~age+insulin) )
summary(pool(fit))


imp <- mice(df, method = "norm.boot", m = 1, maxit = 1) # use predicted values + noise + uncertainty
fit <- with(imp, lm(pressure ~age+insulin) )
summary(fit)

imp <- mice(df, method = "cart", m = 1, maxit = 1) # use tree
fit <- with(imp, lm(pressure ~age+insulin) )
summary(pool(fit))

imp <- mice(df, method = "rf", m = 1, maxit = 1) # use RF
fit <- with(imp, lm(pressure ~age+insulin) )
summary(fit)

set.seed(123)
imp <- mice(df, method = "pmm", m=30, maxit=10, print=F)		# use "similar" observed values. con m = 30 vuol dire che fa 30 imputazioni del mio dataset
imp$data # original data
imp$imp # imputated data
plot(imp)
c3 <- complete(imp, 3) #qua si becca le imputazioni del terzo dataset su 30
md.pattern(c3)
c.long <- complete(imp, "long")  
head(c.long)
tail(c.long)
c.broad <- complete(imp, "broad")
head(c.broad)#mette a fianco le imputazioni dei 30 dataset così le confronti

fit <- with(imp, lm(pressure ~age+insulin) )
fit
summary(fit$analyses[[30]])#così puoi beccarti il 30 modello
summary(fit$analyses[[3]])

pool.fit <- pool(fit)
summary(pool.fit)