sncf=read.table("http://freakonometrics.free.fr/sncf.csv",header=TRUE,sep=";")
train=as.vector(t(as.matrix(sncf[,2:13])))
ts_train=ts(train,start = c(1963, 1), frequency = 12)

View(sncf)
par(mfrow = c(2,2))
plot(ts_train)

## 1 Analyse

# Visuellement, nous remarquons une tendance croissance des données, aussi la moyenne
# n'est pas cste => la série n'est pas stationnaire.

## 2 les auto corrélogrammes

acf(ts_train, main = "ACF de ts_train")
pacf(ts_train, main = "PACF de ts_train")

## 3 différence première

Zt = diff(train)
plot(Zt, type = "l")
acf(Zt, main = "ACF de ts_train")
pacf(Zt, main = "PACF de ts_train")

