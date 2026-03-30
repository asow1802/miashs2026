## Exercice 2
mon_acf <- function(Xt, lag.max = NULL) {
  T <- length(Xt)
  if (is.null(lag.max)) lag.max <- floor(10 * log10(T))
  Xt <- as.vector(Xt)
  mean_Xt <- mean(Xt)
  var_Xt <- sum((Xt - mean_Xt)^2) / T   # variance non biaisée (n)
  acf_vals <- numeric(lag.max + 1)
  acf_vals[1] <- 1
  for (h in 1:lag.max) {
    num <- sum((Xt[1:(T - h)] - mean_Xt) * (Xt[(h + 1):T] - mean_Xt))
    acf_vals[h + 1] <- num / (T * var_Xt)   # division par T (coeff. biaisé)
  }
  return(acf_vals)
}

# Test avec une série simulée
set.seed(123)
x <- arima.sim(n = 100, model = list(ar = 0.5))

# Calcul manuel
acf_manuel <- mon_acf(x, lag.max = 10)

# Calcul avec la fonction acf
acf_r <- acf(x, lag.max = 10, plot = FALSE)$acf[, , 1]

# Comparaison
cbind(manuel = acf_manuel, R = acf_r)

## Exercice 3

# des packages utiles
if (!require(weakARMA)) install.packages("weakARMA")
library(weakARMA)

# 1. Simulation d’un AR(1) avec phi=0.8
set.seed(123)
ar1_weak <- sim.ARMA(n = 200, phi = 0.8, theta = 0)   # weakARMA
ar1_base <- arima.sim(n = 200, model = list(ar = 0.8)) # arima.sim


# 2. Simulation d’un MA(2) theta=(0.5, 0.3)
ma2_weak <- sim.ARMA(n = 200, phi = 0, theta = c(0.5, 0.3))
ma2_base <- arima.sim(n = 200, model = list(ma = c(0.5, 0.3)))


# 3. Simulation d’un ARMA(1,1) phi=0.6, theta=-0.4
arma11_weak <- sim.ARMA(n = 200, phi = 0.6, theta = -0.4)
arma11_base <- arima.sim(n = 200, model = list(ar = 0.6, ma = -0.4))


# 4. Simulation d’un ARIMA(1,1,0) (intégré)
arima110 <- arima.sim(n = 200, model = list(ar = 0.5, order = c(1,1,0)))

## Les graphiques des 7 séries
pdf("simulations.pdf", width = 8, height = 6)
par(mfrow = c(2,4))
plot(ar1_weak, main = "AR(1) weakARMA", ylab = "x_t", type = "l")
plot(ar1_base, main = "AR(1) arima.sim", ylab = "x_t", type = "l")
plot(ma2_weak, main = "MA(2) weakARMA", ylab = "x_t", type = "l")
plot(ma2_base, main = "MA(2) arima.sim", ylab = "x_t", type = "l")
plot(arma11_weak, main = "ARMA(1,1) weakARMA", ylab = "x_t", type = "l")
plot(arma11_base, main = "ARMA(1,1) arima.sim", ylab = "x_t", type = "l")
plot(arima110, main = "ARIMA(1,1,0) arima.sim", ylab = "x_t" , type = "l")
dev.off()

## Les auto-corrélations et auto-corrélations partielles
pdf("acf_pacf.pdf", width = 8, height = 6)
par(mfrow = c(2,4))
acf(ar1_base, main = "AR(1) ACF")
pacf(ar1_base, main = "AR(1) PACF")
acf(ma2_base, main = "MA(2) ACF")
pacf(ma2_base, main = "MA(2) PACF")
acf(arma11_base, main = "ARMA(1,1) ACF")
pacf(arma11_base, main = "ARMA(1,1) PACF")
acf(arima110, main = "ARIMA(1,1,0) ACF")
pacf(arima110, main = "ARIMA(1,1,0) PACF")
dev.off()


## Interprétation des graphiques
'"
AR(1)
ACF : décroissance exponentielle (lente) vers 0
PACF : une seule barre significative au lag 1, les suivantes non significatives.
Processus autorégressif d ordre 1 (p = 1)

MA(2) : theta = c(0.5, 0.3)
ACF : coupure nette après le lag 2 (seuls phi1 et phi2 significatifs).
PACF : décroissance progressive sans coupure.
Processus moyenne mobile d ordre 2 (q = 2)

ARMA(1,1) : phi = 0.6, theta = -0.4
ACF : décroissance exponentielle, pas de coupure.
PACF : décroissance exponentielle, pas de coupure
Mélange AR et MA : ni l ACF ni la PACF ne s annulent brutalement

ARIMA(1,1,0) : processus intégré (racine unitaire)
ARIMA(1,1,0)
Série brute :
- ACF : décroît très lentement (persistance), signe de non-stationnarité.
- PACF : premier coefficient proche de 1, les suivants restent parfois significatifs.
Série différenciée :
- ACF : devient exponentiellement décroissante.
- PACF : se coupe après le lag 1 (comportement AR(1) stationnaire
Présence d une racine unitaire (d = 1).
'"