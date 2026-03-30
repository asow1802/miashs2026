# Simulation d'un AR(2)
set.seed(123)  # pour reproductibilité
n <- 500
phi1 <- 0.8
phi2 <- -0.3
eps <- rnorm(n)
X <- numeric(n)
X[1:2] <- 0  # initialisation

for (t in 3:n) {
  X[t] <- phi1 * X[t-1] + phi2 * X[t-2] + eps[t]
}

# Tracé de la série
plot(X, type = "l", main = "Simulation AR(2)", xlab = "Temps", ylab = "X(t)")

# Tracé de la PACF
pacf(X, lag.max = 20, main = "PACF du AR(2) simulé")

# Calcul des PACF (retards 1 à 20)
pacf_vals <- pacf(X, lag.max = 20, plot = FALSE)
print(pacf_vals$acf)  # valeurs
print(pacf_vals$lag)  # retards correspondants