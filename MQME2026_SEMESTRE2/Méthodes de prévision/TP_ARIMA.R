x = rnorm(1000)
plot(x, type = "l")

res = vector()
logAP = log(AP)

library(tseries)

for (i in 1:40) {
  res[i] = adf.test(x, k = i, alternative="stationary")$p.value
}
res
