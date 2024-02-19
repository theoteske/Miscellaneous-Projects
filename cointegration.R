library("tseries")
library("urca")
library(AER)

data("PepperPrice")
plot(PepperPrice, plot.type = "single", col = 1:2)


data("PepperPrice")
plot(PepperPrice, plot.type = "single", col = 1:2)
legend("topleft", c("black", "white"), bty = "n",col = 1:2, lty = rep(1,2))

#unit root tests
adf.test(log(PepperPrice[, "white"]))

pp.test(log(PepperPrice[, "white"]), type = "Z(t_alpha)")
pepper_ers <- ur.ers(log(PepperPrice[, "white"]),
  type = "DF-GLS", model = "const", lag.max = 4)
summary(pepper_ers)

#stationarity tests
kpss.test(log(PepperPrice[, "white"]))
#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for the null hypothesis that x is level or trend stationary

#cointegration
po.test(log(PepperPrice))
pepper_jo <- ca.jo(log(PepperPrice), ecdet = "const", type = "trace")
summary(pepper_jo)
pepper_jo2 <- ca.jo(log(PepperPrice), ecdet = "const", type = "eigen")
summary(pepper_jo2)