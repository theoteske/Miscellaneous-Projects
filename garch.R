rm(list=ls(all=TRUE))

library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library("TTR")
library(tis)
require("datasets")
require(graphics)
library("forecast")
#install.packages("astsa")
#require(astsa)
library(nlstools)
library(fpp)
library(strucchange)
library(Quandl)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(stockPortfolio)
library(vars)
library(lmtest)
library(dlnm)
library(hts)
library(tseries)
library(rugarch)

#specify an ARMA + GARCH Model
model=ugarchspec(
variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
distribution.model = "sstd")

#check the model parameters
model@model$pars

# get SP500 data (get prices, convert them to returns, then to data frame)
sp500.prices=get.hist.quote(
instrument = "^GSPC",
quote = "Adj",
provider = c("yahoo"), method = NULL,
origin = "1899-12-30", compression = "d",
retclass = c("zoo"), quiet = FALSE, drop = FALSE)

sp500=as.data.frame(sp500.prices)
N=length(sp500[,1])
sp500.returns=100*(log(sp500[2:N,])-log(sp500[1:(N-1),]))

plot(sp500.prices)

#fit the model to the data
modelfit=ugarchfit(spec=model,data=sp500.returns)
modelfit
plot(modelfit)
#note: you can pass the "which=" argument to the plot() function with the number of the graph.

#forecast
modelfor = ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0)
plot(modelfor)