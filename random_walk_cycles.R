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

#random ralk simulation, Zt ~ N(0,1)

#Xt = Xt-1 + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = x[t-1] + e[t]
quartz()
plot(x, type="l")
acf(x)
acf(diff(x))

#Xt =  Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = e[t]
quartz()
plot(x, type="l")
acf(x)
acf(diff(x))

#Xt = -0.3Xt-1 + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = -0.3*x[t-1] + e[t]
quartz()
plot(x, type="l")
acf(x)
acf(diff(x))

#Xt = sin(pi t/3) + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = sin(pi*t/3)+ e[t] -x[t-1]
par(mfrow=c(3,1))
plot(x,type='l',main="Original Time Series of the Data")
acf(x,main="ACF")
acf(diff(x),main="ACF (First Difference)",ylab=expression(ACF(Delta)))

#plot autocorrelation, autocovariance, and partial autocorrelation functions
quartz()
par(mfrow=c(2,2))
plot(x,type='l')
acf(x, type = "correlation")
acf(x, type = "covariance")
acf(x, type = "partial")

#plot autocorrlation function of the first and second difference
quartz()
par(mfrow=c(2,1))
acf(diff(x))
acf(diff(diff(x)))



