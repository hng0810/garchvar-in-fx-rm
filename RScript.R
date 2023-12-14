rm(list = ls())
library(zoo)
library(ggplot2)
library(tseries)
library(FinTS)
library(rugarch)
setwd("C:/Users/nqhtn/Desktop")
df <- read.csv("GBP_VND.csv")

plot(df$price, type = "l")
plot(df$return, type = "l")

jarque.bera.test(na.omit(df$return))
summary(df$return)
r <- na.omit(df$return[1:1000])
r.fit <- na.omit(df$return)
r.backtest <- df$return[1000:1250]

fitdist(distribution = 'std' , x = r)$pars
adf.test(r)
ArchTest(r, lags = 1, demean = T)

rvol2 <- r^2
par(mfrow = c(2, 1))
plot(rvol2, type = "l")
pacf(rvol2)

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , 
                                              garchOrder = c(1 , 1)),
                        mean.model = list(armaOrder = c(0 , 0)))
model.fit = ugarchfit(spec = model.spec , 
                      data = r.fit , 
                      solver = 'solnp',
                      out.sample=250)
options(scipen = 999)
model.fit@fit$matcoef

model.roll = ugarchroll(spec = model.spec , 
                        data = r.fit , 
                        n.start = 999, 
                        refit.every = 1,
                        refit.window = 'moving')
VaR95_td = model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=5.416428, p=0.05)
sum(r.backtest[2:251] < VaR95_td)
plot(r.backtest, type = "l")
lines(VaR95_td, col = "red")
varf <- mean(VaR95_td)*sqrt(250)
350000*(-varf)