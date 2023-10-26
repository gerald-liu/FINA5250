rm(list = ls())
# setwd("~")
setwd("d:/workspace/fina5250")


## fit GARCH

# install.packages("fGarch")
# install.packages("fBasics")
library(moments)
library(fGarch)

set.seed(5)

spec <- garchSpec(model = list(omega = 0.01, alpha = 0.1, beta = 0.8))
garch11 <- garchSim(spec, n = 1000)$garch
ts.plot(garch11,main="GARCH(1,1) with Coef (0.01, 0.1, 0.8)")

par(mfrow=c(1,2))
acf(garch11, main = "ACF Plof of A GARCH(1,1)")
acf((garch11)^2, main = "ACF Plof of A GARCH(1,1)^2")


SP500 <- read.table("DSP500_5Y.csv",header = T, sep=",")

DSP=SP500$Close

DSP_time <- strptime(SP500$Date, format="%Y-%m-%d")


DSP_LR <- diff(log(DSP))  # compute log returns; 
                         # try help(diff) for the usage of diff

par(mfrow=c(2,1))
ts.plot(DSP,main="SP500 from Jan 1997 to Dec 2001")
ts.plot(DSP_LR,main="SP500 Log-Returns from Jan 1997 to Dec 2001")

#Fit:Model Checking
DSP_LR_GARCH <- garchFit( ~ garch(1,1), data = DSP_LR, trace = FALSE, include.mean = FALSE)
DSP_LR_GARCH@fit$coef
summary(DSP_LR_GARCH)

 
# Forecasting

par(mfrow=c(1,1))
prediction<-predict(DSP_LR_GARCH, n.ahead = 10, trace = FALSE, plot=TRUE, nx=5)

DSP_01_02 <- c(1148.08, 1154.67,1165.27,1172.51,1164.89,1160.71,1155.14,1156.55,1145.60,1138.41,1146.19)
# SP500 on the last day of 2001 and first few days of 2002
DSP_LR_2002 <- diff(log(DSP_01_02))
lines(seq(6,15),DSP_LR_2002,lty=2)

prediction$standardDeviation

prediction <- predict(DSP_LR_GARCH, n.ahead = 100, trace = FALSE, plot=TRUE, nx=5)
# lines(seq(6,15),DSP_LR_2002,lty=2)

prediction$standardDeviation[100]^2
omega <- DSP_LR_GARCH@fit$coef[1]
alpha <- DSP_LR_GARCH@fit$coef[2]
beta <- DSP_LR_GARCH@fit$coef[3]
longrunVar <- omega /(1-alpha-beta)

longrunVar

plot(prediction$standardDeviation^2)



n <- length(DSP_LR)
variance_forecast_1 <- as.numeric(omega + alpha* DSP_LR[n]^2+ beta*DSP_LR_GARCH@sigma.t[n]^2)

# 1%-VaR under normal

prediction <- predict(DSP_LR_GARCH, n.ahead = 1, trace = FALSE, mse = "cond", plot=TRUE, nx=5)
sd_forecast <- prediction$standardDeviation

# sd_forecast == sqrt(variance_forecast_1)

VaR_normal <- -sd_forecast*qnorm(0.01)

VaR_normal

# pnorm(0.01, sd = sd_forecast)

# 1%-ES under normal

N<-100000
X<-rnorm(N,0,sd_forecast)
ES_normal <- mean( - X[- X > VaR_normal])

ES_normal


# Model Checking 
w<-DSP_LR/DSP_LR_GARCH@sigma.t

par(mfrow=c(2,1))
acf(w)
acf(w^2)

qqnorm(w)
qqline(w)
