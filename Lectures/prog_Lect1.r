rm(list = ls())
# Mac
# setwd("~/Lect1/Data")
# Windows
# setwd("C:/Lect1/Data")

################################################################################
SP500 <- read.table("DSP500.csv", header=T, sep=",")

attach(SP500)
summary(SP500)
names(SP500)

DSP_time <- strptime(SP500$Date, format="%Y/%m/%d")
DSP <- SP500$Close

plot(DSP_time, DSP, type="l", xlab="Date", main="Daily S&P500 index from Jan 1960 to Aug 2023")

# compute log returns
DSPLR <- diff(log(DSP))

plot(DSP_time[-1], DSPLR, type = "l", xlab = "Date",
     main = "Daily log return of S&P500 from Jan 1960 to Aug 2023")

par(mfrow=c(1, 2))  # divide the plotting area into 1 by 2 to facilitate comparison 
plot(DSP_time, DSP, type="l", xlab="Date", main="Daily S&P500 index from Jan 1960 to Aug 2023")
plot(DSP_time[-1], DSPLR, type="l", xlab="Date", main="Daily log return of S&P500 from Jan 1960 to Aug 2023")

par(mfrow=c(1, 1))
qqnorm(DSPLR, main="Normal QQ plot of DSLPR")

################################################################################
# nonstationary vs stationary 

subsample <- c(13991:14995)
DSP_s <- DSP[subsample]
DSPLR_s <- diff(log(DSP_s))
DSP_shuffle <- sample(DSP_s)
DSPLR_shuffle <- sample(DSPLR_s)

par(mfrow=c(1, 2))
plot(DSP_s, type="l")
plot(DSP_shuffle, type="l", main="Daily S&P500 index shuffled")

plot(DSPLR_s, type="l")
plot(DSPLR_shuffle, type="l", xlab="Date", main="shuffled daily log returns", cex.main=1.5)

################################################################################
# common distributions
# normal
x <- seq(-4, 4, by=0.01)

par(mfrow=c(1, 1))
plot(x, pnorm(x), type="l", lwd=2)

plot(x, dnorm(x), type="l", lwd=2)

x <- seq(-5, 5, by=0.01)
plot(x, dnorm(x), type="l", lwd=3)
lines(x, dnorm(x, mean=2), lty=2, lwd=3, col="red")
lines(x, dnorm(x, mean=0, sd=2), lty=5, lwd=3, col="blue")
legend("topright", c("N(0,1)", "N(2,1)", "N(0,4)"), lty=c(1, 2, 5), lwd=c(3, 3, 3),
       col=c("black", "red", "blue"), text.col=c("black", "red", "blue"))

# Student's t 
## role of m and lambda
x <- seq(-5, 5, by=0.01)
plot(x, dt(x, 4), type="l", lwd=3)
lines(x, dt(x - 2, 4), lty=2, lwd=3, col="red")
lines(x, dt(x / 2, 4) / 2, lty=5, lwd=3, col="blue")
legend("topright", c("t(0,1,4)", "t(2,1,4)", "t(0,2,4)"), lty=c(1, 2, 5), lwd=c(3, 3, 3),
       col=c("black", "red", "blue"), text.col=c("black", "red", "blue"))

## role of nu
x <- seq(-5, 5, by=0.01)
plot(x, dt(x, 2), type="l", lwd=3, ylim=c(0, 0.4))
lines(x, dt(x, 5), lty=2, lwd=3, col="red")
lines(x, dnorm(x), lty=5, lwd=3, col="blue")
legend("topright", c("t(0,1,2)", "t(0,1,5)", "N(0,1)"), lty=c(1, 2, 5), lwd=c(3, 3, 3),
       col=c("black", "red", "blue"), text.col=c("black", "red", "blue"))

################################################################################
# histogram
# try help(hist) to learn its options
par(mfrow=c(2, 2))
hist(DSPLR, breaks=20, freq=F, col=NULL, main="Histogram of DSPLR, #bins = 20")   
hist(DSPLR, breaks=100, freq=F, col=NULL, main="Histogram of DSPLR, #bins = 100")
hist(DSPLR, breaks=500, freq=F, col=NULL, main="Histogram of DSPLR, #bins = 500")
hist(DSPLR, breaks=5000, freq=F, col=NULL, main="Histogram of DSPLR, #bins = 5000")

# histogram vs fitted normal
par(mfrow=c(1, 1))
hist(DSPLR, breaks=100, freq=F, col=NULL, main="Histogram of DSPLR vs Fitted Normal Density")

mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)

curve(dnorm(x, mean=mu_DSPLR, sd=sd_DSPLR), n=1001, col="red", add=TRUE)

# histogram vs fitted t
par(mfrow=c(1, 1))
hist(DSPLR, breaks=100, freq=F, col=NULL, main="Histogram of DSPLR vs Fitted t Density", ylim=c(0, 60))
library(MASS)
DSPLR_t_fit <- fitdistr(DSPLR, "t")
m <- DSPLR_t_fit$estimate[1]
lambda <- DSPLR_t_fit$estimate[2]
nu <- DSPLR_t_fit$estimate[3]
curve(dt((x - m) / lambda, df=nu) / lambda, n=1001, col="red", add=TRUE)

################################################################################
# kernel density (Optional)
# KDE illustration
n <- 5
X <- rnorm(n)
y0 <- rep(0, n)

bw <- 0.3

par(mfrow=c(2, 2))
plot(X, y0, xlim=c(-3, 3), ylim=c(0, 1.3), ylab="", main=paste(bquote(.(n)), "Observations"))

plot(X, y0, xlim=c(-3, 3), ylim=c(0, 1.3), ylab="", main=paste(bquote(.(n)), "Observations & the 5 Kernels"))

x <- seq(-3, 3, by=0.01)

for (i in 1:5)
{
  KD <- dnorm(x, X[i], bw)
  points(x, KD, type="l", col=i)
}

plot(X, y0, xlim=c(-3, 3), ylim=c(0, 0.6), ylab="", main=paste(bquote(.(n)), "Observations & the Average of 5 Kernels"))

KD <- rep(0, 601)

for (i in 1:5)
{
  KD <- KD + dnorm(x, X[i], bw)
}
KD <- KD / 5
points(x, KD, type="l", col="red")

n <- 1000
X <- rnorm(n)
y0 <- rep(0, n)

plot(X, y0, xlim=c(-3, 3), ylim=c(0, 0.6), ylab="", main=paste(bquote(.(n)), "Observations & the Average of All Kernels"))

KD <- rep(0, 601)

for (i in 1:n)
{
  KD <- KD + dnorm(x, X[i], bw)
}
KD <- KD / n
points(x, KD, type="l", col="red")

# superimpose the N(0,1) density
y1 <- dnorm(x)
points(x, y1, type="l", col="black")

################################################################################
# SP500
# different kernel
par(mfrow=c(2, 2))
KD5 <- density(DSPLR, kernel="gaussian", bw=.005)
plot(KD5, type="l", ylim=c(0, 55), main="KDE of DSPLR with Gaussian Kernel")
KD6 <- density(DSPLR, kernel="rectangular", bw=.005)
plot(KD6, type="l", ylim=c(0, 55), main="KDE of DSPLR with rectangular Kernel")
KD7 <- density(DSPLR, kernel="triangular", bw=.005)
plot(KD7, type="l", ylim=c(0, 55), main="KDE of DSPLR with triangular Kernel")
KD8 <- density(DSPLR, kernel="cosine", bw=.005)
plot(KD8, type="l", ylim=c(0, 55), main="KDE of DSPLR with cosine Kernel")

# different bandwidth
par(mfrow=c(2, 2))
KD1 <- density(DSPLR, kernel="gaussian", bw=.01)
plot(KD1, type="l", ylim=c(0, 65), main="KDE of DSPLR with BW=.01")
KD2 <- density(DSPLR, kernel="gaussian", bw=.005)
plot(KD2, type="l", ylim=c(0, 65), main="KDE of DSPLR with BW=.005")
KD3 <- density(DSPLR, kernel="gaussian", bw=.001)
plot(KD3, type="l", ylim=c(0, 65), main="KDE of DSPLR with BW=.001")
KD4 <- density(DSPLR, kernel="gaussian", bw=.00001)
plot(KD4, type="l", main="KDE of DSPLR with BW=.00001")

# histogram and Kernel
par(mfrow=c(2, 1))
hist(DSPLR, breaks=100, freq=F, col=NULL, main="Histogram & KDE of DSPLR, #bin = 100, bw=0.01", ylim=c(0, 65))
KD4 <- density(DSPLR, kernel="gaussian", bw=.01)
points(KD4, type="l", col="red")

KD9 <- density(DSPLR, kernel="gaussian", bw=.001)
hist(DSPLR, breaks=100, freq=F, col=NULL, main="Histogram & KDE of DSPLR, #bin = 100, bw=0.001", ylim=c(0, 65))
points(KD9, type="l", col="red")

# default choice of bw
par(mfrow=c(1, 1))
KD0 <- density(DSPLR, kernel="gaussian")
hist(DSPLR, breaks=100, freq=F, col=NULL, main="Histogram & KDE of DSPLR, #bin = 100, bw=default", ylim=c(0, 65))
points(KD0, type="l", col="red")

KD0
names(KD0)
KD0$bw



################################################################################
# Extra Simulations

# Cauchy(0,1) vs N(0,1)
N<- length(DSPLR)# = length(DSPLR)        
N
GWN <- rnorm(N)
CWN <- rcauchy(N)
#Comparison Between Cauchy and Normal

par(mfrow=c(2,1))
ts.plot(GWN,main=paste("Plot of ", bquote(.(N)), " N(0,1)"),xlab="index",ylab="")
ts.plot(CWN,main=paste("Plot of ", bquote(.(N)), " C(0,1)"),xlab="index",ylab="")


tWN<-rt(N,nu)
ts.plot(tWN,main=paste("Plot of ", bquote(.(N)), " t(nu)"),xlab="index",ylab="")
ts.plot(DSPLR)