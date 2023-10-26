rm(list = ls())
library(moments)

# setwd(~/)
setwd("d:/workspace/fina5250")
SP500 <- read.table("DSP500_5Y.csv",header = T, sep=",")

summary(SP500)
names(SP500)

DSP=SP500$Close

DSPLR <- diff(log(DSP))  # compute log returns; 

#Stylized Fact 1

par(mfrow=c(2,1))

z <- acf(DSPLR,main="Autocorrelations of DSPLR from Jan 1997 to Dec 2001")

plot(
  z$acf[2:20],
  type="h",
  main="Autocorrelations of DSPLR from Jan 1997 to Dec 2001",
  xlab="Lag",
  ylab="ACF",
  ylim=c(-0.2,0.2), # this sets the y scale to -0.2 to 0.2
)

abline(h=0)

n<-length(DSPLR)
abline(h=-2*1/sqrt(n),lty=2)
abline(h=2*1/sqrt(n),lty=2)

n=length(DSPLR)

head(cbind(DSPLR,DSPLR))
head(cbind(DSPLR[-c(n)],DSPLR[-c(1)]))
head(cbind(DSPLR[-c(n-1,n)],DSPLR[-c(1,2)]))

z[0]

z[1]
round(cor(DSPLR[-n],DSPLR[-1]),3)

z[2]
round(cor(DSPLR[-c(n-1,n)],DSPLR[-c(1,2)]),3)

z[5]
round(cor(DSPLR[-c((n-4):n)],DSPLR[-c(1:5)]),3)
head(cbind(DSPLR[-c((n-4):n)],DSPLR[-c(1:5)]),10)


#Stylized Fact 2
par(mfrow=c(1,1)) 
hist(DSPLR,breaks=50,  freq = F,main="Histogram of DSPLR vs Fitted Normal Density")   

mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)

x<-seq(-0.2,0.1,by=0.001)
y<-dnorm(x,mean=mu_DSPLR,sd = sd_DSPLR)
points(x,y,type="l",col="red")

qqnorm(DSPLR )
qqline(DSPLR,col="red")

#Stylized Fact 3
c(mean(DSPLR),sd(DSPLR),skewness(DSPLR),kurtosis(DSPLR))
kurtosis(rnorm(10000))

#Stylized Fact 4
t=(0.0004-0)/(0.0127/sqrt(n))
t

#Stylized Fact 5
z<-acf(DSPLR^2)
 
 

plot(z$acf[2:20], 
     type="h", 
     main="Autocorrelations of Squared DSPLR from Jan 1997 to Dec 2001", 
     xlab="Lag",     
     ylab="ACF", 
     ylim=c(-0.2,0.2), # this sets the y scale to -0.2 to 0.2
     )
abline(h=0)

n<-length(DSPLR)
abline(h=-2*1/sqrt(n),lty=2)
abline(h=2*1/sqrt(n),lty=2)

 

#Stylized Fact 6 figure
 
ts.plot(DSPLR, main="SP500 Log Returns from Jan 1997 to Dec 2001")

#################White Noise################
WN<-rnorm(1024, mean = 0, sd = 2)
ts.plot(WN)

par(mfrow=c(2,1))
acf(WN,40,"covariance",main="Auto-Covariance Function") 
acf(WN,40,"correlation",main="Auto-Correlation Function")

par(mfrow=c(3,1))
acf(WN,5,"correlation",main="Auto-Correlation Function for WN")
acf(WN,40,"correlation",main="Auto-Correlation Function for WN")
acf(WN^2,40,"correlation",main="Auto-Correlation Function for WN squared")

#################Random Walk, Supplementary################

par(mfrow=c(1,1))
ts.plot(SP500$Close)

par(mfrow=c(2,1))

WN<-rnorm(1024, mean = 0, sd = 1)
ts.plot(cumsum(WN),main="RW")
mu<-0.1
ts.plot(cumsum(mu+WN),main="RW With Drift")


#####################AR(1), Supplementary######################
# AR(1)
par(mfrow=c(1,2))
#par(mar=c(2.5,2.5,2.5,2.5))
tsar.sim1<-arima.sim(n=200,list(order=c(1,0,0),ar=.8))
ts.plot(tsar.sim1)
acf(tsar.sim1)

par(mfrow=c(1,2))
tsar.sim2<-arima.sim(n=200,list(order=c(1,0,0),ar=-.8))
ts.plot(tsar.sim2)
acf(tsar.sim2)

######################MA(1), Supplementary######################
# MA(1)
par(mfrow=c(1,2))
tsma.sim1<-arima.sim(n=200,list(order=c(0,0,1),ma=0.8),sd=1)
ts.plot(tsma.sim1)
acf(tsma.sim1)

par(mfrow=c(1,2))
tsma.sim2<-arima.sim(n=200,list(order=c(0,0,1),ma=-0.8),sd=1)
ts.plot(tsma.sim2)
acf(tsma.sim2)


