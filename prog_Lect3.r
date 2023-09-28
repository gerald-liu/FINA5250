# remove all objects in the workspace
rm(list = ls())

# setwd("~/Lect3/Data")
setwd("D:\\workspace\\FINA5250")

BHData <- read.table("MBH_MSP_2009_2023.csv", sep = ",", header = TRUE)
# attach(BHData)

SP <- BHData$SP
BH <- BHData$BH

head(BHData,5)
tail(BHData,5) 

plot(SP,BH, main="BH price VS SP500 index")


cov(SP,BH)
cor(SP,BH)
var(SP)
var(BH)
cov(SP,BH)/sqrt(var(SP)*var(BH))

# LS regression
BHSPl2 <- lsfit(SP,BH)
BHSPl2$coef

plot(SP,BH)
abline(BHSPl2)

lm_1 <- lm(BH ~ SP)
summary(lm_1)
coef(lm_1)

predict(lm_1, data.frame(SP = 1800))

predict(lm_1, data.frame(SP = 5000))

#predictions
NEWSPi<-1800 #interpolation
predi<-BHSPl2$coef[1]+BHSPl2$coef[2]*NEWSPi
predi

NEWSPe<-5000 # extrapolation (more risky!)
prede<-BHSPl2$coef[1]+BHSPl2$coef[2]*NEWSPe
prede

# diagonostics

par(mfrow = c(2, 1))
plot(BHSPl2$residuals, type = "l",
     main = "Residual Plot of LS Regression of BH Against SP500 ")
# to see positive association btw adjacent residuals
n <- length(BH)
plot(BHSPl2$residuals[-n], # remove last item
     BHSPl2$residuals[-1], # remove 1st item
     main = "Residual against previous residual")

cor(BHSPl2$residuals[-n],BHSPl2$residuals[-1])

res_acf <- acf(BHSPl2$residuals, type = "correlation",plot = TRUE)

res_acf

qqnorm(BHSPl2$residuals)

# returns
# monthly data, discrete: don't use log returns
rBH<-diff(BH)/BH[-n]
rSP<-diff(SP)/SP[-n]

par(mfrow=c(2,1))
plot(SP,BH, main="BH price VS SP500 index")
plot(rSP, rBH, main="Returns on BH vs returns on SP500")

# ls fit

par(mfrow=c(1,1))
plot(rSP, rBH, main="Returns on BH vs returns on SP500")
rBHSPl2 <- lsfit(rSP,rBH)
abline(rBHSPl2)

par(mfrow=c(2,1))
plot(rBHSPl2$residuals,type="l",main="Residual Plot of LS Regression of rBH Against rSP")
###  association btw adjacent residuals
n <- length(BH) - 1
#n <- length(rBH)
plot(rBHSPl2$residuals[-n],rBHSPl2$residuals[-1],main="Residual against previous residual ")

cor(rBHSPl2$residuals[-n],rBHSPl2$residuals[-1])

acf(rBHSPl2$residuals, type = "correlation", plot = TRUE)

qqnorm(rBHSPl2$residuals)


#one factor model from 200901-202307
rm(list = ls())
BH2009_2023 <- read.table("BH2009-2023.csv",sep=',',header=TRUE)
head(BH2009_2023)
attach(BH2009_2023)
head(BH2009_2023)
rBH_ex <- rBH-rf
Onefactor <- lm(rBH_ex ~ rM_ex)
summary(Onefactor)
qqnorm(Onefactor$residuals)
qqline(rBH_ex,col="red")


BH1980_2023 <- read.table("BH1980-2023.csv",sep=',',header=TRUE)
head(BH1980_2023)
attach(BH1980_2023)
head(BH1980_2023)
rBH_ex <- rBH-rf
Onefactor <- lm(rBH_ex ~ rM_ex)
summary(Onefactor)
qqnorm(Onefactor$residuals)
qqline(rBH_ex,col="red")
