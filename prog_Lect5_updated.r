rm(list = ls())
# setwd("~/Lect5/Data")
# setwd(“~/Data")
setwd("d:/workspace/fina5250")

# Input: CKH and HSBC
CKH  <- read.table("0001.HK.csv",sep=',',header=TRUE)
HSBC <- read.table("0005.HK.csv",sep=',',header=TRUE)
head(CKH)
head(HSBC)

DCKH<-CKH$Close
DCKHLR <- diff(log(DCKH))  

DHSBC<-HSBC$Close
DHSBCLR <- diff(log(DHSBC))

# Summary stats
c(mean(DCKHLR),var(DCKHLR))
c(mean(DHSBCLR),var(DHSBCLR))
c(cov(DCKHLR,DHSBCLR),cor(DCKHLR,DHSBCLR))

# Portfolio (40%CK 60%HSBC)
# ret.T @ w
P46LR<-0.4*DCKHLR+0.6*DHSBCLR
# based on empirical distribution
q <- 0.01 
VaR_emp <- -quantile(P46LR,q)
VaR_emp
# if based on normal
mu_P46LR <- mean(P46LR)
sd_P46LR <- sd(P46LR)
VaR_normal <- - qnorm(q,mu_P46LR, sd_P46LR)
# if based on normal and assuming mean zero
# prudent for daily returns, reducing model complexity.
VaR_normal_m0 <- - qnorm(q,0, sd_P46LR)
c(VaR_normal,VaR_normal_m0)

VaR_normal_m0 <- - qnorm(q) * sd_P46LR
c(VaR_normal,VaR_normal_m0)



# Portfolio weight from -0.5(1.5) to 1.5(-0.5)

data<-cbind(DCKHLR,DHSBCLR)

w1<-seq(-0.5,1.5,by=0.1) # np.linspace(-0.5, 1.6, 0.1)
w2<-(1-w1)
std<-rep(NA,length(w1)) # np.full((rows, cols), np.nan)

# for (i in 1:length(w1)){
for (i in seq_along(w1)) {
  port_weight <- c(w1[i], w2[i])
  # t() = transpose
  # %*% = matrix multiply
  # w.T @ cov @ w
  std[i] <- sqrt(t(port_weight) %*% cov(data) %*% port_weight)
}

plot(w1, std, ylim=c(0,0.02),main="Portfolio weights and standard deviation", 
     ylab="Standard deviation", xlab="Portfolio weights (CKH)")

w1[std==min(std)]

 

## Simulation, three assets

#Expected return
ER<-c(0.05, 0.07, 0.10)
#Standard deviation
SD<-c(0.15, 0.16, 0.20)
COV<-matrix(c(0.0225,0.012,0.009,0.012,0.0256,0.0064,0.009,0.0064,0.04),nrow=3,ncol=3)
 
Ptf<-matrix(data=NA,nrow=1000,ncol=3)
colnames(Ptf) <- c("Weight for 1","Weight for 2", "Weight for 3")
ERp<-rep(NA,1000)
SDp<-rep(NA,1000)

Ptf[1,1:3]<-c(1,0,0) # Ptf[i,] = Portfolio i's weights
Ptf[2,1:3]<-c(0,1,0)
Ptf[3,1:3]<-c(0,0,1)
Ptf[4,1:3]<-c(0.33,0.33,0.33)
for (i in 1:4){
  ERp[i]=Ptf[i,]%*%ER # expected returns of portfolios i=1,2,3,4
  SDp[i]=sqrt(t(Ptf[i,]) %*%COV%*% Ptf[i,] )# SDs of portfolios i=1,2,3,4
}

plot(SDp[1:4], ERp[1:4],xlim=c(0,0.3),ylim=c(0.03,0.15), main="Portfolio returns and standard deviation", 
     xlab="Standard deviation", ylab="Expected returns", col="blue",pch = 16, cex = 1.5)

for (i in 5:1000){
  Ptf[i,1]=runif(n = 1, min = -1, max = 1) # generate 1 random number from a uniform distribution (-1, 1)
  Ptf[i,2]=runif(1,-1,1) 
  Ptf[i,3]=1-Ptf[i,1]-Ptf[i,2]
  ERp[i]=Ptf[i,]%*%ER
  SDp[i]=sqrt(t(Ptf[i,]) %*%COV%*% Ptf[i,])
}

plot(SDp,ERp,xlim=c(0,0.6),ylim=c(0,0.2), main="Portfolio returns and standard deviation", 
     xlab="Standard deviation", ylab="Expected returns")
points(SDp[1:4], ERp[1:4], col="blue" ,pch = 16, cex = 1.5)


inv_var_sum <- 0

for (i in seq(1, 3)) {
  inv_var <- 1 / COV[i, i]
  print(inv_var)
  inv_var_sum <- inv_var_sum + inv_var
}

inv_var_sum

for (i in seq(1, 3)) {
  inv_var <- 1 / COV[i, i]
  Ptf[5, i] <- inv_var / inv_var_sum
}

Ptf[5, ]

ERp[5]=Ptf[5,]%*%ER
SDp[5]=sqrt(t(Ptf[5,]) %*%COV%*% Ptf[5,] )

ERp[5]
ERp[4]

SDp[5]
SDp[4]

ERp[5]/SDp[5]
ERp[4]/SDp[4]

sd_mkt <- 0.02
beta_a <- 0.5
tau_a <- 0.001

var_a <- (beta_a * sd_mkt)^2 + tau_a^2
var_a

beta_b <- 1.5
tau_b <- 0.0015

var_b <- (beta_b * sd_mkt)^2 + tau_b^2
var_b

cov_ab <- beta_a * beta_b * sd_mkt^2
cov_ab

# byrow = FALSE
cov_ab_m <- matrix(c(var_a, cov_ab, cov_ab, var_b), nrow = 2, ncol = 2)

w <- c(0.3, 0.7)

var_p <- t(w) %*% cov_ab_m %*% w
var_p

cov_e_m <- matrix(c(tau_a^2, 0, 0, tau_b^2), nrow = 2, ncol = 2)
var_e <- t(w) %*% cov_e_m %*% w
var_e

tau_a^2 * w[1]^2 + tau_b^2 * w[2]^2

alpha <- c(-0.000002, 0.00001)
alpha_p <- t(alpha) %*% w
alpha_p

beta <- c(beta_a, beta_b)
beta_p <- t(beta) %*% w
beta_p
