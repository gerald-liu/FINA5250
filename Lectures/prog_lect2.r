rm(list = ls())

# setwd("~/Lect2/Data") 
# setwd(utils::getSrcDirectory(function() {}))
setwd("D:\\workspace\\FINA5250")

q<-c(.01,.025,.05,.1,.15,.25,.5,0.75,0.85,0.9,0.95,0.975,0.99) # c(): make a list
q_norm <- round(qnorm(q),3) # qnorm default mean = 0, sd = 1, round default digits = 0
q_cauchy <- round(qcauchy(q),3)
q_t <- round(qt(q,4),3)
rbind(q,q_norm,q_t,q_cauchy) # row bind; cf. cbind(): column bind; cf. pd.concat([s1, s2])

# QQ plots (Lect2 Scatter Plots of the Quantiles)
par(mfrow=c(2,1))
plot(q_cauchy,q_t,main="t(4) Quantiles vs Cauchy(0,1) Quantiles")
plot(q_norm,q_cauchy,main="Cauchy(0,1) Quantiles vs N(0,1) Quantiles")

# QQ plots (Lect2 Examples of Q-Q Plots)
q <- seq(0.001,0.999,by=0.001)
q_norm1 <- qnorm(q)
q_norm2 <- qnorm(q,3,6)
q_cauchy <- round(qcauchy(q),3)
par(mfrow=c(1,1))
plot(q_norm1,q_cauchy,main="QQ: Cauchy(0,1) vs N(0,1)",xlab="",ylab="")


# Heavy tail (Lect2 A Closer Look at the Tails)
par(mfrow=c(1,1))
x<-seq(3,6,by=0.01)
y_n <- dnorm(x)
y_c <- dcauchy(x)
# ylim: range of y-axis; lty: line type; lwd: line width
plot(x,y_c,ylim=c(0,0.032), type="l", main="Right Tails of N(0,1) and Cauchy(0,1)", ylab="Densities",lty=1,lwd=2)
lines(x,y_n,lty=4,col="red",lwd=2)
legend("topright", c("N(0,1)","Cauchy(0,1)"), text.col=c("red","black"),
       lty = c(4,1),lwd=c(2,2), col=c("red","black"))

# Heavy tail (Lect2 More Examples of Q-Q Plots) 
par(mfrow=c(3,1))
n <- length(q)
t_simu <- rt(n,4)
qqnorm(y = t_simu, main = "QQ: t(4) vs N(0,1)",
       xlab = "", ylab = "")
qqline(y = t_simu)
qqplot(x = q_norm1, y = t_simu)
plot(q_norm1,q_norm2,main="QQ: N(3,36) vs N(0,1)",xlab="",ylab="")
abline(3,6,col="red") # draw a straight line


# edf ( Lect2 EDF of N(0,1) with Different n's)
N <- c(10, 50, 100, 1000)
par(mfrow=c(1,1))
X <- rnorm(N[1])
sort(X)
edf_normal <- ecdf(X)
# bquote(): quote expression in .()
plot(edf_normal, xlim=c(-4,4), main = paste("EDF of N(0,1) with ", bquote(.(N[1])), " observations"))

# edf (Lect2 EDF of N(0,1) with Different n's)
par(mfrow=c(2,2))
for (i in 1:4)
{
  X <- rnorm(N[i])
  edf_normal <- ecdf(X)
  plot(edf_normal, xlim=c(-4,4), main = paste("EDF of N(0,1) with ", bquote(.(N[i])), " observations"))
}
x<-seq(-3,3,by=0.001)
y<-pnorm(x)
lines(x,y,col="red")


# empirical QQ (Lect2 Example: Empirical Q-Q Plots)
N <- c(10, 50, 100, 1000)
par(mfrow=c(2,2))
for (i in 1:4)
{
  X <- rnorm(N[i])
  qqnorm(X,  main = paste("Empirical Q-Q Plot of N(0,1) with ", bquote(.(N[i])), " observations"))
}
abline(0,1,col="red")








#empirical QQ for DSPLR 
SP500 <- read.table("DSP500.csv",header = T, sep=",")
attach(SP500)
DSP<-SP500$Close
DSPLR <- diff(log(DSP))  # compute log returns; 
# try help(diff) for the usage of diff

n <- length(DSPLR)
q <- seq(1/(n+1),n/(n+1),by=1/n)
q_cauchy <- qcauchy(q)
#DSPLR Q-Q plot (in Lect2 Example:Empiricak Q-Q Plot of DSPLR)
par(mfrow = c(2,1))
#detach("package:Rsafd", unload=TRUE) # !!
qqnorm(DSPLR,main="Empirical Quantiles of DSPLR vs N(0,1)",xlab="N(0,1) quantitles", ylab="DSPLR quantiles")
qqplot(q_cauchy, DSPLR, main="Empirical Quantiles of DSPLR vs Cauchy(0,1)",xlab="Cauchy(0,1) quantitles", ylab="DSPLR quantiles")


# compare the right/left tails
## Figure 10
par(mfrow=c(1,1))
DSPLR_N <- - DSPLR[DSPLR < 0]
DSPLR_P <- DSPLR[DSPLR > 0]
qqplot(DSPLR_P, DSPLR_N)
abline(0,1,col="red")
# negative returns have a bit heavier tail


# Compute VaR under different distribution assumptions
# based on empirical distribution
q <- 0.01 
VaR_emp <- -quantile(DSPLR,q)
# if based on normal
mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)
VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_normal 

# if based on Cauchy
m_DSPLR <- median(DSPLR)
lam_DSPLR <- 1/2 * (quantile(DSPLR,3/4) - quantile(DSPLR,1/4))
c(m_DSPLR, lam_DSPLR)


VaR_cauchy <- - qcauchy(q,m_DSPLR, lam_DSPLR)
VaR_cauchy

c(VaR_normal, VaR_cauchy)




# empirical VaR vs VaR_normal
q<-0.01
VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_emp <- -quantile(DSPLR,q)
c(VaR_normal, VaR_emp)

q <- seq(0.001,0.01,by=0.0002)
q_DSPLR <- quantile(DSPLR,q)
q_norm <- qnorm(q,mu_DSPLR,sd_DSPLR)


par(mfrow=c(1,1))
plot(q,q_DSPLR,ylim=c(-0.07,-0.02),main="Left Tails: DSPLR & Normal",xlab="q",ylab="Quantiles",pch=19)
points(q,q_norm,col="red",pch=23)
legend("bottomright", pch=c(19,23),c("DSPLR","Fitted Normal"), text.col=c("black","red"))

#VaR Computations for DSPLR (n Lect2)


par(mfrow=c(1,2))
plot(q,-q_DSPLR,ylim=c(0.02,0.07),main="VaR based on normal and empirical VaR",xlab="q",ylab="",pch=19)
points(q,-q_norm,col="red",pch=23)
legend("topright", pch=c(19,23),c("DSPLR","Fitted Normal"), text.col=c("black","red"))
plot(q,q_norm/q_DSPLR, main="Ratios between VaR based on normal and empirical VaR ",ylab="Ratios")



# Monte-Carlo for computing expected shortall
q<-0.01
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])
# to understand the usage of [ ], try
A<-c(1,2,3)
B<-A[A<=2]
B


mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)
VaR_normal <-  - qnorm(q,mu_DSPLR, sd_DSPLR)
N<-100000
X<-rnorm(N,mu_DSPLR,sd_DSPLR)
ES_normal <- mean( - X[- X > VaR_normal])
c(ES_emp, ES_normal)


q<-0.001
VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])
N<-1000000
X<-rnorm(N,mu_DSPLR,sd_DSPLR)
ES_normal <- mean(- X[- X > VaR_normal])
c(ES_emp, ES_normal)


# Illustration of LLN
# Generate N(0,1)
temp <- - rnorm(4000, mean = 0, sd = 1)
temp2<-temp[temp>0]
out<- vector()
out2<- vector()
# Means
for (x in 1:1000) {
  out[x]=mean(temp[1:x])
  out2[x]=mean(temp2[1:x])
}
# Figure
par(mfrow=c(2,1))
plot(out, type="l", main="Sample Mean of N(0,1) ???> 0 as n ???> infinity", ylab="Sample mean",lty=1,lwd=2)
abline(h=0, col="red", lty=2)
plot(out2, type="l", main=expression(paste("Sample Mean of N(0,1)'s>0", " -> ", 2/sqrt(2*pi), " as n ???> infinity", sep = "")), ylab="Sample mean",lty=1,lwd=2)
abline(h=(2/sqrt(2*pi)), col="red", lty=2)



# Cauchy simulation
N<-100000
U <- runif(N,0,1)
X <- tan((U-0.5)*pi)
q <- seq(0.01,0.99,by=0.001)
q_Cauchy <- qcauchy(q)
q_emp <- quantile(X,q)

par(mfrow=c(1,1))
plot(q_Cauchy, q_emp,xlab="theoretical quantiles", ylab="Empirical Quantiles", 
     main=paste("Empirical Q-Q Plot of ", bquote(.(N)), " Simulated Cauchy(0,1) vs Cauchy(0,1)"))
abline(0,1,col="red")

# Cauchy(0,1) vs N(0,1)
N<- length(DSPLR)# = length(DSPLR)        
N
GWN <- rnorm(N)
CWN <- rcauchy(N)
#Comparison Between Cauchy and Normal(in Lect2)


par(mfrow=c(2,1))
ts.plot(GWN,main=paste("Plot of ", bquote(.(N)), " N(0,1)"),xlab="index",ylab="")
ts.plot(CWN,main=paste("Plot of ", bquote(.(N)), " C(0,1)"),xlab="index",ylab="")


# extreme values: DSPLR (in Lect2)
par(mfrow=c(1,1))
DSP_time<- seq(from=1960,to=2022.66,length.out=length(DSP)) #DSP_time<- seq(from=1960,to=2020.75,length.out=length(DSP)) 
plot(DSP_time[2:length(DSP)], DSPLR,type="l",
     xlab="Date",main="Daily log return of S&P500 from Jan 1960 to August 2023")

mean(DSPLR)
sd(DSPLR)
min(DSPLR)
(min(DSPLR)-mean(DSPLR))/sd(DSPLR)



#Fitted t distribution (in Lect2 t model)
par(mfrow=c(1,2)) 
hist(DSPLR,breaks=100,  freq = F,main="Histogram of DSPLR vs Fitted t Density",ylim=c(0,60))   
library(MASS)
DSPLR_t_fit <- fitdistr(DSPLR,"t")
m <- DSPLR_t_fit$estimate[1]
lambda <- DSPLR_t_fit$estimate[2]
nu <- DSPLR_t_fit$estimate[3]
curve(dt((x-m)/lambda, df=nu)/lambda,col = "red", add = TRUE)


q <- seq(0.001,0.999,by=0.001)
q_empirical <- quantile(DSPLR,q)
q_t_nu <- qt(q,nu)
plot(q_t_nu,q_empirical,main="Empirical Quantiles vs Fitted t Quantiles")


par(mfrow=c(1,1))
tWN <- rt(N,nu)
ts.plot(tWN,main=paste("Plot of ", bquote(.(N)), " t_nu  "),xlab="index",ylab="")


set.seed(1) #to ensure the sumulation results reproducible
# empirical VaR vs VaR_t
q<-0.01
VaR_emp <- -quantile(DSPLR,q)
VaR_t <- - (qt(q,nu)*lambda+m)
c(VaR_emp,VaR_t)


# empirical ES vs ES_t
q<-0.01
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])

VaR_t <-  - (qt(q,nu)*lambda+m)
N<-100000
X<-rt(N,nu)*lambda+m
ES_t <- mean( - X[- X > VaR_t])
c(ES_emp, ES_t)




# empirical ES vs ES_t
q<-0.01
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])

VaR_t <-  - (qt(q,nu)*lambda+m)
N<-100000
X<-rt(N,nu)*lambda+m
ES_t <- mean( - X[- X > VaR_t])
c(ES_emp, ES_t)

