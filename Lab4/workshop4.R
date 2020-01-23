#setwd("C:/Users/Eier/Documents/Lancaster/MATH581/Lab\ 4/")
# Workshop 4 solutions
#

library(tseries)
source("FTSe.R")

#
# Question 1
#

n=length(FTSE)

X=log(FTSE[(n+1-1000):n]/FTSE[(n-1000):(n-1)])

#
# Last 1000 log returns
# 

mu=mean(X)

Y=X-mu

#
# Fitting ARCH(1) model

out.arch1=garch(Y,order=c(0,1))

#
# Computing residuals

res=Y/out.arch1$fitted[,1]
qqnorm(res)
qqline(res)

#estimate of sigma^2 for next obs
#

sigsq=out.arch1$coef[1]+out.arch1$coef[2]*Y[1000]^2
# 0.0001910985 
#
# VaR

alpha=c(0.1,0.01)
calpha=qnorm(alpha) ##alpha quantile
VaR=1000*(1-exp(mu+calpha*sqrt(sigsq)))
VaR
# 17.63865 31.72501
#
# Cross-validation
#

Xfull=log(FTSE[2:n]/FTSE[1:(n-1)])

# log returns for full data set.


N=length(Xfull) # 1999
m=1000

I=rep(0,1000)

alpha=0.1

# alpha=0.01

for(i in 1:(N-m))
{
  Xrnew=Xfull[i:(m+i-1)] ##data to use at step i of CV
  mu=mean(Xrnew)
  Ynew=Xrnew-mu
out.arch1=garch(Ynew,order=c(0,1))
sigsq=out.arch1$coef[1]+out.arch1$coef[2]*Ynew[m]^2


calpha=qnorm(alpha) ##alpha quantile
VaR=1000*(1-exp(mu+calpha*sqrt(sigsq)))

  I[i]=(1000*(1-exp(Xfull[m+i]))>VaR)
}

sum(I)/(N-m)

#
# Ysq
#

Ysq=rep(0,10)

Ysq[1]=out.arch1$coef[1]+out.arch1$coef[2]*Y[1000]^2
for(i in 2:10) Ysq[i]=out.arch1$coef[1]+out.arch1$coef[2]*Ysq[i-1]

#
# Simulations of realisations
#

K=100
H=10

Ysim=matrix(0,ncol=H,nrow=K)

for(i in 1:K)
{
sigsq=out.arch1$coef[1]+out.arch1$coef[2]*Y[1000]^2
epsilon=rnorm(1)
Ysim[i,1]=sqrt(sigsq)*epsilon
for(j in 2:H)
{
sigsq=out.arch1$coef[1]+out.arch1$coef[2]*Ysim[i,(j-1)]^2
epsilon=rnorm(1)
Ysim[i,j]=sqrt(sigsq)*epsilon
}
}

plot.ts(Ysim[1,],ylim=c(min(Ysim),max(Ysim)),xlab="Time",ylab="Y_t")
for(i in 2:K) lines(Ysim[i,],col=i)

hist(Ysim[,H])

