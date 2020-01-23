#
# Workshop 4 - Laplace ARCH(1)
#

source("LaplaceSolve.R")
source("FTSe.R")

#
# MLE
#

y=log(FTSE[2:2000]/FTSE[1:1999])

x=y[1000:1999]
mu=mean(x)

Y=x-mu

gamma=LaplaceSolve(Y,0.000001, 0.000001)

#
# epsilon
#

Yminus=Y[1:999]
Yplus=Y[2:1000]

epsilon=Yplus/(gamma[1]+gamma[2]*abs(Yminus))

sampleLaplace=rexp(10000)*sample(c(-1,1),10000,replace=T)
qqplot(sampleLaplace,epsilon,main="Q-Q plot",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
lines(c(-10,10),c(-10,10),col=2)

#
# VaR
#

Theta=gamma[1]+gamma[2]*abs(Y[1000])

alpha=c(0.1,0.01)
calpha=log(2*alpha) ##alpha quantile
VaR=1000*(1-exp(mu+calpha*Theta))
VaR

#
# Cross-validation
#

Xfull=log(FTSE[2:n]/FTSE[1:(n-1)])

# log returns for full data set.


N=length(Xfull)
m=1000

I=rep(0,1000)

alpha=0.1

#alpha=0.01

for(i in 1:(N-m))
{
  Xrnew=Xfull[i:(m+i-1)] ##data to use at step i of CV
  mu=mean(Xrnew)
  Ynew=Xrnew-mu
  gamma=LaplaceSolve(Ynew,0.000001)
  Theta=gamma[1]+gamma[2]*abs(Ynew[1000])
  
  
  calpha=log(2*alpha) ##alpha quantile
  VaR=1000*(1-exp(mu+calpha*Theta))
  
  I[i]=(1000*(1-exp(Xfull[m+i]))>VaR)
}

sum(I)/(N-m)

#
# Ysq
#

Yabs=rep(0,10)

Yabs[1]=gamma[1]+gamma[2]*abs(Y[1000])
for(i in 2:10) Yabs[i]=gamma[1]+gamma[2]*Yabs[i-1]

#
# Simulations of realisations
#

K=100
H=10

Ysim=matrix(0,ncol=H,nrow=K)

for(i in 1:K)
{
  Theta=gamma[1]+gamma[2]*abs(Y[1000])
  epsilon=rexp(1)*sample(c(-1,1),1)
  Ysim[i,1]=Theta*epsilon
  for(j in 2:H)
  {
    Theta=gamma[1]+gamma[2]*abs(Ysim[i,(j-1)])
    epsilon=rexp(1)*sample(c(-1,1),1)
    Ysim[i,j]=Theta*epsilon
  }
}

plot.ts(Ysim[1,],ylim=c(min(Ysim),max(Ysim)),xlab="Time",ylab="Y_t")
for(i in 2:K) lines(Ysim[i,],col=i)

hist(Ysim[,H],main="Forecast 10 steps ahead",xlab="Y_1010")



