#
# Newton-Raphson solver of MLE for Laplacian ARCH(1) model.
#

#
# Function (derivative of log-likelihood)
#

fLaplace=function(x,gamma)
{
  n=length(x)
  R=gamma[1]+gamma[2]*abs(x[1:(n-1)])
  O=rep(0,2)
  O[1]=-sum(1/R)+sum(abs(x[2:n])/R^2)
  O[2]=-sum(abs(x[1:(n-1)])/R)+sum(abs(x[1:(n-1)])*abs(x[2:n])/R^2)
  O
}

#
# Derivative of Function (second derivative of log-likelihood)
#

fLDiv=function(x,gamma)
{
  n=length(x)
  R=gamma[1]+gamma[2]*abs(x[1:(n-1)])
  O=rep(0,2)
  O[1]=sum(1/R^2)-2*sum(abs(x[2:n])/R^3)
  O[2]=sum(abs(x[1:(n-1)])^2/R^2)-2*sum(abs(x[1:(n-1)])^2*abs(x[2:n])/R^3)
  O
}

#
# Newton-Raphson algorithm
#

LaplaceSolve=function(x,eps)
  # eps gives convergence requirement
{
  n=length(x)
  y=abs(x)
  A=sum(y[1:(n-1)])
  diff=2*eps
  gamma=rep(0,2)
  gamma[1]=A/(n-1)
  #print(gamma)
  while(diff>eps)
  {
    gammaold=gamma
    gamma=gamma-fLaplace(x,gamma)/fLDiv(x,gamma)
    diff=sum(abs(gamma-gammaold))
    #print(gamma)
  }
  gamma
}

#
# To run on data x:
#
# LaplaceSolve(x,0.0001)
