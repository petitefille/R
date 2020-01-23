
2.
sample(x,size,replace =TRUES)

3. Simulating a standard Wiener process

> N = 10 ## number of realisations
> t = 2
> xt = rnorm(N,0,sqrt(t))
> xt
[1] -0.7896249  1.5973102  2.6674164  1.5336862 -0.3046570 -1.9853688 -1.3302308  1.3542312
[9]  0.1681342 -2.3663887

N = 4 ## nr of realisations
"rror: unexpected input in "
> N = 4 
> s = 2
> t = 2.5
> xs = rnorm(N,0,sqrt(2))
> dx = rnorm(N,0,sqrt(t-s))
> xt = xs + dx
> times = c(s,t)
> X = matrix(0,nrow=N,ncol=length(times))
> X[,1] = xs
> X[,2] = xt
> Delta = 0.1
> n = 10
> N = 4
> times = (0:n)*Delta
> X = matrix(0,nrow=N,ncol=n+1)
> for (i in 1:n) {
  +     dx = rnorm(N,0,sqrt(Delta))
  +     X[,i+1] = X[,i] + dx
  + }    
> plot(times,X[1,], type="l", xlab="Time", ylab="X_t")
> plot(range(times),range(X), type="n", xlab="Time", ylab="X_t")
> for (i in 1:N) {
  +     lines(times,X[i,])
  + }

c)

rWiener = function(n,N,Delta) {
  times = (0:n)*Delta
  X = matrix(0,nrow=N,ncol=n+1)
  for (i in 1:n) {
    dx = rnorm(N,0,sqrt(Delta))
    X[,i+1] = X[,i] + dx
  }
  return(list(X=X,times=times))
}
rWiener(10,4,0.1)


R version 3.5.1 (2018-07-02) -- "Feather Spray"
Copyright (C) 2018 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> normr?
  + ;
Error: unexpected ';' in:
  "normr?
;"
> qnorm(0.05)
[1] -1.644854
> pnorm(12.42)
[1] 1
> pnorm(0.3928)
[1] 0.6527664
> qnorm(0.975)
[1] 1.959964
> qnorm(0.95)
[1] 1.644854
> qnorm(2.778)
[1] NaN
Warning message:
  In qnorm(2.778) : NaNs produced
> dnorm(2.778)
[1] 0.008416337
> dnorm(87.84)
[1] 0
> pnorm(2.778)
[1] 0.9972653
> pnorm(87.8410)
[1] 1
> N = 10 ## number of realisations
> t = 2
> xt = rnorm(N,0,sqrt(t))
> xt
[1] -0.7896249  1.5973102  2.6674164  1.5336862 -0.3046570 -1.9853688 -1.3302308  1.3542312
[9]  0.1681342 -2.3663887
N = 4 ## nr of realisations
"rror: unexpected input in "
> N = 4 
> s = 2
> t = 2.5
> xs = rnorm(N,0,sqrt(2))
> dx = rnorm(N,0,sqrt(t-s))
> xt = xs + dx
> times = c(s,t)
> X = matrix(0,nrow=N,ncol=length(times))
> X[,1] = xs
> X[,2] = xt
> Delta = 0.1
> n = 10
> N = 4


times = (0:n)*Delta
X = matrix(0,nrow=N,ncol=n+1)
for (i in 1:n) {
       dx = rnorm(N,0,sqrt(Delta))
       X[,i+1] = X[,i] + dx
   }    
plot(times,X[1,], type="l", xlab="Time", ylab="X_t")
plot(range(times),range(X), type="n", xlab="Time", ylab="X_t")
for (i in 1:N) {
     lines(times,X[i,])
 }
 rWiener = function(n,N,Delta) {
     times = (0:n)*Delta
      X = matrix(0,nrow=N,ncol=n+1)
     for (i in 1:n) {
            dx = rnorm(N,0,sqrt(Delta))
            X[,i+1] = X[,i] + dx
        }
       return(list(X=X,times=times))
   }
> rWiener(10,4,0.1)
$`X`
[,1]       [,2]       [,3]       [,4]         [,5]       [,6]        [,7]       [,8]
[1,]    0 -0.5809373 -0.1023999 -0.2331175 -0.002944738  0.0749253  0.18849512 0.14149794
[2,]    0  0.1164738  0.1253587 -0.1269085 -0.589150235 -0.2573182 -0.06334851 0.05022974
[3,]    0  0.7081652  0.5142093  0.4809640  0.593138003  0.9186753  1.34076351 1.15253624
[4,]    0 -0.2546923 -1.3892339 -1.5532582 -0.917141699 -0.8561203 -0.42300558 0.28230309
[,9]      [,10]       [,11]
[1,] 0.2595069 -0.1584419 -0.03815827
[2,] 0.5069074  0.4590369  0.87894085
[3,] 1.3890253  0.9602115  0.70501513
[4,] 0.4801156 -0.2435994 -0.68919201

$times
[1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

WP$X
[,1]         [,2]         [,3]        [,4]        [,5]        [,6]         [,7]
[,8]        [,9]       [,10]        [,11]       [,12]       [,13]       [,14]
[,15]       [,16]        [,17]       [,18]        [,19]      [,20]      [,21]
[,22]       [,23]       [,24]       [,25]       [,26]       [,27]       [,28]
[,29]       [,30]       [,31]       [,32]        [,33]       [,34]       [,35]


> WP$times
[1]  0.00  0.01  0.02  0.03  0.04  0.05  0.06  0.07  0.08  0.09  0.10  0.11  0.12  0.13
[15]  0.14  0.15  0.16  0.17  0.18  0.19  0.20  0.21  0.22  0.23  0.24  0.25  0.26  0.27
[29]  0.28  0.29  0.30  0.31  0.32  0.33  0.34  0.35  0.36  0.37  0.38  0.39  0.40  0.41
[43]  0.42  0.43  0.44  0.45  0.46  0.47  0.48  0.49  0.50  0.51  0.52  0.53  0.54  0.55

N = dim(WP$X)[1]
plot(range(WP$times),range(WP$X), type ="n",xlab="Time",ylab="X_t")
for(i in 1:N) {
      lines(WP$times,WP$X[i,])
 }

4. Transforming a standard Wiener process

WP = rWiener(1000,10,0.01 )
Y = 10 + 2*WP$X

plot(range(WP$times),range(Y),type="n",xlab="Time",ylab="Y_t")
for(i in 1:10){
     lines(WP$times,Y[i,])
 }


------------------------------
  
  WP = rWiener(100,1,0.1)

N = 1000
n = 100
Delta = 0.01
WP = rWiener(n, N, Delta)

Y = rep(0,N) ## Vector to store realisations
## Loop to calculate Y
for (i in 1:N) {
  Y[i] = Delta*sum(WP$X[i,2:(n+1)])
}

hist(Y)

qqnorm(Y)

qqline(Y)


mean(Y)
[1] 0.02675086
var(Y)
> var(Y)
[1] 0.3401068

1:10
cumsum(1:10)

Y = matrix(0,nrow=N,ncol=n+1) ## Matrix to store realisations
## lopp to calculate Y
for (i in 1:N) {
  Y[i,] = Delta*cumsum(WP$X[i,])
}

## PLOT

plot(range(WP$times),range(Y),type="n", xlab="Time", ylab="Y_t")
for (i in 1:5) {
  lines(WP$times,Y[i,])
}

plot(range(WP$times),range(Y),type="n", xlab="Time", ylab="Y_t")
for (i in 1:10) {
  lines(WP$times,Y[i,])
}

plot(range(WP$times),range(WP$X),type="n", xlab="Time", Xlab="X_t")
for (i in 1:10) {
  lines(WP$times,WP$X[i,])
}
-----------------------------------------------------------------------
# mu = 0.01
## Black - Sholes Model 
 n = 1000

 N = 10
 Delta = 0.01
 WP = rWiener(n,N,Delta)

S0 = 1
mu =0.01
sigma = sqrt(0.02)

## Matrix to store realisations

St = matrix(0,nrow = N, ncol = n + 1)

## Loop to transform each realisation
for (i in 1:N) { # N = 10
  St[i,] = S0*exp(mu*WP$times+sigma*WP$X[i,]) 
}

## Plot
plot(range(WP$times), range(St), type ="n", xlab = "Time", ylab = "S_t")
for (i in 1:N) {
  lines(WP$times,St[i,])
}

# 4. Black-Scholes Model (ctd)

# (a)
rho = 0.03
## Matrix to store 
Pt = matrix(0,nrow=N,ncol= n+1)
## Loop to transform each realisation 
for (i in 1:N) {
  Pt[i,] = St[i,]*exp(-rho*WP$times)
}
--------
## Plot : mu = 0.01
plot(range(WP$times), range(Pt), type="n", xlab = "Time", ylab = "S_t")
for (i in 1:N) {
       lines(WP$times, Pt[i,])
}

--------------------------------------------------------------------------------
 #  mu = 0.02
   n = 1000
#There were 12 warnings (use warnings() to see them)
N = 10
Delta = 0.01
 WP = rWiener(n,N,Delta)

S0 = 1
mu =0.02
sigma = sqrt(0.02)

## Matrix to store realisations

St = matrix(0,nrow = N, ncol = n + 1)

## Loop to transform each realisation
for (i in 1:N) { # N = 10
  St[i,] = S0*exp(mu*WP$times+sigma*WP$X[i,]) 
}

## Plot
# plot(range(WP$times), range(St), type ="n", xlab = "Time", ylab = "S_t")
# for (i in 1:N) {
 #  lines(WP$times,St[i,])
# }

# 4. Black-Scholes Model (ctd)

# (a)
rho = 0.03
## Matrix to store 
Pt = matrix(0,nrow=N,ncol= n+1)
## Loop to transform each realisation 
for (i in 1:N) {
  Pt[i,] = St[i,]*exp(-rho*WP$times)
}
-------------------
## Plot : mu = 0.02
plot(range(WP$times), range(Pt), type="n", xlab = "Time", ylab = "S_t")
for (i in 1:N) {
  lines(WP$times, Pt[i,])
}

-------------------------------------------------------------------------------------
  # mu = 0.03
  

   n = 1000

 N = 10
 Delta = 0.01
 WP = rWiener(n,N,Delta)

S0 = 1
mu =0.03
sigma = sqrt(0.02)

## Matrix to store realisations

St = matrix(0,nrow = N, ncol = n + 1)

## Loop to transform each realisation
for (i in 1:N) { # N = 10
  St[i,] = S0*exp(mu*WP$times+sigma*WP$X[i,]) 
}

## Plot
# plot(range(WP$times), range(St), type ="n", xlab = "Time", ylab = "S_t")
# for (i in 1:N) {
#  lines(WP$times,St[i,])
# }

# 4. Black-Scholes Model (ctd)

# (a)
rho = 0.03
## Matrix to store 
Pt = matrix(0,nrow=N,ncol= n+1)
## Loop to transform each realisation 
for (i in 1:N) {
  Pt[i,] = St[i,]*exp(-rho*WP$times)
}
---------------
## Plot : mu = 0.01
plot(range(WP$times), range(Pt), type="n", xlab = "Time", ylab = "S_t")
for (i in 1:N) {
  lines(WP$times, Pt[i,])
}


--------------------------------------------------------------------------------
  # mu = 0.04
  
  
  n = 1000

 N = 10
 Delta = 0.01
 WP = rWiener(n,N,Delta)

S0 = 1
mu =0.04
sigma = sqrt(0.02)

## Matrix to store realisations

St = matrix(0,nrow = N, ncol = n + 1)

## Loop to transform each realisation
for (i in 1:N) { # N = 10
  St[i,] = S0*exp(mu*WP$times+sigma*WP$X[i,]) 
}

## Plot
# plot(range(WP$times), range(St), type ="n", xlab = "Time", ylab = "S_t")
# for (i in 1:N) {
#  lines(WP$times,St[i,])
# }

# 4. Black-Scholes Model (ctd)

# (a)
rho = 0.03
## Matrix to store 
Pt = matrix(0,nrow=N,ncol= n+1)
## Loop to transform each realisation 
for (i in 1:N) {
  Pt[i,] = St[i,]*exp(-rho*WP$times)
}
-------------
## Plot : mu = 0.04
plot(range(WP$times), range(Pt), type="n", xlab = "Time", ylab = "S_t")
for (i in 1:N) {
  lines(WP$times, Pt[i,])
}
-------------------------------------------------------

  
  
  ----------------------------------------------------
  
# 4.b and mu = 0.01 (see copy code section below)
  plot(range(WP$times), range(Pt), type="n", xlab = "Time", ylab = "S_t")
for (i in 1:N) {
  # lines(WP$times, Pt[i,])
  lines(WP$times, S0*exp( (mu + 0.5*sigma^2 -rho)*WP$times), col = 2)
}

  
  -------------------------------------------------------------------
  
  
  
## The OU process
  # 1
  
  rOU = function(n,N,Delta,theta, sigma) {
       times = (0:n)*Delta   ## Vector of t_0, t_1, ..., t_n
       X = matrix(0, nrow = N, ncol = n+1)
       
       for (i in 1:n) {
           x = X[,i] # current value   
           m = x*exp(-theta*Delta) # mean of the new value 
           v = (sigma^2)*(1 - exp(-2*theta*Delta)) / (2*theta) ## Variance of new value 
           X[,i+1] = rnorm(N,m,sqrt(v)) # simulate new value 
       }
       return(list(X=X, times =times))
  }
    
N = 10
 n = 1000
 Delta = 0.01
OU = rOU(n,N,Delta,1,sqrt(2))
 ## PLOT 

plot(range(OU$times), range(OU$X), type ="n", xlab = "Time", ylab ="X_t")
for (i in 1:N) {
  lines(OU$times,OU$X[i,])
}

# lines(OU$times,OU$X[1,])

# 1

n = 1000
N = 10
Delta = 0.01
theta = 0.5
sigma = 1

OU = rOU(n,N,Delta,theta,sigma)
# now transform

r0 = 10
" storage for R_t"
Rt = matrix(0,nrow =N, ncol = n+1)
# Loop to transform OU to R_t
for (i in 1:N) {
  Rt[i,] = exp(-theta*OU$times)*r0 + OU$X[i,]
}

# plot 

plot(range(OU$times), range(Rt), type = "n", xclab = "Time", ylab = "R_t")

for (i in 1:N) {
  lines(OU$times, Rt[i,])
}





# 2

n = 1000
N = 10
Delta = 0.01
theta = 2
sigma = sqrt(0.1) 
OU = rOU(n, N, Delta, theta, sigma)

# now transform

v0 = 10
mu = -2

# storage for V_t
Vt = matrix(0,nrow = N, ncol = n + 1)
# Loop to trandform OU to V_t
for (i in 1:N) {
    Vt[i,] = exp(-theta*OU$times)*v0 + (1 - exp(-theta*OU$times))*mu + OU$X[i,]
}

## plot

plot(range(OU$times), range(Vt), type="n", xlab = "Times", ylab = "V_t")
for (i in 1:N) {
  lines(OU$times, Vt[i,])
}