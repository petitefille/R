# setwd("C:/Users/Eier/Documents/Lancaster/MATH581/Lab\ 4/")
# source("FTSe.R")

install.packages("tseries")
library(tseries)

n = length(FTSE) # 2000
# X = log(FTSE(n+1 ))

data = log(FTSE[2:2000]/FTSE[(1:1999)])
X = data[1000:1999]

mu = mean(X)

Y = X - mu

# fit  model 
# data.arch = garch(data, order = c(0,1))
out.arch = garch(Y, order = c(0,1))

# 2. 
omega_hat = out.arch$coef[1]
alpha_hat = out.arch$coef[2]
sigma_hat = omega_hat/(1 - alphahat)
epsilon = (X - mu_hat)/sigma_hat 
  
res = Y/out.arch1$fitted[,1]
qqline(res)

# 3. 

VaR = S*(1-exp(alpha_hat + c_alpha*sigma_hat))


# 4. use cross - validation to evaluate this approach for estimate Value at Risk
N = 1999 # length of X = log returns 
n = 1000 # length of Xr = X[1000:1999] og som vi har brukt for å regne ut VaR
alpha = 0.01

I = rep(0, N-n) # lengde: N-n = 1999 - 1000 = 999 

for (i in 1:(N-n)) {   # for i = 1,..., N-n
  Xrnew = X[i:(n+i-1)] #  data to use at step i of CV
  mu = mean(Xrnew)
  # theta_hat = n/sum(abs(Xrnew)) # MLE for Laplace model 
  # xm = log(2*alpha)/theta_hat # formula for quantile if alpha < 0.5 
  
  
  
  sigma_hat = sqrt(omega + alpha* )
  c_alpha = qnorm( )  
  VaR = S*(1-exp(mu + c_alpha*sigma_hat))
  I[i] =( S*(1-exp(X[n+i])) > VaR   )
}


# result = Value at risk 


# 5

for (i in 1:10){
  
}



# 6 




sum(I)  # 34
sum(I)/(N-n)  # 0.03403403



# 5. 


# 6. 

# 7. Laplace not 