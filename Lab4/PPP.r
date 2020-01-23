#setwd("C:/Users/Eier/Documents/Lancaster/MATH581/Lab\ 4/")

# Project: R code to solve task (a), (b), (c) and (d)
#
install.packages("tseries")
library(tseries)

source("FTSe.R") # # FTSE100 closing values: 07.11.02 - 06.10.10 

#
# Part (a)
#
# nr of observations from the FTSE100 data set
n=length(FTSE) # 2000

X=log(FTSE[(n+1-1000):n]/FTSE[(n-1000):(n-1)]) # last 1000 log returns 
# (length(X) = 1000)

# we estimate mu by the sample mean of the 1000 most recent log returns (X)
mu=mean(X) # -8.009957e-05

# Xt = mu + Yt => Yt = Xt - mu
Y=X-mu

#
# we fit the GARCH(1,1) model to Yt 

out.garch11=garch(Y,order=c(1,1))


# ***** ESTIMATION WITH ANALYTICAL GRADIENT ***** 
  
  
#   I     INITIAL X(I)        D(I)

# 1     2.287772e-04     1.000e+00
# 2     5.000000e-02     1.000e+00
# 3     5.000000e-02     1.000e+00

# ...

# ***** RELATIVE FUNCTION CONVERGENCE *****
  
# FUNCTION    -3.841232e+03   RELDX        3.945e-06
# FUNC. EVALS      69         GRAD. EVALS      30
# PRELDF       2.772e-11      NPRELDF      2.772e-11

# I      FINAL X(I)        D(I)          G(I)

# 1    2.478413e-06     1.000e+00     3.521e+02
# 2    1.271763e-01     1.000e+00     1.038e-03
# 3    8.694462e-01     1.000e+00    -5.887e-03

omega = out.garch11$coef[1] # 2.478413e-06 
alpha1 = out.garch11$coef[2] # 0.1271763 
beta1 = out.garch11$coef[3]  # 0.8694462 

#(b)

# Computing residuals
res=Y/out.garch11$fitted[,1]

## QQ-plot of residuals
qqnorm(res)
qqline(res, col = "red")



# (c) 

sigsq = omega + alpha1*(Y[1000]^2) + beta1*(out.garch11$fitted[1000,1]^2)
# 8.346525e-05 

alpha = c(0.1,0.01)
x_alpha = qnorm(alpha) # alpha quantile of the standard normal distribution
VaR = 1000*(1 - exp(mu + x_alpha*sqrt(sigsq)))
VaR
# 11.71906 21.10751

# (d)

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
  Xrnew=Xfull[i:(m +i-1)] ##data to use at step i of CV
  mu=mean(Xrnew)
  Ynew=Xrnew-mu
  out.garch1=garch(Ynew,order=c(1,1))
  sigsq = out.garch11$coef[1] + out.garch11$coef[2]*(Ynew[m]^2) + out.garch11$coef[3]*(out.garch11$fitted[m,1]^2)
  x_alpha=qnorm(alpha) ##alpha quantile
  VaR=1000*(1-exp(mu+x_alpha*sqrt(sigsq)))
  
  I[i]=(1000*(1-exp(Xfull[m+i]))>VaR)
}

sum(I)/(N-m)

# VaR(0.1) = 0.1521522 or 15.22 % 
# Var(0.01) = 0.05905906or 5.91 % 


#Discuss the appropriateness of the GARCH(1,1) model assumptions for the FTSE100
# data. Are there any issues to deal with concerning violations of the GARCH
# assumptions ? 



ts.plot(FTSE[1000:1999])
ts.plot(X) 

acf(X) # autocorrelation 
acf(X^2)

# Ljung-Box Test 
Box.test(res,lag=10,"Ljung")

# Box-Ljung test

# data:  res
# X-squared = 15.242, df = 10, p-value = 0.1235

Box.test(res,lag=15,"Ljung")

# Box-Ljung test

# data:  res
# X-squared = 16.922, df = 15, p-value = 0.3236

Box.test(res,lag=20, "Ljung")

#Box-Ljung test

# data:  res
# X-squared = 19.795, df = 20, p-value = 0.4708

Box.test(res^2,lag=10, "Ljung")
# Box-Ljung test

# data:  res^2
# X-squared = 10.681, df = 10, p-value = 0.3829

Box.test(res^2,lag=15, "Ljung")
# Box-Ljung test

# data:  res^2
# X-squared = 22.926, df = 15, p-value = 0.08573

Box.test(res^2,lag=20, "Ljung")
# Box-Ljung test

# data:  res^2
# X-squared = 26.558, df = 20, p-value = 0.1482


# Jarque–Bera test 
jarque.bera.test(res[2:1000])

# Jarque Bera Test

# data:  res[2:1000]
# X-squared = 52.098, df = 2, p-value = 4.864e-12


# Look at fitting other GARCH models, and comment on the results from these


install.packages("fGarch")
library("fGarch")

install.packages("rugarch")
library("rugarch")


# Treshold GARCH (T-GARCH)

tgarch = garchFit(~garch(1,1), delta = 1, leverage = T, data = Y, trace = F, include.mean = F )
summary(tgarch)


# Title:
#   GARCH Modelling 

# Call:
#  garchFit(formula = ~garch(1, 1), data = Y, delta = 1, include.mean = F, 
#           leverage = T, trace = F) 

# Mean and Variance Equation:
#  data ~ garch(1, 1)
# <environment: 0x000000002aab2ac0>
#  [data = Y]

# Conditional Distribution:
#  norm 

# Coefficient(s):
#  omega      alpha1      gamma1       beta1  
# 0.00031366  0.08352469  0.97286185  0.91219321  

# Std. Errors:
#  based on Hessian 

# Error Analysis:
#         Estimate  Std. Error  t value Pr(>|t|)    
#  omega  3.137e-04   8.054e-05    3.895 9.83e-05 ***
#  alpha1 8.352e-02   1.532e-02    5.452 4.99e-08 ***
#  gamma1 9.729e-01   1.964e-01    4.954 7.29e-07 ***
#  beta1  9.122e-01   1.454e-02   62.755  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Log Likelihood:
#  2945.072    normalized:  2.945072 

# Description:
#  Sat Jan 18 13:15:09 2020 by user: Eier 


# Standardised Residuals Tests:
#  Statistic p-Value     
# Jarque-Bera Test   R    Chi^2  52.49752  3.983924e-12
# Shapiro-Wilk Test  R    W      0.9909344 7.881933e-06
# Ljung-Box Test     R    Q(10)  15.94025  0.1013556   
# Ljung-Box Test     R    Q(15)  18.66677  0.2292333   
# Ljung-Box Test     R    Q(20)  22.16872  0.3314259   
# Ljung-Box Test     R^2  Q(10)  29.79612  0.0009249391
# Ljung-Box Test     R^2  Q(15)  34.65607  0.002751811 
# Ljung-Box Test     R^2  Q(20)  35.68932  0.01672572  
# LM Arch Test       R    TR^2   14.27986  0.2831952   

# Information Criterion Statistics:
#  AIC       BIC       SIC      HQIC 
# -5.882145 -5.862514 -5.882177 -5.874684 




# QQplot of residulas
res=Y/tgarch@sigma.t
qqnorm(res)
qqline(res, col = "red")


# EGARCH(1,1)
spec = ugarchspec(variance.model = list(model="eGARCH"), mean.model = list(armaOrder = c(0,0), include.mean = FALSE))
fit = ugarchfit(data = Y,spec = spec)
fit

# *---------------------------------*
#  *          GARCH Model Fit        *
#   *---------------------------------*
  
#  Conditional Variance Dynamics 	
# -----------------------------------
#   GARCH Model	: eGARCH(1,1)
# Mean Model	: ARFIMA(0,0,0)
# Distribution	: norm 

# Optimal Parameters
# ------------------------------------
#   Estimate  Std. Error   t value Pr(>|t|)
# omega   -0.20029    0.002081  -96.2449        0
# alpha1  -0.14413    0.014646   -9.8406        0
# beta1    0.97681    0.000623 1568.1077        0
# gamma1   0.15314    0.003389   45.1832        0

# Robust Standard Errors:
#   Estimate  Std. Error   t value Pr(>|t|)
# omega   -0.20029    0.006216  -32.2193        0
# alpha1  -0.14413    0.020951   -6.8792        0
# beta1    0.97681    0.000632 1546.1751        0
# gamma1   0.15314    0.018339    8.3506        0

# LogLikelihood : 2955.351 

# Information Criteria
# ------------------------------------
  
# Akaike       -5.9027
# Bayes        -5.8831
# Shibata      -5.9027
# Hannan-Quinn -5.8952

# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#   statistic p-value
# Lag[1]                      1.273  0.2592
# Lag[2*(p+q)+(p+q)-1][2]     1.298  0.4110
# Lag[4*(p+q)+(p+q)-1][5]     4.819  0.1681
# d.o.f=0
# H0 : No serial correlation

# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#   statistic p-value
# Lag[1]                      3.935 0.04728
# Lag[2*(p+q)+(p+q)-1][5]     6.182 0.08148
# Lag[4*(p+q)+(p+q)-1][9]     6.994 0.19969
# d.o.f=2

# Weighted ARCH LM Tests
# ------------------------------------
#   Statistic Shape Scale P-Value
# ARCH Lag[3]     1.083 0.500 2.000  0.2979
# ARCH Lag[5]     1.341 1.440 1.667  0.6350
# ARCH Lag[7]     1.534 2.315 1.543  0.8146

# Nyblom stability test
# ------------------------------------
#   Joint Statistic:  1.1621
# Individual Statistics:              
# omega  0.32393
# alpha1 0.05123
# beta1  0.31792
# gamma1 0.69219

# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	 1.07 1.24 1.6
# Individual Statistic:	 0.35 0.47 0.75

# Sign Bias Test
# ------------------------------------
#   t-value    prob sig
# Sign Bias           0.9079 0.36417    
# Negative Sign Bias  0.2041 0.83835    
# Positive Sign Bias  2.0079 0.04492  **
#   Joint Effect        4.0768 0.25329    


# Adjusted Pearson Goodness-of-Fit Test:
#   ------------------------------------
#   group statistic p-value(g-1)
# 1    20     22.88      0.24267
#     30     30.20      0.40407
# 3    40     46.24      0.19811
# 4    50     65.20      0.06055


# Elapsed time : 0.227391 


egarch.sigma.t = as.vector(sigma(fit)[,1])

             
# QQplot of residulas
res3=Y/egarch.sigma.t
qqnorm(res3)
qqline(res3, col = "red")

# Ljung-Box Test 
# Box.test(res3,lag=10,"Ljung")
# Box-Ljung test

# data:  res3
# X-squared = 14.856, df = 10, p-value = 0.1374

Box.test(res3,lag=15, "Ljung")
# Box-Ljung test

# data:  res3
# X-squared = 16.92, df = 15, p-value = 0.3237

Box.test(res3,lag=20, "Ljung")
# Box-Ljung test

# data:  res3
# X-squared = 19.921, df = 20, p-value = 0.4629

Box.test(res3^2,lag=10, "Ljung")

# Box-Ljung test

# data:  res3^2
# X-squared = 8.9428, df = 10, p-value = 0.5375


Box.test(res3^2,lag=15, "Ljung")

# Box-Ljung test

# data:  res3^2
# X-squared = 20.353, df = 15, p-value = 0.1588

Box.test(res3^2,lag=20, "Ljung")

# Box-Ljung test

# data:  res3^2
# X-squared = 22.495, df = 20, p-value = 0.3143

jarque.bera.test(res3)
# Jarque Bera Test
# Jarque Bera Test

# data:  res3
# X-squared = 37.135, df = 2, p-value = 8.634e-09



# Comparing mle of sigma_t for different GARCH models

plot.sigt = function(y,sigt,c){
  limy = range(abs(y),-abs(y)) 
  par(mfrow=c(1,1)) 
  plot(abs(y),xlab="Days",ylab="Log-returns (-/+)",main="",type="h",axes=FALSE,ylim=limy) 
  lines(-abs(y), type="h")
  axis(2);box();axis(1,at = c(0,200,400,600, 800, 1000)) 
  for (i in 1:3) {
    lines(sigt[i,],col=c[i]) 
    lines(-sigt[i,],col=c[i]) 
    
  }
  title("Comparing SD(Yt|Yt-1) for different GARCH models")
}
sigt = matrix(0, nrow = 3, ncol = 1000)
sigt[1,] = out.garch11$fitted[,1] # GARCH(1,1)
sigt[2,] = tgarch@sigma.t # TGARCH(1,1)
sigt[3,] = egarch.sigma.t # EGARCH(1,1)

col = rainbow(3)



plot.sigt(Y,sigt,col)

op = par(cex=0.7)
legend(650,-0.05,legend = c("GARCH(1,1)","TGARCH(1,1)","EGARCH(1,1)"),col = col, lty = 1)




