install.packages("tseries")
library(tseries)

n = 1100
a = c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
e = rnorm(n)  
x <- double(n)
x[1:2] <- rnorm(2, sd = sqrt(a[1]/(1.0-a[2]-a[3]))) 
for(i in 3:n)  # Generate ARCH(2) process
{
  x[i] <- e[i]*sqrt(a[1]+a[2]*x[i-1]^2+a[3]*x[i-2]^2)
}
x <- ts(x[101:1100])
ts.plot(x)   ###plot the data
acf(x)
acf(x^{2})

x.arch <- garch(x, order = c(0,1))  # Fit ARCH(2) 
summary(x.arch)                     # Diagnostic tests
plot(x.arch)                        

data(EuStockMarkets)  
dax <- diff(log(EuStockMarkets))[,"DAX"]
ts.plot(dax)
dax.garch <- garch(dax)  # Fit a GARCH(1,1) to DAX returns
summary(dax.garch)       # ARCH effects are filtered. However, 
plot(dax.garch)          # conditional normality seems to be violated