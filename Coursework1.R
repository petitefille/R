# R code for Coursework 1

source("WS2.R")

# Task 4

# Calculate the MLEs for beta and gamma

# xi = ws2[i]

# first we need to separate the xi values that are >= 0 and the xi values that are negative

n = length(ws2) # 400

pos = 0 # nr of xi >= 0

neg = 0 # nr of negative xi values
# count the nr of values that are >= 0 (pos) and negative (neg)
for (i in 1:n) {
  
  if (ws2[i] >= 0){
    pos = pos + 1
    
  } else {
    neg = neg + 1
  }

}

# pos = 195
# neg =205 
# and pos + neg = 400 = n


WS2_POS = matrix(0,nrow = 1,ncol= pos) # to store xi values >= 0
WS2_NEG = matrix(0,nrow = 1, ncol = neg) # to store negative xi values

x = 1
y = 1

# storing xi values >= 0 in WS2_POS and xi values < 0 in WS2_NEG
for (i in 1:n) {
  if (ws2[i] >=0) {
    WS2_POS[x] = ws2[i]
    x = x +1
  }
  else{
    WS2_NEG[y] = ws2[i]
    y = y + 1
  }
}

beta_hat = (mean(WS2_POS))^(-1) # 1.556191

# n - m = neg = 205

gamma_hat = -(  (mean(WS2_NEG))^(-1)  ) # 2.067524 

 

# End task 4 

# Start Task 5

# Using the asymptotic distributions for the MLEs, construct 95 % confidence intervals for 
# beta_hat and gamma_hat 

z_0.025 = qnorm(0.975) # 1.959964

beta_low = beta_hat - z_0.025*(beta_hat/sqrt(m)) # 1.251183
beta_up = beta_hat + z_0.025*(beta_hat/sqrt(m)) # 1.861199

gamma_low = gamma_hat - z_0.025*(gamma_hat/sqrt(neg)) # 1.784501
gamma_up = gamma_hat + z_0.025*(gamma_hat/sqrt(neg)) # 2.350547

# End task 5

# Start task 7

N = n/2 # 200

X = c(rexp(N,beta_hat),-rexp(N,gamma_hat))
qqplot(X,ws2,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = 'red')

plot(density(X))
plot(density(ws2))




# End task 7

# Start task 8
       
S = 1000

alpha = 0.01
v_0.01 = (S*(1-exp(log(2*alpha)/gamma_hat ) ) ) # 849.2495


alpha = 0.1
v_0.1 = (S*(1-exp(log(2*alpha)/gamma_hat ) ) ) # 540.8771

# End task 8


# Start task 9

alpha = 0.01

VAR_v_0.01 = ((S^2)*((exp((log(2*alpha))/(gamma_hat)))^2)*((log(2*alpha))^2))/(neg*(gamma_hat^2)) 
#VAR_v_0.01 = 396.8854
  
SE_v_0.01 = sqrt(VAR_v_0.01) # 19.92198


alpha = 0.1

VAR_v_0.1 = ((S^2)*((exp((log(2*alpha))/(gamma_hat)))^2)*((log(2*alpha))^2))/(neg*(gamma_hat^2)) 
# VAR_v_0.1 = 623.0909
  
SE_v_0.1 = sqrt(VAR_v_0.1) # 24.96179


# End task 9

 