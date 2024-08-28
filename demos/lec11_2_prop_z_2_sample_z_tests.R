#which has more power? The theta_shared? Or the standard 2-prop z test?

set.seed(1)
theta_1 = 0.25
theta_2 = 0.27
n1 = 500
n2 = 500
Nsim = 1e6

rejections_shared = array(NA, Nsim)
rejections_standard = array(NA, Nsim)
for (nsim in 1 : Nsim){
  sum_x1 = rbinom(1, n1, theta_1)
  xbar1 =  sum_x1 / n1
  sum_x2 = rbinom(1, n2, theta_2)
  xbar2 = sum_x2 / n2
  thetahat_s = (sum_x1 + sum_x2) / (n1 + n2)
  xbar_diff = xbar1 - xbar2
  z_shared = xbar_diff / (sqrt(1 / n1 + 1 / n2) * sqrt(thetahat_s * (1 - thetahat_s)))
  z_standard = xbar_diff / sqrt(xbar1 * (1 - xbar1) / n1 + xbar2 * (1 - xbar2) / n2)
  rejections_shared[nsim] = abs(z_shared) > 1.96
  rejections_standard[nsim] = abs(z_standard) > 1.96
}
mean(rejections_shared, na.rm = TRUE)
mean(rejections_standard, na.rm = TRUE)


### Let's do examples of each test and each CI from the past two lectures

##Two proportions
theta_1 = 0.65
theta_2 = 0.50
n_1 = 30
n_2 = 30

set.seed(1)
x1s = rbinom(n_1, 1, theta_1)
x2s = rbinom(n_2, 1, theta_2)

xbar_1 = mean(x1s)
xbar_2 = mean(x2s)
xbar_diff = xbar_1 - xbar_2
theta_hat_hat_s = (n_1 * xbar_1 + n_2 * xbar_2) / (n_1 + n_2)

#Two-prop z-test of equal proportion (both ways)
z_hat_hat = xbar_diff / 
  (sqrt(1 / n_1 + 1 / n_2) * sqrt(theta_hat_hat_s * (1 - theta_hat_hat_s)))
z_hat_hat

z_hat_hat = xbar_diff / 
  (sqrt(xbar_1 * (1 - xbar_1) / n_1 + xbar_2 * (1 - xbar_2) / n_2))
z_hat_hat

#Decision: Retain (Type II error due to not enough power)

#CI for difference of two proportions (alpha = 5%)
moe = 2 * (sqrt(xbar_1 * (1 - xbar_1) / n_1 + xbar_2 * (1 - xbar_2) / n_2))
c(xbar_diff - moe, xbar_diff + moe)
#This CI covers theta_1 - theta2

#CI for each sample (alpha = 5%)
moe1 = 2 * sqrt(xbar_1 * (1 - xbar_1) / n_1)
c(xbar_1 - moe, xbar_1 + moe)
#This CI covers theta_1

moe2 = 2 * sqrt(xbar_2 * (1 - xbar_2) / n_2)
c(xbar_2 - moe, xbar_2 + moe)
#This CI covers theta_2
  

##One mean with DGP: iid normal
n = 30
theta = 17
set.seed(1)
xs = rnorm(n, mean = theta, sd = 10)

xbar = mean(xs)
s = sd(xs)

#1-sample z test of H_0: theta = 0
z_hat_hat = xbar / (s / sqrt(n))
z_hat_hat

#Decision: Reject H_0 (correct)

#1-sample CI (alpha = 5%)
moe = 2 * s / sqrt(n)
c(xbar - moe, xbar + moe)
#This CI covers theta

##One mean with DGP: not iid normal
n = 30
theta = 17
set.seed(1)
xs = rexp(n, rate = 1 / theta)

xbar = mean(xs)
s = sd(xs)

#1-sample z test of H_0: theta = 10
z_hat_hat = (xbar - 10) / (s / sqrt(n))
z_hat_hat

#Decision: Reject H_0 (correct)

#1-sample CI (alpha = 5%)
moe = 2 * s / sqrt(n)
c(xbar - moe, xbar + moe)
#This CI covers theta

##Two means with DGP: iid normal
n_1 = 30
n_2 = 30
theta_1 = 37
theta_2 = 17
set.seed(1)
x1s = rnorm(n_1, mean = theta_1, sd = 10)
x2s = rnorm(n_2, mean = theta_2, sd = 10)

xbar_1 = mean(x1s)
xbar_2 = mean(x2s)
xbar_diff = xbar_1 - xbar_2
s_1 = sd(x1s)
s_2 = sd(x2s)

#1-sample z test of H_0: theta = 0
z_hat_hat = xbar_diff / sqrt(s_1^2 / n_1 + s_2^2 / n_2)
z_hat_hat

#Decision: Reject H_0 (correct)

#1-sample CI (alpha = 5%)
moe = 2 * sqrt(s_1^2 / n_1 + s_2^2 / n_2)
c(xbar_diff - moe, xbar_diff + moe)
#This CI covers theta_1 - theta_2

##Two means with DGP: not iid normal
n_1 = 30
n_2 = 30
theta_1 = 37
theta_2 = 17
set.seed(1)
x1s = rexp(n_1, rate = 1 / theta_1)
x2s = rexp(n_2, rate = 1 / theta_2)

xbar_1 = mean(x1s)
xbar_2 = mean(x2s)
xbar_diff = xbar_1 - xbar_2
s_1 = sd(x1s)
s_2 = sd(x2s)

#1-sample z test of H_0: theta = 0
z_hat_hat = xbar_diff / sqrt(s_1^2 / n_1 + s_2^2 / n_2)
z_hat_hat

#Decision: Reject H_0 (correct)

#1-sample CI (alpha = 5%)
moe = 2 * sqrt(s_1^2 / n_1 + s_2^2 / n_2)
c(xbar_diff - moe, xbar_diff + moe)
#This CI covers theta_1 - theta_2