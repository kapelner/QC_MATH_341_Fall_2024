#### Estimations of theta in logistic distr where sigma = 1: MLE vs xbar performance bakeoff

pacman::p_load(optimx)

mle_logistic_sigma_1 = function(xs){
  optim(mean(xs), method = "BFGS", fn = function(theta){
    e_to_the_theta_times_e_to_the_minus_x = exp(theta) * exp(-xs)
    (n - 2 * sum(e_to_the_theta_times_e_to_the_minus_x / (1 + e_to_the_theta_times_e_to_the_minus_x)))^2
  })$par
}

n = 30
theta = 17
Nsim = 1e5
set.seed(1)

xbars = array(NA, Nsim)
theta_hat_mles = array(NA, Nsim)
for (nsim in 1 : Nsim){
  xs = rlogis(n, location = theta, scale = 1)
  xbars[nsim] = mean(xs)
  theta_hat_mles[nsim] = mle_logistic_sigma_1(xs)
}

head(cbind(xbars, theta_hat_mles))
#bias
mean(xbars - theta)
mean(theta_hat_mles - theta)
#var
var(xbars - theta)
var(theta_hat_mles - theta)
#mse
mse_xbar = mean((xbars - theta)^2)
mse_mle = mean((theta_hat_mles - theta)^2)
mse_xbar
mse_mle
(mse_mle - mse_xbar) / mse_xbar



#### power of Wald vs Score when testing against H_0: theta = 0

n = 100
theta = 0.2
Nsim = 1e5
set.seed(1)

wald_rejections = array(NA, Nsim)
score_rejections = array(NA, Nsim)
for (nsim in 1 : Nsim){
  xs = rlogis(n, location = theta, scale = 1)
  z_wald = mean(xs) / (sd(xs) / sqrt(n))
  z_score = (n - 2 * sum(exp(-xs) / (1 + exp(-xs)))) / sqrt(n / 3)
  wald_rejections[nsim] = abs(z_wald) > 1.96
  score_rejections[nsim] = abs(z_score) > 1.96
}
power_wald = mean(wald_rejections)
power_score = mean(score_rejections)
power_wald
power_score
(power_score - power_wald) / power_wald
