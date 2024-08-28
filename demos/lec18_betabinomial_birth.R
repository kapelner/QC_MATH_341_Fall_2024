pacman::p_load(fitODBOD, ggplot2)

#let's look at some beta binomial prior predictive distributions first

dbetabinom = function(x, n, alpha, beta){
  exp(lchoose(n, x) - lbeta(alpha, beta) + lbeta(x + alpha, n - x + beta) )
}
xmax = 17
ggplot() + ylab("p(x)") + xlab("x") + ylim(0, 1) + 
  geom_bar(aes(x = 0 : xmax, y = dbetabinom(0 : xmax, xmax, 1, 1)), stat = "identity")
ggplot() + ylab("p(x)") + xlab("x") + ylim(0, 1) + 
  geom_bar(aes(x = 0 : xmax, y = dbetabinom(0 : xmax, xmax, 1e-6, 1e-6)), stat = "identity")
ggplot() + ylab("p(x)") + xlab("x") + ylim(0, 1) + 
  geom_bar(aes(x = 0 : xmax, y = dbetabinom(0 : xmax, xmax, 1e6, 1e6)), stat = "identity")
rbind(dbetabinom(0 : xmax, xmax, 1e6, 1e6), dbinom(0 : xmax, xmax, 0.5))

#original data
x = c(3,24,104,286,670,1033,1343,1112,829,478,181,45,7)
ggplot(data.frame(freq = x, num_boys = 0 : 12)) + 
  geom_bar(aes(x = num_boys, y = freq), stat = "identity") + 
  xlab("Number of boys out of first 12 births")
sum(x) #number of couples who had >=12 children surveyed

x_boys = x * (0 : 12)
x_girls = x * (12 : 0)
x_num_boys = sum(x_boys)
x_num_girls = sum(x_girls)

n = x_num_boys + x_num_girls
n
xbar = x_num_boys / n
xbar

#Model 1: X_1, ..., X_73380 ~iid Bern(theta)
binomial_loglik = x_num_boys * log(xbar) + x_num_girls * log(1 - xbar)

p_binomial = dbinom(0:12, 12, xbar)
p_binomial
x_pred_binomial = round(p_binomial * sum(x))
x_pred_binomial

#Model 1: X_1, ..., X_6115 ~iid BetaBinom(12, alpha, beta)
bbmles = EstMLEBetaBin(x = 0 : 12, freq = x, a = 100, b = 100)
alpha_hat_hat_mle = bbmle::coef(bbmles)[1]
beta_hat_hat_mle = bbmle::coef(bbmles)[2]
alpha_hat_hat_mle
beta_hat_hat_mle

ggplot() +
  xlim(0, 1) +
  geom_function(fun = dbeta, args = list(shape1 = alpha_hat_hat_mle, shape2 = beta_hat_hat_mle))

cr_hat_hat_theta_95percent = c(
  qbeta(0.025, alpha_hat_hat_mle, beta_hat_hat_mle),
  qbeta(0.975, alpha_hat_hat_mle, beta_hat_hat_mle)
)
cr_hat_hat_theta_95percent

aic_binomial = -2 * binomial_loglik + 2 * 1
round(aic_binomial)
fbb = fitBetaBin(x = 0 : 12, obs.freq = x, a = alpha_hat_hat_mle, b = beta_hat_hat_mle)
round(fbb$AIC)

p_pred_betabinomial = fitODBOD::dBetaBin(0 : 12, 12, alpha_hat_hat_mle, beta_hat_hat_mle)$pdf
p_pred_betabinomial
x_pred_betabinomial = round(p_pred_betabinomial * sum(x))
x_pred_betabinomial



data.frame(
  x = x,
  x_pred_binomial = x_pred_binomial,
  x_pred_betabinomial = x_pred_betabinomial
)
