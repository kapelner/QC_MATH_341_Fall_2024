pacman::p_load(fitdistrplus, nycflights13, nimble, VaRES, data.table, ggplot2)
plogistic = function(q, mu, sigma){VaRES::plogistic(q, mu, sigma)}

set.seed(1)
#Look at how much impact the AIC idea has had on science:
#https://scholar.google.com/scholar?hl=en&as_sdt=0%2C39&q=A+new+look+at+the+statistical+model+identification&btnG=

n = 50
x = rt(n, 4)
hist(x, breaks = 20)

#####
#MOD 1: iid N(theta_1, theta_2)
#MOD 2: iid Cauchy(theta_1, theta_2)
#MOD 3: iid Logistic(theta_1, theta_2)
#MOD 4: iid Laplace(theta_1, theta_2)
#MOD 5: iid T(theta_1, theta_2)
#All these models have two parameters: 
#theta_1 represents a center and 
#theta_2 represents a "scale" controlling how fast its tails go to zero
#thus K_m = 2 for all models

fit_normal = fitdist(x, "norm")
fit_cauchy = fitdist(x, "cauchy")
fit_logistic = fitdist(x, "logistic", start = list(mu = mean(x), sigma = 0.1))
fit_laplace = fitdist(x, "dexp", start = list(location = mean(x), scale = 0.1)) #AKA Laplace distribution
fit_t = fitdist(x, "t", start = list(df = 1, ncp = 0))

#here's all the maximum likelihood estimates:
fit_normal
fit_cauchy
fit_logistic
fit_laplace
fit_t

#and all the AIC's
aic_s = c(fit_normal$aic, fit_cauchy$aic, fit_logistic$aic, fit_laplace$aic, fit_t$aic)
names(aic_s) = c("Normal", "Cauchy", "Logistic", "Laplace", "StudentsT")
aic_s_sorted = sort(aic_s)
aic_s_sorted


#now compute the Akaike weights as percentages
best_aic = aic_s_sorted[1]
delta_aics = aic_s_sorted - best_aic
akaike_weights = exp(-delta_aics / 2) / sum(exp(-delta_aics / 2))
round(akaike_weights * 100, 2)

#now compute the AICc's
k = 2 #it's the same for all models
aiccs = aic_s_sorted - 2 * k + 2 * k * n / (n-k-1)
aicc_s_sorted = sort(aiccs)
aicc_s_sorted

best_aicc = aicc_s_sorted[1]
delta_aiccs = aicc_s_sorted - best_aicc
akaike_weights = exp(-delta_aiccs / 2) / sum(exp(-delta_aiccs / 2))
round(akaike_weights * 100, 2)
#doesn't make any difference as the small-n correction is just a constant wrt the weights


#real data - windspeeds - extreme distributions!
X = data.table(nycflights13::weather)
x = as.matrix(X[origin == "JFK", .(max_windspeed = max(wind_speed, na.rm = TRUE)), by = c("month", "day")])[, 3]
ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x), bins = 100) +
  xlab("maximum wind speeds by day at JFK airport in 2013")

pgumbel = function(q, mu, sigma){VaRES::pgumbel(q, mu, sigma)}
pfrechet = function(q, mu, sigma){VaRES::pfrechet(q, mu, sigma)}
pgompertz = function(q, mu, sigma){VaRES::pgompertz(q, mu, sigma)}
pgenlogis = function(q, a, mu, sigma){VaRES::pgenlogis(q, a, mu, sigma)}


fit_exponential = fitdist(x, "exp")
fit_normal = fitdist(x, "norm")
fit_weibull = fitdist(x, "weibull")
fit_gamma = fitdist(x, "gamma")
fit_gumbel = fitdist(x, "gumbel", start = list(mu = mean(x), sigma = 0.1))
fit_gompertz = fitdist(x, "gompertz", start = list(b = .1, eta = .1))
fit_frechet = fitdist(x, "frechet", start = list(alpha = .1, sigma = .1))
fit_genlogis = fitdist(x, "genlogis", start = list(a = 1, mu = mean(x), sigma = 0.1))
#K = 1, 2, 2, 2, 2, 2, 3

list(
  exp = fit_exponential$estimate,
  norm = fit_normal$estimate,
  weibull = fit_weibull$estimate,
  gamma = fit_gamma$estimate,
  gumbel = fit_gumbel$estimate,
  gompertz = fit_gompertz$estimate,
  frechet = fit_frechet$estimate,
  logis = fit_genlogis$estimate
)

unlist(list(
  exp = fit_exponential$loglik,
  norm = fit_normal$loglik,
  weibull = fit_weibull$loglik,
  gamma = fit_gamma$loglik,
  gumbel = fit_gumbel$loglik,
  gompertz = fit_gompertz$loglik,
  frechet = fit_frechet$loglik,
  logis = fit_genlogis$loglik
))

aics = unlist(list(
  exp = fit_exponential$aic,
  norm = fit_normal$aic,
  weibull = fit_weibull$aic,
  gamma = fit_gamma$aic,
  gumbel = fit_gumbel$aic,
  gompertz = fit_gompertz$aic,
  frechet = fit_frechet$aic,
  logis = fit_genlogis$aic
))
aic_s_sorted = sort(aics)
aic_s_sorted
best_aic = aic_s_sorted[1]
delta_aics = aic_s_sorted - best_aic
akaike_weights = exp(-delta_aics / 2) / sum(exp(-delta_aics / 2))
round(akaike_weights, 3)

#examining the AIC penalty (overfitting)
set.seed(1)
n = 20
x = rexp(n, 1 / 17)
fit_exp = fitdist(x, "exp")
fit_gamma = fitdist(x, "gamma") #beta here is fixed at 1, but the computer doesn't know that!

fit_exp$loglik
fit_gamma$loglik
k_exp = 1
k_gamma = 2
aicc_exp = fit_exp$aic - 2 * k_exp + 2 * k_exp * n / (n-k_exp-1)
aicc_gamma = fit_gamma$aic - 2 * k_gamma + 2 * k_gamma * n / (n-k_gamma-1)
aicc_exp
aicc_gamma
#this is just one example dataset 
#you'd have to do many draws and average to ensure it truly works

aicc_s_sorted = sort(c(aicc_exp, aicc_gamma))
best_aicc = aicc_s_sorted[1]
delta_aiccs = aicc_s_sorted - best_aicc
akaike_weights = exp(-delta_aiccs / 2) / sum(exp(-delta_aiccs / 2))
round(akaike_weights * 100, 2)
