pacman::p_load(fitdistrplus, wiqid, nimble, VaRES, data.table, ggplot2)

set.seed(1)
n = 50
x = abs(3 + rt(n, 2))

round(x, 2)
mean(round(x, 2))
var(round(x, 2))
hist(x, breaks = 20)

plogistic = function(q, mu, sigma){VaRES::plogistic(q, mu, sigma)}
pgumbel = function(q, mu, sigma){VaRES::pgumbel(q, mu, sigma)}
pfrechet = function(q, mu, sigma){VaRES::pfrechet(q, mu, sigma)}
pgompertz = function(q, mu, sigma){VaRES::pgompertz(q, mu, sigma)}
pgenlogis = function(q, a, mu, sigma){VaRES::pgenlogis(q, a, mu, sigma)}

all_fits = list(
  fit_exponential = fitdist(x, "exp"),
  fit_normal = fitdist(x, "norm"),
  fit_cauchy = fitdist(x, "cauchy"),
  fit_logistic = fitdist(x, "logistic", start = list(mu = mean(x), sigma = 0.1)),
  fit_laplace = fitdist(x, "dexp", start = list(location = mean(x), scale = 0.1)), #AKA Laplace distribution
  fit_t = fitdist(x, "t", start = list(df = 1, ncp = 0)),
  fit_weibull = fitdist(x, "weibull"),
  fit_gamma = fitdist(x, "gamma"),
  fit_gumbel = fitdist(x, "gumbel", start = list(mu = mean(x), sigma = 0.1)),
  fit_gompertz = fitdist(x, "gompertz", start = list(b = 1, eta = 1)),
  fit_frechet = fitdist(x, "frechet", start = list(alpha = 1, sigma = 1)),
  fit_genlogis = fitdist(x, "genlogis", start = list(a = .02, mu = .01, sigma = .001))
)

round(t(t(sort(unlist(Map(function(obj){obj$aic}, all_fits))))),1)

50 * log(2*pi * 13.475) + 49*13.75/13.475 + 4

set.seed(1)
n = 5
x = round(rnorm(n, 100, 15), 1)
x
mean(x)
var(x) * (n-1) / n

round(7/87 + 1.96*sqrt(7*80/87^3) * c(-1,1),3)
