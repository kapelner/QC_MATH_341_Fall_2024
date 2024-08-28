library(ggplot2)

n = 21
t = 0 : n
thetahathat = t / n
theta = .524
pmf_theta = dbinom(t, n, theta)
cbind(t, thetahathat, pmf_theta)

ggplot(data.frame(thetahathat = as.factor(round(thetahathat, 4)), pmf_theta = pmf_theta)) + 
  geom_bar(aes(x = thetahathat, y = pmf_theta), stat = "identity")
