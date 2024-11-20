pacman::p_load(ggplot2, xtable)

####problem 1

set.seed(1)
m = 1000
FWER = 0.05
digits = 6
num_to_show = 150
sorted_pvals = sort(c(
  rbeta(0.05 * m, 0.1, 1),
  rbeta(0.95 * m, 2, 1)
))
round(head(sorted_pvals, num_to_show), digits)

alpha_bonf = FWER / m

linear_step_up_vals = FWER * (1 : m) / m
max(which(sorted_pvals < linear_step_up_vals, num_to_show))
cbind(round(head(sorted_pvals, 30), digits), round(head(linear_step_up_vals, 30), digits))

####problem 2

set.seed(1) 
n_1 = 45
n_2 = 57

x_1 = rnorm(n_1, 10000, 1000) 
x_2 = rnorm(n_2, 11000, 800) 

ggplot(data.frame()) +
  stat_ecdf(data = data.frame(x_1 = x_1), aes(x = x_1), geom = "step", color = "blue", size = 1.2) + 
  stat_ecdf(data = data.frame(x_2 = x_2), aes(x = x_2), geom = "step", color = "red", size = 1.2) +  
  labs(x = "retailers' spending", y = "empirical CDF")


####problem 3

set.seed(1) 
n = 10
theta = 3.14
x = round(rexp(n, 1/theta) * sample(c(1,-1), n, replace = TRUE), 2)
paste(x, collapse = ", ")
paste(abs(x), collapse = " + ")
sum(abs(x))
mean(abs(x))

round(2.645 + 1.96 * 2.645 / sqrt(10) * c(-1,1), 3)
round(2 + 1.96 * 2 / sqrt(10) * c(-1,1), 3)
(-5 + 26.45/4) / sqrt(10/4)
2 * (-10 * log(2.645) - 10 + 10 * log(2) + 26.45 / 2)


####problem 4

17+53+28+47
70/144
74/144
45/144
99/144
Os = rbind(c(
  17,
  53
),c(
  28,
  46
))
Es = rbind(c(
  70 * 45/144,
  70 * 99/144 
),c(
  74 * 45/144,
  74 * 99/144
))

sum((Os - Es)^2 / Es)



#### problem 5
xtable(round(rbind(1:13, qt(.975, 1:13)), 2))

set.seed(1)
n = 11
survs = rweibull(n, 0.2, 2.3)
mean(survs)
sd(survs)

round(40.3 + 2.23 * 114.6 / sqrt(n) * c(-1,1), 2)
round(log(40.3) + 1.96*114.6/(40.3 * sqrt(n)) * c(-1,1), 2)
