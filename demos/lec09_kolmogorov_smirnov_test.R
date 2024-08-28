pacman::p_load(ggplot2)

set.seed(1)
n = 50

#H_0: iid N(0, 1)

#assume H_0 true so let data come from same DGP
x = rnorm(n, mean = 0, sd = 1)
x_min = min(x)
x_max = max(x)
range_min = x_min - (x_max - x_min) / 2
range_max = x_max + (x_max - x_min) / 2

x_RES = 0.01
x_grid = seq(from = range_min, to = range_max, by = x_RES)
Fhathat_x = ecdf(x)(x_grid)
F_H_0_x = pnorm(x_grid)
abs_diff = abs(F_H_0_x - Fhathat_x)
illustration_data = data.frame(
  x = x_grid, 
  F_H_0_x = F_H_0_x, 
  Fhathat_x = Fhathat_x, 
  abs_diff = abs_diff)
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = F_H_0_x), color = "red") + 
  geom_point(aes(x = x, y = Fhathat_x), color = "blue")
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = F_H_0_x), color = "red") + 
  geom_point(aes(x = x, y = Fhathat_x), color = "blue") + 
  geom_point(aes(x = x, y = abs_diff), color = "yellow")

d_n_hat_hat = max(abs_diff)
d_n_hat_hat
k_hat_hat = sqrt(n) * d_n_hat_hat
k_hat_hat

####critical values of the Kolmogorov distribution
# > K_95
# [1] 1.359
# > K_99
# [1] 1.628

### --> RETAIN H_0 --> correct decision


#assume H_0 not true... so let data come from different DGP
#how about iid N(0, 3^2)
#NOTE: no difference in mean between the real DGP and the null DGP
#we cannot use any tools from class before K-S test to find a difference here!
set.seed(1)
x = rnorm(n, mean = 0, sd = 3)
x_min = min(x)
x_max = max(x)
range_min = x_min - (x_max - x_min) / 2
range_max = x_max + (x_max - x_min) / 2

x_RES = 0.01
x_grid = seq(from = range_min, to = range_max, by = x_RES)
Fhathat_x = ecdf(x)(x_grid)
F_H_0_x = pnorm(x_grid)
abs_diff = abs(F_H_0_x - Fhathat_x)
abs_diff = abs(F_H_0_x - Fhathat_x)
illustration_data = data.frame(
  x = x_grid, 
  F_H_0_x = F_H_0_x, 
  Fhathat_x = Fhathat_x, 
  abs_diff = abs_diff)
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = F_H_0_x), color = "red") + 
  geom_point(aes(x = x, y = Fhathat_x), color = "blue")
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = F_H_0_x), color = "red") + 
  geom_point(aes(x = x, y = Fhathat_x), color = "blue") + 
  geom_point(aes(x = x, y = abs_diff), color = "orange")

d_n_hat_hat = max(abs_diff)
d_n_hat_hat
k_hat_hat = sqrt(n) * d_n_hat_hat
k_hat_hat

####critical values of the Kolmogorov distribution
# > K_95
# [1] 1.359
# > K_99
# [1] 1.628

### --> REJECT H_0 --> correct decision

#assume H_0 not true... so let data come from different DGP
#how about iid N(0.1 1)
set.seed(1)
x = rnorm(n, mean = 0.1, sd = 1)
x_min = min(x)
x_max = max(x)
range_min = x_min - (x_max - x_min) / 2
range_max = x_max + (x_max - x_min) / 2

x_RES = 0.01
x_grid = seq(from = range_min, to = range_max, by = x_RES)
Fhathat_x = ecdf(x)(x_grid)
F_H_0_x = pnorm(x_grid)
abs_diff = abs(F_H_0_x - Fhathat_x)

illustration_data = data.frame(
  x = x_grid, 
  F_H_0_x = F_H_0_x, 
  Fhathat_x = Fhathat_x, 
  abs_diff = abs_diff)
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = F_H_0_x), color = "red") + 
  geom_point(aes(x = x, y = Fhathat_x), color = "blue")
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = F_H_0_x), color = "red") + 
  geom_point(aes(x = x, y = Fhathat_x), color = "blue") + 
  geom_point(aes(x = x, y = abs_diff), color = "orange")

d_n_hat_hat = max(abs_diff)
d_n_hat_hat
k_hat_hat = sqrt(n) * d_n_hat_hat
k_hat_hat

####critical values of the Kolmogorov distribution
# > K_95
# [1] 1.359
# > K_99
# [1] 1.628

### --> RETAIN H_0 --> Type II error --> not enough power to detect the difference!!

#But this can be a simple z-test of the mean i.e. H_0: mu = theta = 0 
#where sigsq = 1 and known
#yes, there are always many tests to choose from
#who has higher power at alpha = 5%? K-S vs z-test
  
set.seed(1)
Nsim = 1e4
ks_test_reject = array(NA, Nsim)
ztest_reject = array(NA, Nsim)
for (nsim in 1 : Nsim){
  x = rnorm(n, mean = 0.1, sd = 1)
  x_min = min(x)
  x_max = max(x)
  range_min = x_min - (x_max - x_min) / 2
  range_max = x_max + (x_max - x_min) / 2
  
  x_grid = seq(from = range_min, to = range_max, by = x_RES)
  Fhathat_x = ecdf(x)(x_grid)
  F_H_0_x = pnorm(x_grid)
  abs_diff = abs(F_H_0_x - Fhathat_x)
  
  d_n_hat_hat = max(abs_diff)
  k_hat_hat = sqrt(n) * d_n_hat_hat
  ks_test_reject[nsim] = k_hat_hat > 1.359
  z_hat_hat = (mean(x) - 0) / (1 / sqrt(n))
  ztest_reject[nsim] = z_hat_hat > 1.96
}
mean(ks_test_reject)
mean(ztest_reject)

#why did this happen? The K-S test is much more general! z-test is only for mean differences
#among normal dgp's


###### two sample K-S tests
set.seed(1)
rm(list = ls())

n_1 = 50
n_2 = 50
#Let H_0 be true: i.e. both populations are the same DGP's
x_1 = rnorm(n_1, mean = 0, sd = 1)
x_2 = rnorm(n_2, mean = 0, sd = 1)
x_min = min(c(x_1, x_2))
x_max = max(c(x_1, x_2))
range_min = x_min - (x_max - x_min) / 2
range_max = x_max + (x_max - x_min) / 2

x_RES = 0.01
x_grid = seq(from = range_min, to = range_max, by = x_RES)
Fhathat_x_1 = ecdf(x_1)(x_grid)
Fhathat_x_2 = ecdf(x_2)(x_grid)
abs_diff = abs(Fhathat_x_1 - Fhathat_x_2)

illustration_data = data.frame(
  x = x_grid, 
  Fhathat_x_1 = Fhathat_x_1, 
  Fhathat_x_2 = Fhathat_x_2, 
  abs_diff = abs_diff)
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = Fhathat_x_1), color = "green") + 
  geom_point(aes(x = x, y = Fhathat_x_2), color = "blue")
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = Fhathat_x_1), color = "green") + 
  geom_point(aes(x = x, y = Fhathat_x_2), color = "blue") + 
  geom_point(aes(x = x, y = abs_diff), color = "orange")


d_n_1_n_2_hat_hat = max(abs_diff)
d_n_1_n_2_hat_hat
k_hat_hat = sqrt(n_1 * n_2 / (n_1 + n_2)) * d_n_1_n_2_hat_hat
k_hat_hat

####critical values of the Kolmogorov distribution
# > K_95
# [1] 1.359
# > K_99
# [1] 1.628

### --> RETAIN H_0 --> correct


#Let H_0 be false: i.e. population 1 DGP is not the same as population 2 DGP
set.seed(1)
rm(list = ls())

n_1 = 50
n_2 = 50
x_1 = rnorm(n_1, mean = 0, sd = 1)
x_2 = rnorm(n_2, mean = 0, sd = 2)
x_min = min(c(x_1, x_2))
x_max = max(c(x_1, x_2))
range_min = x_min - (x_max - x_min) / 2
range_max = x_max + (x_max - x_min) / 2

x_RES = 0.01
x_grid = seq(from = range_min, to = range_max, by = x_RES)
Fhathat_x_1 = ecdf(x_1)(x_grid)
Fhathat_x_2 = ecdf(x_2)(x_grid)
abs_diff = abs(Fhathat_x_1 - Fhathat_x_2)

illustration_data = data.frame(
  x = x_grid, 
  Fhathat_x_1 = Fhathat_x_1, 
  Fhathat_x_2 = Fhathat_x_2, 
  abs_diff = abs_diff)
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = Fhathat_x_1), color = "green") + 
  geom_point(aes(x = x, y = Fhathat_x_2), color = "blue")
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = Fhathat_x_1), color = "green") + 
  geom_point(aes(x = x, y = Fhathat_x_2), color = "blue") + 
  geom_point(aes(x = x, y = abs_diff), color = "orange")


d_n_1_n_2_hat_hat = max(abs_diff)
d_n_1_n_2_hat_hat
k_hat_hat = sqrt(n_1 * n_2 / (n_1 + n_2)) * d_n_1_n_2_hat_hat
k_hat_hat

####critical values of the Kolmogorov distribution
# > K_95
# [1] 1.359
# > K_99
# [1] 1.628

### --> RETAIN H_0 --> Type II error => not enough power


#Let H_0 be false: i.e. population 1 DGP is not the same as population 2 DGP
set.seed(1)
rm(list = ls())

n_1 = 50
n_2 = 50
x_1 = rnorm(n_1, mean = 0, sd = 1)
x_2 = rnorm(n_2, mean = 0, sd = 3)
x_min = min(c(x_1, x_2))
x_max = max(c(x_1, x_2))
range_min = x_min - (x_max - x_min) / 2
range_max = x_max + (x_max - x_min) / 2

x_RES = 0.01
x_grid = seq(from = range_min, to = range_max, by = x_RES)
Fhathat_x_1 = ecdf(x_1)(x_grid)
Fhathat_x_2 = ecdf(x_2)(x_grid)
abs_diff = abs(Fhathat_x_1 - Fhathat_x_2)

illustration_data = data.frame(
  x = x_grid, 
  Fhathat_x_1 = Fhathat_x_1, 
  Fhathat_x_2 = Fhathat_x_2, 
  abs_diff = abs_diff)
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = Fhathat_x_1), color = "green") + 
  geom_point(aes(x = x, y = Fhathat_x_2), color = "blue")
ggplot(illustration_data) + 
  geom_point(aes(x = x, y = Fhathat_x_1), color = "green") + 
  geom_point(aes(x = x, y = Fhathat_x_2), color = "blue") + 
  geom_point(aes(x = x, y = abs_diff), color = "orange")


d_n_1_n_2_hat_hat = max(abs_diff)
d_n_1_n_2_hat_hat
k_hat_hat = sqrt(n_1 * n_2 / (n_1 + n_2)) * d_n_1_n_2_hat_hat
k_hat_hat


####critical values of the Kolmogorov distribution
# > K_95
# [1] 1.359
# > K_99
# [1] 1.628

### --> REJECT H_0 --> correct
