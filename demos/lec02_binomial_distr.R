pacman::p_load(ggplot2)

theta = .524
n = 28
t_hathat = 0 : n
theta_hathat = t / n
pmf_theta = dbinom(t, n, theta)
left_tail_probs = pbinom(t, n, theta)
right_tail_probs = 1 - pbinom(t - 1, n, theta)

info = data.frame(
  t_hathat = as.factor(t_hathat), 
  theta_hathat = as.factor(round(theta_hathat, 3)), 
  pmf_theta,
  left_tail_probs,
  right_tail_probs,
  rejection_region = theta_hathat <= 0.3219 | theta_hathat >= 0.714
)
sum(info$pmf_theta[info$rejection_region])

ggplot(info) + 
  geom_bar(aes(x = t_hathat, y = pmf_theta), stat = "identity")
ggplot(info) + 
  geom_bar(aes(x = theta_hathat, y = pmf_theta), stat = "identity")
ggplot(info) + 
  geom_bar(aes(x = theta_hathat, y = left_tail_probs), stat = "identity") +
  geom_hline(yintercept = 0.025, col = "red")
ggplot(info) + 
  geom_bar(aes(x = theta_hathat, y = right_tail_probs), stat = "identity") +
  geom_hline(yintercept = 0.025, col = "red")
ggplot(info) + 
  geom_bar(aes(x = theta_hathat, y = pmf_theta, fill = rejection_region), stat = "identity")
