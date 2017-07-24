### Plot results ---------------------------------------------------------------

setwd("C:/Users/jennifer.brussow/Documents/group_aberr_results_20170724")
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(tidyverse)
library(rstan)

fit_files <- grep("fit", list.files(), value = TRUE)
true_files <- grep("true", list.files(), value = TRUE)


student_info <- readRDS("student_info.rds")
item_info <- readRDS("item_info.rds")
analysis <- readRDS("stan_model.rds")

theta_corr <- 0.7
n_items <- 30
n_anchor <- 10

fit_summary <- summary(analysis)

rhat <- fit_summary$summary %>%
  as.data.frame() %>%
  mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>%
  ggplot(aes(x = Parameter, y = Rhat, color = Parameter)) +
  geom_jitter(height = 0, width = 0.4, show.legend = FALSE) +
  geom_hline(aes(yintercept = 1.1), linetype = "dashed") +
  labs(y = expression(hat(italic(R))), title = "Convergence") +
  theme_bw()

true_group <- student_info$groups %>%
  mutate(true_effect = group_inc + cheat_eff)

gr_summary <- summary(analysis, pars = "group_inc")$summary %>%
  as_data_frame() %>%
  pull(mean)

perfect <- data_frame(x = c(-100, 100), y = c(-100, 100))

true_group <- true_group %>%
  select(group, true_effect) %>%
  mutate(estimate = gr_summary)

group_inc <- ggplot(true_group, aes(x = true_effect, y = estimate)) +
  geom_point() +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(0, 2, 0.25)) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(0, 2, 0.25)) +
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 1.5)) +
  labs(title = "Group increase recovery") +
  theme_bw()

est_cor <- summary(analysis, pars = c("corr"))$summary %>%
  as_data_frame() %>%
  pull(mean)

corr <- data_frame(true = theta_corr, estimate = est_cor) %>%
  ggplot(aes(x = true, y = estimate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(title = "Correlation recovery") +
  theme_bw()

est_a <- summary(analysis, pars = c("a"))$summary %>%
  as_data_frame() %>%
  pull(mean)
est_b <- summary(analysis, pars = c("b"))$summary %>%
  as_data_frame() %>%
  pull(mean)

items <- bind_rows(item_info$items1, item_info$items2) %>%
  distinct() %>%
  arrange(item_id) %>%
  mutate(type = c(rep("Year 1", n_items - n_anchor), rep("Anchor", n_anchor),
                  rep("Year 2", n_items - n_anchor))) %>%
  mutate(b_est = est_b, a_est = est_a)

b_recovery <- ggplot(items, aes(x = b_param, y = b_est, color = type)) +
  geom_point() +
  scale_x_continuous(limits = c(-1.5, 2.5), breaks = seq(-3, 3, 0.5)) +
  scale_y_continuous(limits = c(-1.5, 2.5), breaks = seq(-3, 3, 0.5)) +
  labs(title = "b-parameter recovery", x = "True", y = "Estimated") +
  scale_color_discrete(name = "Item Type") +
  theme_bw() +
  theme(legend.position = "bottom")

a_recovery <- ggplot(items, aes(x = a_param, y = a_est, color = type)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 4, 0.5)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 4, 0.5)) +
  labs(title = "a-parameter recovery", x = "True", y = "Estimated") +
  scale_color_discrete(name = "Item Type") +
  theme_bw() +
  theme(legend.position = "bottom")

theta1_est <- summary(analysis, pars = c("theta1"))$summary %>%
  as_data_frame() %>%
  pull(mean)
theta2_est <- summary(analysis, pars = c("theta2"))$summary %>%
  as_data_frame() %>%
  pull(mean)

theta <- student_info$people %>%
  arrange(id) %>%
  select(-responses) %>%
  mutate(est1 = theta1_est, est2 = theta2_est)

theta_recovery <- ggplot(theta) +
  geom_point(aes(x = theta1, y = est1, color = "Year 1")) +
  geom_point(aes(x = theta2, y = est2, color = "Year 2")) +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  scale_y_continuous(breaks = seq(-4, 4, 1)) +
  labs(x = "True", y = "Estimated", title = "Theta recovery") +
  scale_color_discrete(name = "Year") +
  theme_bw() +
  theme(legend.position = "bottom")

all_plots <- list(rhat, group_inc, corr, b_recovery, a_recovery, theta_recovery)

pdf("model-results.pdf")
walk(all_plots, print)
dev.off()
