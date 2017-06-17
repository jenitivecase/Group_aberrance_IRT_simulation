### Setup ----------------------------------------------------------------------
options(
  repos = c("https://cran.rstudio.com/", "http://rweb.crmda.ku.edu/kran"),
  scipen = 999
)

needed_packages <- c("tidyverse", "rstan")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)


### Define Functions -----------------------------------------------------------
logit <- function(x) {
  log(x / (1 - x))
}
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
mvrnorm <- function(n, mu, sigma) {
  ncols <- ncol(sigma)
  mu <- rep(mu, each = n)
  mu + matrix(rnorm(n = ncols * n), ncol = ncols) %*% chol(sigma)
}
gen_people <- function(n_people, n_groups, group_min, group_max, theta_mean,
  theta_sd, theta_increase, theta_corr, n_cheat, cheat_mean) {
  # check inputs
  if (!(n_people < (group_max * n_groups))) {
    stop("Incompatible sample and group size")
  }
  if (!(n_people > (group_min * n_groups))) {
    stop("Incompatible sample and group size")
  }
  
  # generate people and groups
  people <- data_frame(id = seq_len(n_people), group = 0)
  repeat {
    people <- mutate(people, group = sample(seq_len(n_groups), size = n_people,
      replace = TRUE))
    
    min_count <- people %>%
      group_by(group) %>% summarize(n = n()) %>% pull(n) %>% min()
    max_count <- people %>%
      group_by(group) %>% summarize(n = n()) %>% pull(n) %>% max()
    
    if (min_count >= group_min && max_count <= group_max) break
  }
  
  groups <- data_frame(
    group = seq_len(n_groups),
    group_inc = rnorm(n = n_groups, mean = theta_increase, sd = 0.1)
  )
  
  # generate theta from mvnormal dist. by group
  # mean vec = c(0, group_inc)
  # sigma = diagonal matrix with theta_corr of off diagonals
  people <- people %>%
    left_join(groups, by = "group") %>%
    group_by(group, group_inc) %>%
    nest(.key = students) %>%
    arrange(group) %>%
    mutate(stu_data = map2(students, group_inc, .f = function(x, y) {
        mu <- c(theta_mean, theta_mean + y)
        sigma <- matrix(data = c(theta_sd, theta_corr, theta_corr, theta_sd),
          ncol = 2)
        theta_val <- mvrnorm(n = nrow(x), mu = mu, sigma = sigma) %>%
          as_data_frame() %>%
          select(theta1 = V1, theta2 = V2)
        bind_cols(x, theta_val)
      })) %>%
    select(-students) %>%
    unnest() %>%
    ungroup()
  
  # haven't added cheating yet, as true thetas should be correlated:
  # cor(people$theta1, people$theta2)
  # mean of people$theta1 should be ~= 0
  # mean(people$theta1)
  # mean of people$theta2 should be ~= to weighted average of group$group_inc
  # mean(people$theta2)
  # people %>%
  #   group_by(group) %>%
  #   summarize(n = n(), inc = unique(group_inc)) %>%
  #   mutate(total = n * inc) %>%
  #   pull(total) %>%
  #   sum() %>%
  #   `/`(n_people)
  
  # Now add cheating
  groups <- groups %>%
    mutate(
      cheat = case_when(group %in% seq_len(n_cheat) ~ 1, TRUE ~ 0),
      cheat_eff = rnorm(n = n_groups, mean = cheat_mean, sd = 0.1) * cheat
    )
  
  people <- people %>%
    left_join(select(groups, group, cheat_eff), by = "group") %>%
    mutate(theta2 = theta2 + cheat_eff) %>%
    select(id, group, theta1, theta2)
  
  list(
    people = people,
    groups = groups
  )
}
gen_items <- function(n_items, n_anchor, b1_mean, b2_mean, b_sd, a_min,
  a_max) {
  items1 <- data_frame(
    item_id = seq_len(n_items - n_anchor),
    b_param = rnorm(n = n_items - n_anchor, mean = b1_mean, sd = b_sd),
    a_param = runif(n = n_items - n_anchor, min = a_min, max = a_max)
  )
  
  items2 <- data_frame(
    item_id = seq_len(n_items - n_anchor) + n_items,
    b_param = rnorm(n = n_items - n_anchor, mean = b2_mean, sd = b_sd),
    a_param = runif(n = n_items - n_anchor, min = a_min, max = a_max)
  )
  
  anchor <- data_frame(
    item_id = seq_len(n_anchor) + n_items - n_anchor,
    b_param = rnorm(n = n_anchor, mean = mean(c(b1_mean, b2_mean)), sd = b_sd),
    a_param = runif(n = n_anchor, min = a_min, max = a_max)
  )
  
  list(
    items1 = bind_rows(items1, anchor),
    items2 = bind_rows(anchor, items2)
  )
}


### Run simulations ------------------------------------------------------------
n_people <- 250
n_groups <- 10
group_min <- 5
group_max <- 35
theta_mean <- 0
theta_sd <- 1
theta_increase <- 0.5
theta_corr <- 0.7
n_cheat <- 2
cheat_mean <- 0.5

n_items <- 30
n_anchor <- 10
b1_mean = 0
b2_mean = 0.5
b_sd = 0.7
a_min = 0.5
a_max = 3.5

# generate parameters
set.seed(9416)
student_info <- gen_people(n_people, n_groups, group_min, group_max, theta_mean,
  theta_sd, theta_increase, theta_corr, n_cheat, cheat_mean)
item_info <- gen_items(n_items, n_anchor, b1_mean, b2_mean, b_sd, a_min, a_max)

# generate response data
student_info$people$responses <- map2(.x = student_info$people$theta1,
  .y = student_info$people$theta2, .f = function(x, y, items) {
    year1 <- items$items1 %>%
      mutate(
        prob = inv_logit(a_param * (x - b_param)),
        rand = runif(n = nrow(.), min = 0, max = 1),
        score = case_when(rand < prob ~ 1, TRUE ~ 0),
        year = 1
      ) %>%
      select(year, item_id, score)
    year2 <- items$items2 %>%
      mutate(
        prob = inv_logit(a_param * (y - b_param)),
        rand = runif(n = nrow(.), min = 0, max = 1),
        score = case_when(rand < prob ~ 1, TRUE ~ 0),
        year = 2
      ) %>%
      select(year, item_id, score)
    bind_rows(year1, year2)
  },
  items = item_info)

full_responses <- student_info$people %>%
  select(id, group, responses) %>%
  unnest()

year1 <- filter(full_responses, year == 1)
year2 <- filter(full_responses, year == 2)

# estimate model
stan_data <- list(
  I = (n_items * 2) - n_anchor,
  J = n_people,
  G = n_groups,
  N = nrow(year1),
  jg = student_info$people %>% select(id, group) %>% arrange(id) %>% pull(group),
  ii1 = year1$item_id,
  jj1 = year1$id,
  gg1 = year1$group,
  y1 = year1$score,
  ii2 = year2$item_id,
  jj2 = year2$id,
  gg2 = year2$group,
  y2 = year2$score
)

precomp <- stanc(file = "aberrance.stan")
precomp_model <- stan_model(stanc_ret = precomp)

analysis <- sampling(precomp_model, data = stan_data, seed = 71715,
  iter = 10000, warmup = 5000, chains = 2, cores = 2)

saveRDS(student_info, file = "student_info.rds")
saveRDS(item_info, file = "item_info.rds")
saveRDS(analysis, file = "stan_model.rds")


### Plot results ---------------------------------------------------------------
library(tidyverse)
library(rstan)

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
