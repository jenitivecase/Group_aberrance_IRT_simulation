
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
