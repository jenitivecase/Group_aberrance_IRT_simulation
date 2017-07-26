### Plot results ---------------------------------------------------------------

setwd("C:/Users/jennifer.brussow/Documents/group_aberr_results_20170724")
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(tidyverse)
library(rstan)
library(ggplot2)

fit_files <- grep("fit", list.files(), value = TRUE)
true_files <- grep("true", list.files(), value = TRUE)

for(file in 1:length(fit_files)){
  
  #### SETUP ####
  info <- unlist(strsplit(as.character(fit_files[file]), "_"))
  
  fit_summary <- readRDS(fit_files[file])
  
  ##kludgy fix to remove final failed NULL list value
  fit_summary <- fit_summary[1:4]
  
  fit_summary <- lapply(fit_summary, FUN = function(x) as.data.frame(x$summary))
  fit_summary <- do.call("rbind", fit_summary)
  
  true_info <- readRDS(true_files[file])
  
  ##kludgy fix to remove final failed NULL list value
  true_info <- true_info[1:4]
  
  student_info <- lapply(true_info, FUN = function(x) x <- x$student_info)
  
  item_info <- lapply(true_info, FUN = function(x) x <- x$item_info)
  


  
  #fixed parameters
  n_items <- 60
  n_anchor <- 20
  b1_mean <- 0
  b2_mean <- 0.5
  b_sd <- 0.7
  a_min <- 0.5
  a_max <- 3.5
  
  n_people <- 6000
  n_groups <- 300
  group_min <- 5
  group_max <- 35
  theta_mean <- 0
  theta_sd <- 1
  theta_increase <- 0.5
  theta_corr <- 0.7

  #varying parameters  
  cheat_mean <- gsub("cheat-mean", "", info[4])
  cheat_mean <- as.numeric(gsub("-", ".", cheat_mean))
  
  n_cheat <- as.numeric(gsub("n", "", info[5]))
  pct_cheat <- n_cheat/n_groups *100
  
  ### MAKING GRAPHS ###
  
  tag <- paste0("For ", cheat_mean, " cheating effect and ", pct_cheat, "% of groups cheating")
  
  rhat <- fit_summary %>%
    mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>%
    ggplot(aes(x = Parameter, y = Rhat, color = Parameter)) +
    geom_jitter(height = 0, width = 0.4, show.legend = FALSE) +
    geom_hline(aes(yintercept = 1.1), linetype = "dashed") +
    labs(y = expression(hat(italic(R))), title = "Convergence",
        subtitle = tag) +
    theme_bw() 
  
  
  groups <- lapply(student_info, FUN = function(x) as.data.frame(x$groups)) 
  groups <- do.call(rbind, groups)
  
  true_group <- groups %>%
    mutate(true_effect = group_inc + cheat_eff)
  
  gr_summary <- fit_summary[grep("group_inc", rownames(fit_summary)),] %>%
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
    labs(title = "Group increase recovery",
         subtitle = tag) +
    theme_bw()
  
  est_cor <- fit_summary[grep("corr", rownames(fit_summary)), "mean"] 
    
  corr <- data_frame(true = rep(theta_corr, length(est_cor)), estimate = est_cor) %>%
    ggplot(aes(x = true, y = estimate)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(title = "Correlation recovery",
         subtitle = tag) +
    theme_bw()
  

  est_a <- fit_summary[grep("^a", rownames(fit_summary)),] %>%
    as_data_frame() %>%
    pull(mean)
  
  est_b <- fit_summary[grep("^b", rownames(fit_summary)),] %>%
    as_data_frame() %>%
    pull(mean)
  
  items <- lapply(item_info, FUN = function(x){
    x <- bind_rows(list(x$items1, x$items2)) %>%
      distinct() %>%
      arrange(item_id) %>%
      mutate(type = c(rep("Year 1", n_items - n_anchor), rep("Anchor", n_anchor),
                      rep("Year 2", n_items - n_anchor))) 
  })
  
  items <- bind_rows(items) %>%
    mutate(b_est = est_b, a_est = est_a)
    
  b_recovery <- ggplot(items, aes(x = b_param, y = b_est, color = type)) +
    geom_point() +
    scale_x_continuous(limits = c(-2.0, 3.0), breaks = seq(-3, 3, 0.5)) +
    scale_y_continuous(limits = c(-2.0, 3.0), breaks = seq(-3, 3, 0.5)) +
    labs(title = "b-parameter recovery", x = "True", y = "Estimated",
         subtitle = tag) +
    scale_color_discrete(name = "Item Type") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  a_recovery <- ggplot(items, aes(x = a_param, y = a_est, color = type)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 4, 0.5)) +
    scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 4, 0.5)) +
    labs(title = "a-parameter recovery", x = "True", y = "Estimated",
         subtitle = tag) +
    scale_color_discrete(name = "Item Type") +
    theme_bw() +
    theme(legend.position = "bottom")
  

  theta1_est <- fit_summary[grep("^theta1", rownames(fit_summary)),] %>%
    as_data_frame() %>%
    pull(mean)
  theta2_est <- fit_summary[grep("^theta2", rownames(fit_summary)),] %>%
    as_data_frame() %>%
    pull(mean)
  
  people <- lapply(student_info, FUN = function(x) x <- x$people)
  
  theta <- lapply(people, FUN = function(x){
    x <- x %>%
      arrange(id) %>%
      select(-responses) 
  })
  theta <- theta %>%
    bind_rows() %>%
    mutate(est1 = theta1_est, est2 = theta2_est)
  
  
  theta_recovery <- ggplot(theta) +
    geom_point(aes(x = theta1, y = est1, color = "Year 1")) +
    geom_point(aes(x = theta2, y = est2, color = "Year 2")) +
    scale_x_continuous(breaks = seq(-4, 4, 1)) +
    scale_y_continuous(breaks = seq(-4, 4, 1)) +
    labs(x = "True", y = "Estimated", title = "Theta recovery",
         subtitle = tag) +
    scale_color_discrete(name = "Year") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  all_plots <- list(rhat, group_inc, corr, b_recovery, a_recovery, theta_recovery)
  
  pdf(paste0("model-results_cheat-inc", cheat_mean, "_pct-cheat", pct_cheat, "_", date, ".pdf"))
  walk(all_plots, print)
  dev.off()
}