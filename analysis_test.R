### Plot results ---------------------------------------------------------------

setwd("C:/Users/jennifer.brussow/Documents/Group_aberrance_IRT_simulation/combined")
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(tidyverse)
library(rstan)
library(ggplot2)

if(!dir.exists("./results")){ dir.create("./results") }

fit_files <- grep("fit", list.files(), value = TRUE)
true_files <- grep("true", list.files(), value = TRUE)

thresholds <- c(1, 1.5, 2)

for(file in 1:length(fit_files)){
  
  #### SETUP ####
  info <- unlist(strsplit(as.character(fit_files[file]), "_"))
  
  fit_summary <- readRDS(fit_files[file])
  
  fit_summary <- lapply(fit_summary, FUN = function(x) as.data.frame(x$summary))
  fit_summary <- do.call("rbind", fit_summary)
  
  true_info <- readRDS(true_files[file])
  
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
  out_label <- paste0(cheat_mean, "_cheat-mean", pct_cheat, "_pct-cheat_", date)
  
  rhat <- fit_summary %>%
    mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>%
    mutate(Param_type = gsub("[0-9\\_]", "", Parameter)) %>%
    ggplot(aes(x = Parameter, y = Rhat, color = Param_type)) +
    geom_jitter(height = 0, width = 0.4, aes(color = Param_type)) +
    geom_hline(aes(yintercept = 1.1), linetype = "dashed") +
    labs(y = expression(hat(italic(R))), title = "Convergence",
         subtitle = tag, color = "Param Type") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          legend.position = "bottom")
  
  ggsave(paste0("./results/convergence_", out_label, ".png"), plot = rhat, height = 7, width = 7)
  
  
  groups <- lapply(student_info, FUN = function(x) as.data.frame(x$groups)) 
  groups <- do.call(rbind, groups)
  
  for(detect_thresh in thresholds){
    
    out_thresh_label <- paste0(detect_thresh, "detect-thresh_", cheat_mean, "_cheat-mean", pct_cheat, "_pct-cheat_", date)
    
    true_group <- groups %>%
      mutate(true_effect = group_inc + cheat_eff)
    
    gr_summary <- fit_summary[grep("group_inc", rownames(fit_summary)),] %>%
      as_data_frame() %>%
      pull(mean)
    
    perfect <- data_frame(x = c(-100, 100), y = c(-100, 100))
    
    true_group <- true_group %>%
      select(group, cheat, true_effect) %>%
      mutate(estimate = gr_summary) %>%
      mutate(Decision = case_when((cheat == 1 & estimate >= detect_thresh) ~ "Correct Classification",
                                  (cheat == 1 & estimate < detect_thresh) ~ "Incorrect Classification",
                                  (cheat == 0 & estimate < detect_thresh) ~ "Correct Classification",
                                  (cheat == 0 & estimate >= detect_thresh) ~ "Incorrect Classification",
                                  TRUE ~ "NA")) %>%
      mutate(Decision_spec = case_when(cheat == 1 & estimate >= detect_thresh ~ "True Positive", 
                                       cheat == 1 & estimate < detect_thresh ~ "False Negative",
                                       cheat == 0 & estimate < detect_thresh ~ "True Negative", 
                                       cheat == 0 & estimate >= detect_thresh ~ "False Positive",
                                       TRUE ~ "NA"))
    
    N <- nrow(true_group)
    TP_N <- nrow(filter(true_group, Decision_spec == "True Positive"))
    FP_N <- nrow(filter(true_group, Decision_spec == "False Positive"))
    
    print(out_thresh_label)
    print(paste0("False Pos Rate = ", FP_N/N))
    print(paste0("Power = ", TP_N/N))
    print(paste0("Precision = ", TP_N/(TP_N + FP_N)))
    
    group_inc <- ggplot(true_group, aes(x = true_effect, y = estimate, color = Decision)) +
      geom_point() +
      scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.25)) +
      scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.25)) +
      coord_cartesian(xlim = c(0, 2), ylim = c(-.25, 2)) +
      labs(title = "Group increase recovery",
           subtitle = paste0(tag, ", ", detect_thresh, 
                             " effect threshold"), 
           x = "True Effect", y = "Estimated Effect",
           caption = paste0(paste0("False Pos Rate = ", round(FP_N/N, 3)), "; ",
                            paste0("Power = ", round(TP_N/N, 3)), "; ",
                            paste0("Precision = ", round(TP_N/(TP_N + FP_N), 3)))) +
      theme_bw() + 
      scale_color_manual(values = c("forestgreen", "darkred")) + 
      geom_hline(aes(yintercept = detect_thresh)) +
      geom_vline(aes(xintercept = detect_thresh)) +
      theme(legend.position = "bottom") 
    
    ggsave(paste0("./results/groupinc-recovery_", out_thresh_label, ".png"), plot = group_inc, height = 7, width = 7)
    
    
    classifications <- as.data.frame(table(true_group$Decision_spec))
    
    write.csv(classifications, paste0("./results/classification-decisions_", out_thresh_label, ".csv"))
  }
  
  
  est_cor <- fit_summary[grep("corr", rownames(fit_summary)), "mean"] 
  
  corr <- data_frame(true = rep(theta_corr, length(est_cor)), estimate = est_cor) %>%
    ggplot(aes(x = true, y = estimate)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(title = "Correlation recovery",
         subtitle = tag) +
    theme_bw()
  
  ggsave(paste0("./results/corr-recovery_", out_label, ".png"), plot = corr, height = 7, width = 7)
  
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
  
  ggsave(paste0("./results/b-recovery_", out_label, ".png"), plot = b_recovery, height = 7, width = 7)
  
  a_recovery <- ggplot(items, aes(x = a_param, y = a_est, color = type)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 4, 0.5)) +
    scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 4, 0.5)) +
    labs(title = "a-parameter recovery", x = "True", y = "Estimated",
         subtitle = tag) +
    scale_color_discrete(name = "Item Type") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(paste0("./results/a-recovery_", out_label, ".png"), plot = a_recovery, height = 7, width = 7)
  
  
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
  
  ggsave(paste0("./results/theta-recovery_", out_label, ".png"), plot = theta_recovery, height = 7, width = 7)
}


