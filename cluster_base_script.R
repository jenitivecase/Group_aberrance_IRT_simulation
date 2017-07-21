library(tidyverse)
library(rstan)

options(scipen = 999)
options(stringsAsFactors = FALSE)

work_dir <- getwd()
date <- format.Date(Sys.Date(), "%Y%m%d")

source("cluster_functions.R")

#### COMMAND LINE ARGUMENT SETUP ####
comm_args <- commandArgs(trailingOnly = TRUE)

args <- strsplit(comm_args,"=",fixed=TRUE)

for (arg in 1:length(args)){
  argname <- args[[arg]][1]
  argval <- as.numeric(args[[arg]][2])
  assign(argname,argval)
}

#### PARAM SETUP ####
n_reps <- 5

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
n_cheat <- n_groups*(pct_cheat/100)


#### DATA SAVE SETUP ####
true_params <- vector("list", nreps)
result_objs <- vector("list", nreps)
fit_summaries <- vector("list", nreps)

#setup output folder for use later
folder_name <- paste0(date, "_simulation-results")
file_tag <- paste0(nreps, "reps_", 
                   gsub(".", "-", as.character(cheat_mean), fixed = TRUE), "cheat-mean_", 
                   gsub(".", "-", as.character(n_cheat), fixed = TRUE), "n_cheat_", 
                   date)

if(!dir.exists(paste0(work_dir, "/", folder_name))){
  dir.create(paste0(work_dir, "/", folder_name))
}
setwd(paste0(work_dir, "/", folder_name))

for(i in 1:nreps){
  # generate parameters
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
  
  true_params[[i]] <- list(student_info, item_info)
  names(true_params[[i]]) <- c("student_info", "item_info")
  
  precomp <- stanc(file = "aberrance.stan")
  precomp_model <- stan_model(stanc_ret = precomp)
  
  analysis <- sampling(precomp_model, data = stan_data,
                       iter = 10000, warmup = 5000, chains = 2, cores = 2)
  
  
  result_objs[[i]] <- analysis
  
  fit_summaries[[i]] <- summary(analysis)
  
  #### SAVE TO DISK ####
  #write all the good stuff out to disk
  saveRDS(true_params, paste0("true_params_", file_tag, ".rds"))
  saveRDS(result_objs, paste0("result_objs_", file_tag, ".rds"))
  saveRDS(fit_summaries, paste0("fit_summaries_", file_tag, ".rds"))
}  