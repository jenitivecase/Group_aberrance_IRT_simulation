#### SETUP ####
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(truncnorm)
library(dplyr)
library(tidyr)
library(mirt)

N_people <- 6000
N_groups <- 300
groupsize_min <- 5
groupsize_max <- 35
n_items <- 60
mean_increase <- 0.5
yr_corr <- 0.6
cheat_eff <- 0.5
n_cheat <- N_groups * 0.05

items <- item_sim(n_items, b_mean = 0, b_sd = 1,
                  a_min = 0.5, a_max = 3)

people <- ability_sim(N_people, theta_mean=0, theta_sd=1,
                    N_groups, groupsize_min, groupsize_max, 
                    mean_increase, yr_corr, cheat_eff)

dataset <- one_dataset(person_param = people, item_param = items)
