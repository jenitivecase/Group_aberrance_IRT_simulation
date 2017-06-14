#### SETUP ####
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(truncnorm)
library(dplyr)
library(tidyr)
library(rstan)

source("functions.R")
source("stan_script.R")

N_people <- 6000
N_groups <- 300
groupsize_min <- 5
groupsize_max <- 35
group_sd <- 1
n_items <- 60
mean_increase <- 0.5
yr_corr <- 0.6
cheat_eff <- 0.5
n_cheat <- N_groups * 0.05

# add parameter: length of anchor test
anchor_length <- 10

items_yr1 <- item_sim(n_items, b_mean = 0, b_sd = 1,
                  a_min = 0.5, a_max = 3)

# change b_mean in year2 to be equal to theta mean (0.5 growth)
items_yr2 <- item_sim(n_items, b_mean = 0.5, b_sd = 1,
                      a_min = 0.5, a_max = 3)

# anchor test is defined as the last anchor_length items on year1
anchor_items <- items_yr1 %>%
  top_n(anchor_length, wt = itemid)

# alter year2 items
# remove first anchor_length items, increment itemids so all items across
#   grades have a unique id
# bind on anchor test
items_yr2 <- items_yr2 %>%
  top_n(-(n_items - anchor_length), wt = itemid) %>%
  mutate(itemid = itemid + n_items) %>%
  bind_rows(anchor_items)


people <- two_yr_ability_sim(N_people, theta_mean=0, theta_sd=1,
                             N_groups, groupsize_min, groupsize_max, group_sd,
                             mean_increase, yr_corr, n_cheat, cheat_eff)

responses_yr1 <- apply(items_yr1, 1, FUN = item_response_sim, person_param = people,
                       ability_col = "yr1_ability")
responses_yr1 <- do.call(rbind, responses_yr1)

responses_yr2 <- apply(items_yr2, 1, FUN = item_response_sim, person_param = people,
                       ability_col = "yr2_ability")
responses_yr2 <- do.call(rbind, responses_yr2)

studentid <- responses_yr2[, "studentid"]
groupid <- responses_yr2[, "groupid"]
itemid <- responses_yr2[, "itemid"]
response_yr1 <- responses_yr1[, "response"]
response_yr2 <- responses_yr2[, "response"]
  
n_people <- N_people
n_groups <- N_groups
n_observations <- length(response_yr2)

# recalculate number of unique items for stan
n_items <- nrow(items_yr1) + nrow(items_yr2) - anchor_length

b.dat_long <- list("n_people", "n_items", "n_observations", "n_groups", 
                   "studentid", "groupid", "itemid", 
                   "response_yr1", "response_yr2")


precomp <- stanc(model_code = stancode_long)
precomp_model <- stan_model(stanc_ret = precomp)

#conducting the analysis
analysis <- sampling(precomp_model, data = b.dat_long,
                     iter = 10000, warmup = 5000, chains = 2, verbose = FALSE, 
                     cores = 2)

saveRDS(analysis, "group_aberrance_IRT_test_20170613.rds")
