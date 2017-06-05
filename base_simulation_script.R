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
