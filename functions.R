#### DATA GENERATION ####

#simulate a set of items
item_sim <- function(n_items, b_mean, b_sd, a_min, a_max){
  item_param <- matrix(NA, nrow = n_items, ncol = 2)
  colnames(item_param) <- c("b_param", "a_param")
  
  item_param[, "b_param"] <- rnorm(nrow(item_param), b_mean, b_sd)
  #change to draw uniform distribution .5, 3.5
  item_param[, "a_param"] <- runif(nrow(item_param), a_min, a_max)
  
  return(item_param)
}

#simulate a set of people's ability scores
two_yr_ability_sim <- function(N_people, theta_mean, theta_sd,
                        N_groups, groupsize_min, groupsize_max,
                        mean_increase, yr_corr, n_cheat, cheat_eff){
  #df setup
  yr1_ability_scores <- data.frame(matrix(NA, nrow = N_people, ncol = 3))
  yr2_ability_scores <- data.frame(matrix(NA, nrow = N_people, ncol = 3))
  
  colnames(yr1_ability_scores) <- c("theta", "group")
  colnames(yr2_ability_scores) <- c("theta", "group")
  
  #group sim
  group_init <- 1
  while(sum(group_init) != N_people){
    group_init <- round(rtruncnorm(n = N_groups, a = groupsize_min, 
                                   b = groupsize_max, 
                                   mean = 20, sd = 5))
  }
  
  yr1_ability_scores[,"group"] <- group_init
  
  yr2_ability_scores[,"group"] <- group_init[sample(length(group_init), length(group_init))]
  
  cheat_groups <- c((N_groups - n_cheat):N_groups)
  
  #ability sim
  yr1_ability_scores[,"ability"] <- rnorm(N_people, theta_mean, theta_sd)
  
  yr1_input <- yr1_ability_scores[,"ability"] * yr_corr
  overall_increase <- rnorm(N_people, mean_increase, 0.01)
  
  yr2_ability_scores[,"ability"] <- yr1_input + overall_increase + 
    rnorm(N_people, 1, 0)
  
  return(list(yr1_ability_scores, yr2_ability_scores))
}

#get the responses for a single item
response_sim <- function(person_vec, item_vec){
  guts <- item_vec["a_param"]*(person_vec["theta"]-item_vec["b_param"])
  prob <- exp(guts)/(1+exp(guts))
  ifelse(runif(1, 0, 1) <= prob, return(1), return(0)) 
}

#get responses for a single person to a set of items
person_sim <- function(person_vec, item_param = item_param){
  responses_vec <- matrix(NA, nrow=nrow(item_param))
  for(i in 1:nrow(item_param)){
    responses_vec[i] <- response_sim(person_vec, item_param[i,])
  }
  return(responses_vec)
}

#get responses for a set of people to a set of items
one_dataset <- function(person_param, item_param){
  responses <- matrix(NA, nrow = nrow(person_param), ncol = nrow(item_param))
  for(i in 1:nrow(person_param)){
    responses[i,] <- person_sim(person_param[i,], item_param)
  }
  #colnames(responses) <- paste0("V", 1:nrow(item_param))
  return(responses)
}


#### LONG FORMAT RESTRUCTURING ####
long_format <- function(data = dataset, group_data = group){
  #prep for reformatting
  data <- as.data.frame(data)
  names(data) <- paste0("Item", 1:ncol(data))
  data$respondentid <- c(1:nrow(data))
  
  #move to long format
  dataset_long <- gather(data, key = respondentid, value = response)
  names(dataset_long)[2] <- "itemid"
  
  #joining group
  group_data <- as.data.frame(group_data)
  group_data$respondentid <- c(1:nrow(group_data))
  dataset_long <- left_join(dataset_long, group_data, by = "respondentid")
  
  dataset_long$itemid <- gsub("Item", "", dataset_long$itemid)
  
  names(dataset_long) <- c("respondentid", "itemid", "response", "group")
  
  return(dataset_long)
}
