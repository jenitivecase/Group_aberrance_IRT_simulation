#### DATA GENERATION ####

#simulate a set of items
item_sim <- function(n_items, n_DIF, b_mean, b_sd, a_min, a_max, 
                     nodif_mean, nodif_sd, dif_mean, dif_sd){
  item_param <- matrix(NA, nrow = n_items, ncol = 3)
  colnames(item_param) <- c("b_param", "a_param", "dif_param")
  
  item_param[, "b_param"] <- rnorm(nrow(item_param), b_mean, b_sd)
  #change to draw uniform distribution .5, 3.5
  item_param[, "a_param"] <- runif(nrow(item_param), a_min, a_max)
  
  if(n_DIF > 0){
    noDIF_rows <- c(1:(nrow(item_param)-n_DIF))
    DIF_rows <- c((nrow(item_param)-n_DIF+1):nrow(item_param))
    item_param[noDIF_rows, "dif_param"] <- rnorm(length(noDIF_rows), 
                                                 nodif_mean, nodif_sd)
    item_param[DIF_rows, "dif_param"] <- rnorm(length(DIF_rows), dif_mean, dif_sd)
  } else {
    item_param[, "dif_param"] <- rnorm(nrow(item_param), nodif_mean, nodif_sd)
  }
  
  return(item_param)
}

#simulate a set of people's ability scores
ability_sim <- function(N_people, P_REF, ref_theta_mean, ref_theta_sd, 
                        focal_theta_mean, focal_theta_sd){
  ability_scores <- matrix(NA, nrow = N_people, ncol = 2)
  colnames(ability_scores) <- c("theta", "group")
  ref_cutoff <- nrow(ability_scores)*P_REF
  ref_rows <- c(1:ref_cutoff)
  focal_rows <- c((ref_cutoff+1):nrow(ability_scores))
  
  ability_scores[ref_rows, "theta"] <- rnorm(length(ref_rows), 
                                             ref_theta_mean, ref_theta_sd)
  ability_scores[ref_rows, "group"] <- 0
  
  ability_scores[focal_rows, "theta"] <- rnorm(length(focal_rows), 
                                               focal_theta_mean, focal_theta_sd)
  ability_scores[focal_rows, "group"] <- 1
  
  return(ability_scores)
}

#get the responses for a single item
response_sim <- function(person_vec, item_vec){
  guts <- item_vec["a_param"]*(person_vec["theta"]-
                                 (item_vec["b_param"]+item_vec["dif_param"]*person_vec["group"]))
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
