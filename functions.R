#### DATA GENERATION ####

#simulate a set of items
item_sim <- function(n_items, b_mean, b_sd, a_min, a_max){
  item_param <- data.frame(matrix(NA, nrow = n_items, ncol = 3))
  colnames(item_param) <- c("itemid", "b_param", "a_param")
  item_param$itemid <- c(1:n_items)
  
  item_param$b_param <- rnorm(nrow(item_param), b_mean, b_sd)
  #change to draw uniform distribution .5, 3.5
  item_param$a_param <- runif(nrow(item_param), a_min, a_max)
  
  return(item_param)
}

#simulate group membership for a given number of examinees
group_sim <- function(N_groups, N_people, groupsize_min, groupsize_max,
                      mean_increase){
  group_init <- 1
  while(sum(group_init) != N_people){
    group_init <- round(rtruncnorm(n = N_groups, a = groupsize_min, 
                                   b = groupsize_max, 
                                   mean = 20, sd = 5))
  }
  group_init <- data.frame(matrix(c(group_init, c(1:300)), ncol = 2))
  names(group_init) <- c("size", "id")
  group_init$ability_inc <- rnorm(N_groups, mean_increase, 0.1)
  
  return(group_init)
}

#simulate a set of people's ability scores
two_yr_ability_sim <- function(N_people, theta_mean, theta_sd,
                               N_groups, groupsize_min, groupsize_max, group_sd,
                               mean_increase, yr_corr, n_cheat, cheat_eff){
  person_data <- data.frame(matrix(NA, nrow = N_people, ncol = 4))
  names(person_data) <- c("studentid", "groupid", 
                          "yr1_ability", "yr2_ability")
  person_data$studentid <- c(1:N_people)
  
  person_data$yr1_ability <- rnorm(N_people, theta_mean, theta_sd)
  
  #year 2
  groups <- group_sim(N_groups, N_people, groupsize_min, groupsize_max,
                      mean_increase)
  
  cheat_groups <- c((N_groups - n_cheat):N_groups)
  
  person_data$groupid <- rep(groups$id, groups$size)
  
  for(i in 1:nrow(person_data)){
    group_inc <- rnorm(1, 
                       groups[which(groups$id == person_data[i, "groupid"]), "ability_inc"],
                       0.1)
    cheat_inc <- ifelse(person_data[i, "groupid"] %in% cheat_groups, 
                        rnorm(1, cheat_eff, 0.1), 
                        rnorm(1, 0, 0.1))
    indiv_inc <- rnorm(1, 0, 0.1)
    person_data[i, "yr2_ability"] <- yr_corr * person_data[i, "yr1_ability"] +
      group_inc + cheat_inc + indiv_inc
    
  }
  
  return(person_data)
}

#get the a single person's response to a single item
response_sim <- function(ability, item_vec){
  guts <- item_vec["a_param"]*(ability-item_vec["b_param"])
  prob <- exp(guts)/(1+exp(guts))
  ifelse(runif(1, 0, 1) <= prob, return(1), return(0)) 
}

#get a set of people's responses to a single item 
item_response_sim <- function(item_param, person_param){
  output <- data.frame(matrix(NA, nrow = nrow(person_param), ncol = 4))
  colnames(output) <- c("studentid", "groupid", "itemid", "response")
  output$studentid <- person_param$studentid
  output$groupid <- person_param$groupid
  output$itemid <- item_param["itemid"]
  output$response <- sapply(person_param[,"yr2_ability"], FUN = response_sim, 
                            item_vec = item_param)
  
  return(output)
}