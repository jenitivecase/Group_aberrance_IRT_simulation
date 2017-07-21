stancode_long <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int<lower=0> n_observations;
  int<lower=0> n_groups;
  int<lower=0, upper=n_people> studentid[n_observations];
  int<lower=0, upper=n_items> itemid_yr1[n_observations];
  int<lower=0, upper=n_items> itemid_yr2[n_observations];
  int<lower=0, upper=1> response_yr1[n_observations];
  int<lower=0, upper=1> response_yr2[n_observations];
  int<lower=0, upper=n_groups> groupid[n_observations];
  int<lower=1, upper=n_groups> per_group[n_people];
}

parameters {
  // real<lower=0> a_yr1[n_items];
  // real<lower=0> b_yr1[n_items];
  // real<lower=0> a_yr2[n_items];
  // real<lower=0> b_yr2[n_items];
  // Define a single vector of a's and b's (removing lower limit on b)
  real<lower=0> a[n_items];
  real b[n_items];
  real theta1[n_people];
  real<lower=-1, upper=1> corr;
  real group_inc[n_groups];
  real indiv_err[n_people];
}

transformed parameters {
  real theta2[n_people];
  real mu2;
  
  for (i in 1:n_people) {
    theta2[i] = corr*theta1[i] + group_inc[per_group[i]] + indiv_err[i];
  }

  mu2 = mean(theta2);
}

model {
  real eta_yr1[n_observations];
  real eta_yr2[n_observations];
  
  a ~ lognormal(0, 1);
  b ~ normal(0, 1);
  //  a_yr2 ~ lognormal(0, 1);
  //  b_yr2 ~ normal(0, 1);
  theta1 ~ normal(0, 1);
  corr ~ normal(0, 1);
  group_inc ~ normal(0, 3);
  indiv_err ~ normal(0, 3);

  for(i in 1:n_observations){
    eta_yr1[i] = a[itemid_yr1[i]]*(theta1[studentid[i]] - (b[itemid_yr1[i]]));
  }

  response_yr1 ~ bernoulli_logit(eta_yr1);  

  for(i in 1:n_observations){
    eta_yr2[i] = a[itemid_yr2[i]]*(theta2[studentid[i]] - (b[itemid_yr2[i]]));
  }
  
  response_yr2 ~ bernoulli_logit(eta_yr2);
}
"