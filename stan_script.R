stancode_long <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int<lower=0> n_observations;
  int<lower=0> n_groups;
  int<lower=0, upper=n_people> studentid[n_observations];
  int<lower=0, upper=n_items> itemid[n_observations];
  int<lower=0, upper=1> response[n_observations];
  int<lower=0, upper=1> groupid[n_observations];
}

parameters {
  vector<lower=0>[n_items] a;
  vector[n_items] b;
  vector[n_people] theta1;
  real<lower=0, upper=1> corr;
  real<lower=0> sigma2;
  real mu2;
}

transformed parameters {
  vector[n_groups] group_inc;
  vector[n_people] indiv_err;
  vector[n_people] theta2;
  # vector[n_items] ss_err;
  # vector[n_items] ss_reg;
  # real R2;
  
  for (i in 1:n_observations) {
    theta2[studentid[i]] = corr*theta1[studentid[i]] + group_inc[groupid[i]] + indiv_err[studentid[i]];
  }
  
  
### how to calculate here?
#   for (j in 1:n_items) {
#     ss_err[j] = pow((D[j]-mu[j]),2);
#     ss_reg[j] = pow((mu[j]-mean(D[])),2);
#   }
#   
#   R2 = sum(ss_reg[])/(sum(ss_reg[])+sum(ss_err[]));
}

model {
  vector[n_observations] eta;
  
  a ~ lognormal(0, 1);
  b ~ normal(0, 1);
  theta2 ~ normal(mu2, 1);
  theta1 ~ normal(0, 1);
  sigma2 ~ normal(0, 10);
  corr ~ normal(0, 1);
  group_inc ~ normal(0, 3);
  indiv_err ~ normal(0, 3);
  
  for(i in 1:n_observations){
    eta[i] = a[itemid[i]]*(theta2[studentid[i]] - (b[itemid[i]]));
  }
  
  response ~ bernoulli_logit(eta);
}
"