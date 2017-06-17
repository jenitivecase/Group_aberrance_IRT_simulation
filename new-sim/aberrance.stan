data {
  int<lower=1> I;                     // number of items
  int<lower=1> J;                     // number of people
  int<lower=1> G;                     // number of groups
  int<lower=1> N;                     // number of observations
  int<lower=1, upper=G> jg[J];        // group for j
  int<lower=1, upper=I> ii1[N];       // question for n in year 1
  int<lower=1, upper=J> jj1[N];       // person for n in year 1
  int<lower=1, upper=G> gg1[N];       // group for n in year 1
  int<lower=0, upper=1> y1[N];        // correctness for n in year 1
  int<lower=1, upper=I> ii2[N];       // question for n in year 2
  int<lower=1, upper=J> jj2[N];       // person for n in year 2
  int<lower=1, upper=G> gg2[N];       // group for n in year 2
  int<lower=0, upper=1> y2[N];        // correctness for n in year 2
}
parameters {
  vector<lower=0>[I] a;               // discrimination for i
  vector[I] b;                        // difficulty for i
  vector[J] theta1;                   // year 1 ability for j
  vector[J] theta2;                   // year 2 ability for j
  
  real<lower=-1, upper=1> corr;       // theta correlation
  vector[G] group_inc;                // average increase for g
}
transformed parameters {
  vector[J] mu2;
  
  for (j in 1:J) {
    mu2[j] = corr * theta1[j] + group_inc[jg[j]];
  }
}
model {
  vector[N] eta_yr1;
  vector[N] eta_yr2;
  
  // priors
  a ~ lognormal(0, 1);
  b ~ normal(0, 1);
  theta1 ~ normal(0, 1);
  theta2 ~ normal(mu2, 1);
  corr ~ normal(0, 1);
  group_inc ~ normal(0, 3);
  
  for (n in 1:N) {
    eta_yr1[n] = a[ii1[n]] * (theta1[jj1[n]] - b[ii1[n]]);
  }
  y1 ~ bernoulli_logit(eta_yr1);
  
  for (n in 1:N) {
    eta_yr2[n] = a[ii2[n]] * (theta2[jj2[n]] - b[ii2[n]]);
  }
  y2 ~ bernoulli_logit(eta_yr2);
}
