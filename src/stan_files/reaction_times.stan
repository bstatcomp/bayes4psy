data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  vector<lower=0>[n] rt; // reaction times
  int<lower=0> s[n]; // id of the subject
}

parameters {
  // reaction times
  vector<lower=0>[m] mu;
  vector<lower=0>[m] sigma;
  vector<lower=0>[m] lambda;
  real mu_m;
  real mu_l;
  real mu_s;
  real<lower=0> ss_m;
  real<lower=0> ss_l;
  real<lower=0> ss_s;
}

model {
  mu ~ normal(mu_m, ss_m);
  sigma ~ normal(mu_s, ss_s);
  lambda ~ normal(mu_l, ss_l);

  // iterate over all measurements
  for (i in 1:n) {
    rt[i] ~ exp_mod_normal(mu[s[i]], sigma[s[i]], lambda[s[i]]);
  }
}
