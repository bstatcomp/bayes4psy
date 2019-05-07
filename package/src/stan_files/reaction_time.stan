data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  vector<lower=0>[n] rt; // reaction times
  int<lower=0> s[n]; // subject ids
}

parameters {
  // per subject parameters
  vector<lower=0>[m] mu;
  vector<lower=0>[m] sigma;
  vector<lower=0>[m] lambda;

  // global parameters
  real<lower=0> mu_m;
  real<lower=0> mu_l;
  real<lower=0> mu_s;
  real<lower=0> sigma_m;
  real<lower=0> sigma_l;
  real<lower=0> sigma_s;
}

model {
  mu ~ normal(mu_m, sigma_m);
  sigma ~ normal(mu_s, sigma_s);
  lambda ~ normal(mu_l, sigma_l);

  // iterate over all measurements
  for (i in 1:n) {
    rt[i] ~ exp_mod_normal(mu[s[i]], sigma[s[i]], lambda[s[i]]);
  }
}
