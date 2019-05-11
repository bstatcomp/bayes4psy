data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  vector<lower=0>[n] t; // reaction times
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
  for (j in 1:m) {
    mu[j] ~ normal(mu_m, sigma_m) T[0,];
    sigma[j] ~ normal(mu_s, sigma_s) T[0,];
    lambda[j] ~ normal(mu_l, sigma_l);
  }

  // iterate over all measurements
  for (i in 1:n) {
    t[i] ~ exp_mod_normal(mu[s[i]], sigma[s[i]], lambda[s[i]]);
  }
}

generated quantities {
  real<lower=0> rt;
  vector<lower=0>[m] rt_subjects;

  rt = mu_m + 1/mu_l;
  for (i in 1:m) {
    rt_subjects[i] = mu[i] + 1/lambda[i];
  }
}
