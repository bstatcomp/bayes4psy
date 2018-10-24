data {
  int<lower=0> n; // number of subjects
  int<lower=0> m; // number of tasks per subject
  matrix[n,m] T; // reaction times
}

parameters {
  vector<lower=0>[n] mu;
  vector<lower=0>[n] ss;
  vector<lower=0>[n] lambda;
  // improper/flat priors
  real mu_m;
  real mu_l;
  real<lower=0> ss_m;
  real<lower=0> ss_l;
}

model {
  // iterate over subjects
  for (i in 1:n) {
    mu[i] ~ normal(mu_m, ss_m);
    lambda[i] ~ normal(mu_l, ss_l);
    // iterate over tasks
    for (j in 1:m) {
      T[i,j] ~ exp_mod_normal(mu[i], ss[i], lambda[i]);
    }
  }
}
