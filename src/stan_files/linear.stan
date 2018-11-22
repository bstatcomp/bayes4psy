data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  vector[n] x; // sequence/time
  vector[n] y; // response
  int<lower=0> s[n]; // subject ids
}

parameters {
  // per subject parameters  
  vector[m] alpha;
  vector[m] beta;
  vector<lower=0>[m] sigma;

  // global parameters
  real mu_a;
  real mu_b;
  real mu_s;
  real<lower=0> ss_a;
  real<lower=0> ss_b;
  real<lower=0> ss_s;
}

model {
  alpha ~ normal(mu_a, ss_a);
  beta ~ normal(mu_b, ss_b);
  sigma ~ normal(mu_s, ss_s);

  for (i in 1:n)
    y[i] ~ normal(alpha[s[i]] + beta[s[i]] * x[i], sigma[s[i]]);
}
