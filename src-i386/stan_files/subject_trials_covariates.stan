data {
  int n; // number of subjects
  int m; // number of tasks per subject
  int s; // number of covariates per subject
  int t; // number of covariates per task
  matrix[n,m] T; // times
  int S[n,m]; // succesess
  matrix[s,n] C_s; // covariates for subjects
  matrix[t,m] C_t; // covariates for tasks
}

parameters {
  // times
  vector[n] mu;
  vector<lower=0>[n] ss;
  vector<lower=0>[n] lambda;
  real mu_m;
  real mu_l;
  real<lower=0> ss_m;
  real<lower=0> ss_l;
  // successes
  vector[n] p;
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  // iterate over subjects
  for (i in 1:n) {
    mu[i] ~ normal(mu_m, ss_m);
    lambda[i] ~ normal(mu_l, ss_l);
    p[i] ~ beta(alpha, beta);
    // iterate over tasks
    for (j in 1:m) {
      T[i,j] ~ exp_mod_normal(mu[i], ss[i], lambda[i]);
      S[i,j] ~ bernoulli(p[i]);
    }
  }
}
