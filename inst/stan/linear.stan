data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  vector[n] x; // sequence/time
  vector[n] y; // response
  array[n] int<lower=0> s; // subject ids

  // priors
  array[6] int<lower=0> p_ids;
  vector[12] p_values;
}

parameters {
  // per subject parameters  
  vector[m] alpha;
  vector[m] beta;
  vector<lower=0>[m] sigma;

  // global parameters
  real mu_a;
  real mu_b;
  real<lower=0> mu_s;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_s;
}

model {
  // priors
  // mu_a
  int id = 1;
  if (p_ids[id] == 1) {
    mu_a ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    mu_a ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    mu_a ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    mu_a ~ beta(p_values[id*2-1], p_values[id*2]);
  }
  // sigma_a
  id = 2;
  if (p_ids[id] == 1) {
    sigma_a ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma_a ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma_a ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma_a ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // mu_b
  id = 3;
  if (p_ids[id] == 1) {
    mu_b ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    mu_b ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    mu_b ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    mu_b ~ beta(p_values[id*2-1], p_values[id*2]);
  }
  // sigma_b
  id = 4;
  if (p_ids[id] == 1) {
    sigma_b ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma_b ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma_b ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma_b ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // mu_s
  id = 5;
  if (p_ids[id] == 1) {
    mu_s ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    mu_s ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    mu_s ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    mu_s ~ beta(p_values[id*2-1], p_values[id*2]);
  }
  // sigma_s
  id = 6;
  if (p_ids[id] == 1) {
    sigma_s ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma_s ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma_s ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma_s ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  alpha ~ normal(mu_a, sigma_a);
  beta ~ normal(mu_b, sigma_b);
  sigma ~ normal(mu_s, sigma_s);

  for (i in 1:n)
    y[i] ~ normal(alpha[s[i]] + beta[s[i]] * x[i], sigma[s[i]]);
}
