data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  vector<lower=0>[n] t; // reaction times
  array[n] int<lower=0> s; // subject ids

  // priors
  array[6] int p_ids;
  vector[12] p_values;
}

parameters {
  // per subject parameters
  vector<lower=0>[m] mu;
  vector<lower=0>[m] sigma;
  vector<lower=0>[m] lambda;

  // global parameters
  real<lower=0> mu_m;
  real<lower=0.05> mu_l;
  real<lower=0> mu_s;
  real<lower=0> sigma_m;
  real<lower=0> sigma_l;
  real<lower=0> sigma_s;
}

model {
  // priors
  // mu_m
  int id = 1;
  if (p_ids[id] == 1) {
    mu_m ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    mu_m ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    mu_m ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    mu_m ~ beta(p_values[id*2-1], p_values[id*2]);
  }
  // sigma_m
  id = 2;
  if (p_ids[id] == 1) {
    sigma_m ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma_m ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma_m ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma_m ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // mu_s
  id = 3;
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
  id = 4;
  if (p_ids[id] == 1) {
    sigma_s ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma_s ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma_s ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma_s ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // mu_l
  id = 5;
  if (p_ids[id] == 1) {
    mu_l ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    mu_l ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    mu_l ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    mu_l ~ beta(p_values[id*2-1], p_values[id*2]);
  }
  // sigma_l
  id = 6;
  if (p_ids[id] == 1) {
    sigma_l ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma_l ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma_l ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma_l ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  mu ~ normal(mu_m, sigma_m);
  sigma ~ normal(mu_s, sigma_s);
  lambda ~ normal(mu_l, sigma_l);

  // iterate over all measurements
  for (i in 1:n) {
    t[i] ~ exp_mod_normal(mu[s[i]], sigma[s[i]], lambda[s[i]]);
  }
}

generated quantities {
  real rt;
  vector[m] rt_subjects;

  rt = mu_m + 1/mu_l;
  for (i in 1:m) {
    rt_subjects[i] = mu[i] + 1/lambda[i];
  }
}
