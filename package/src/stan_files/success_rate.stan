data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  int<lower=0,upper=1> r[n]; // results - success or fail
  int<lower=0> s[n]; // subject ids

  // priors
  int<lower=0> p_ids[2];
  real<lower=0> p_values[4];
}

parameters {
  // global parameters
  real<lower=0,upper=1> p0;
  real<lower=0> tau;

  // success per subject
  vector<lower=0,upper=1>[m] p;
}

model {
  // priors
  // p0
  int id = 1;
  if (p_ids[id] == 1) {
    p0 ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    p0 ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    p0 ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    p0 ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // tau
  id = 2;
  if (p_ids[id] == 1) {
    tau ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    tau ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    tau ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    tau ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // percentage
  p ~ beta(p0*tau, (1 - p0)*tau);

  // iterate over all measurements
  for (i in 1:n) {
    r[i] ~ bernoulli(p[s[i]]);
  }
}
