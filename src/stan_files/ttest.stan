data {
  int<lower=0> n; // number of samples
  real y[n];

  // priors
  int<lower=0> p_ids[3];
  real p_values[6];
}

parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0, upper=10000> nu;
}

model {
  // priors
  // mu
  int id = 1;
  if (p_ids[id] == 1) {
    mu ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    mu ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    mu ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    mu ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // sigma
  id = 2;
  if (p_ids[id] == 1) {
    sigma ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    sigma ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    sigma ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    sigma ~ beta(p_values[id*2-1], p_values[id*2]);
  }

  // nu
  id = 3;
  if (p_ids[id] == 1) {
    nu ~ uniform(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 2) {
    nu ~ normal(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 3) {
    nu ~ gamma(p_values[id*2-1], p_values[id*2]);
  } else if (p_ids[id] == 4) {
    nu ~ beta(p_values[id*2-1], p_values[id*2]);
  } else {
    nu ~ exponential(1.0/29.0);
  }

  y ~ student_t(nu, mu, sigma);
}
