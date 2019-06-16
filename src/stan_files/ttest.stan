data {
  int<lower=1> n; // number of samples
  real y[n];

  // priors
  int<lower=0> p_ids[2];
  real p_values[4];
}

transformed data {
  real eLambda;
  eLambda = 1 / 29.0;
}

parameters {
  real<lower=0> nuMinusOne;
  real mu;
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=0> nu;
  nu = nuMinusOne + 1;
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

  nuMinusOne ~ exponential(eLambda);

  for (i in 1:n) {
    y[i] ~ student_t(nu, mu, sigma);
  }
}
