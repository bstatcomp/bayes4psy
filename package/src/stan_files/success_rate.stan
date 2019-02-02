data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  int<lower=0,upper=1> r[n]; // results - success or fail
  int<lower=0> s[n]; // subject ids
}

parameters {
  // success probability
  vector<lower=0,upper=1>[m] p;
}

model {
  p ~ beta(1, 1);

  // iterate over all measurements
  for (i in 1:n) {
    r[i] ~ bernoulli(p[s[i]]);
  }
}
