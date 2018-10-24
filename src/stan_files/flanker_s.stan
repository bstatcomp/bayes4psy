data {
  int<lower=0> n; // number of subjects
  int<lower=0> m; // number of tasks per subject
  int<lower=0,upper=1> S[n,m]; // successes
}

parameters {
  // sucesses
  vector<lower=0,upper=1>[n] p;
}

model {
  // iterate over subjects
  for (i in 1:n) {
    p[i] ~ beta(1, 1);
    // iterate over tasks
    for (j in 1:m) {
      S[i,j] ~ bernoulli(p[i]);
    }
  }
}
