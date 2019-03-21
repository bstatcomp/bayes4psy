data {
	// number of samples
	int<lower=1> n;
	// hue
	real<lower=0> h[n];
}

parameters {
	real<lower=0> mu;
	real<lower=0> ss;
}

model {
	h ~ normal(mu, ss);
}
