data {
	// number of samples
	int<lower=1> n;
	// hue
	real<lower=0,upper=2*pi()> h[n];
}

parameters {
	real<lower=0,upper=2*pi()> mu;
	real<lower=0> kappa;
}

model {
	h ~ von_mises(mu, kappa);
}
