data {
	int<lower=1> n; // number of samples
	real y[n];
	// priors
	int p_mu;
	real p_mu1; 
	real p_mu2;
	int p_sigma;
	real<lower=0> p_sigma1;
	real<lower=0> p_sigma2;
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
	if (p_mu == 1) {
		mu ~ uniform(p_mu1, p_mu2);
	} else if (p_mu == 2) {
		mu ~ normal(p_mu1, p_mu2);
	} else if (p_mu == 3) {
		mu ~ gamma(p_mu1, p_mu2);
	} else if (p_mu == 4) {
		mu ~ beta(p_mu1, p_mu2);
	}

	if (p_sigma == 1) {
		sigma ~ uniform(p_sigma1, p_sigma2);
	} else if (p_sigma == 2) {
		sigma ~ normal(p_sigma1, p_sigma2);
	} else if (p_sigma == 3) {
		sigma ~ gamma(p_sigma1, p_sigma2);
	} else if (p_sigma == 4) {
		sigma ~ beta(p_sigma1, p_sigma2);
	}

	nuMinusOne ~ exponential(eLambda);

	for (i in 1:n) {
		y[i] ~ student_t(nu, mu, sigma);
	}
}
