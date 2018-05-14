data {
	int<lower=1> n;
	real y[n];
	real yMu;
	real<lower=0> ySd;
	real<lower=0> uL;
	real<lower=0> uH;
	int<lower=0,upper=1> log_skewed;
}

transformed data {
	real yt[n];

	yt = y;
	if (log_skewed == 1) {
		yt = log(yt);
	}
}

parameters {
	real<lower=0> sigma;
	real mu;

	real mu0;
	real<lower=0> sigma0;
}

model {
	sigma ~ uniform(uL, uH);
	mu ~ normal(yMu, ySd);

	yt ~ normal(mu, sigma);

	mu ~ normal(mu0, sigma0);
}
