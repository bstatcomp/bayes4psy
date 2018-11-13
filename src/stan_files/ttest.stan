data {
	int<lower=1> n; // number of samples
	real y[n];
	real yMu;
	real<lower=0> ySd;
}

transformed data {
	real uLo;
	real uHi;
	real nSigma;
	real eLambda;
	uLo = ySd / 1000;
	uHi = ySd * 1000;
	nSigma = ySd * 100;
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
	sigma ~ uniform(uLo, uHi);
	mu ~ normal(yMu, nSigma);
	nuMinusOne ~ exponential(eLambda);

	for (i in 1:n)
		y[i] ~ student_t(nu, mu, sigma);
}
