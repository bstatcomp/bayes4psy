data {
	int<lower=1> n; // number of samples
	int<lower=1> m; // number of stimuli
	int<lower=0> stimuli[n]; // id of the stimnuli
	// rgb
	real<lower=0,upper=255> r[n];
	real<lower=0,upper=255> g[n];
	real<lower=0,upper=255> b[n];
	// hsv
	real<lower=0,upper=2*pi()> h[n];
	real<lower=0,upper=1> s[n];
	real<lower=0,upper=1> v[n];
}

parameters {
	// rgb
	real<lower=0,upper=255> mu_r[m];
	real<lower=0> ss_r[m];
	real<lower=0,upper=255> mu_g[m];
	real<lower=0> ss_g[m];
	real<lower=0,upper=255> mu_b[m];
	real<lower=0> ss_b[m];
	// hsv
	real<lower=0,upper=2*pi()> mu_h[m];
	real<lower=0> kappa_h[m];
	real<lower=0,upper=1> mu_s[m];
	real<lower=0> ss_s[m];
	real<lower=0,upper=1> mu_v[m];
	real<lower=0> ss_v[m];
}

model {
	// priors
	// rgb
	mu_r ~ uniform(0, 255);
	ss_r ~ normal(0, 50);
	mu_g ~ uniform(0, 255);
	ss_g ~ normal(0, 50);
	mu_b ~ uniform(0, 255);
	ss_b ~ normal(0, 50);

	// hsv
	mu_h ~ uniform(0, 2*pi());
	kappa_h ~ normal(0, 50);
	mu_s ~ uniform(0, 1);
	ss_s ~ normal(0, 0.2);
	mu_v ~ uniform(0, 1);
	ss_v ~ normal(0, 0.2);

	for (i in 1:n) {
		// rgb
		//r[i] ~ normal(mu_r[stimuli[i]], ss_r[stimuli[i]])T[0,255]; // truncated normal
		//g[i] ~ normal(mu_g[stimuli[i]], ss_g[stimuli[i]])T[0,255];
		//b[i] ~ normal(mu_b[stimuli[i]], ss_b[stimuli[i]])T[0,255];
		// hsv
		//h[i] ~ von_mises(mu_h[stimuli[i]], kappa_h[stimuli[i]]);
		//s[i] ~ normal(mu_g[stimuli[i]], ss_g[stimuli[i]])T[0,1];
		//b[i] ~ normal(mu_b[stimuli[i]], ss_b[stimuli[i]])T[0,1];
	}
}
