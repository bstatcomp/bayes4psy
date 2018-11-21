data {
	int<lower=1> n; // number of samples
	int<lower=1> m; // number of stimuli
	int<lower=0> stimuli[n]; // id of the stimuli
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
	mu_r ~ normal(127.5, 50);
	ss_r ~ normal(50, 25);
	mu_g ~ normal(127.5, 50);
	ss_g ~ normal(50, 25);
	mu_b ~ normal(127.5, 50);
	ss_b ~ normal(50, 25);

	// hsv
	mu_h ~ von_mises(pi(), 0.1);
	kappa_h ~ normal(50, 25);
	mu_s ~ normal(0.5, 0.2);
	ss_s ~ normal(0.2, 0.1);
	mu_v ~ normal(0.5, 0.2);
	ss_v ~ normal(0.2, 0.1);

	for (i in 1:n) {
		// rgb
		r[i] ~ normal(mu_r[stimuli[i]], ss_r[stimuli[i]]);
		g[i] ~ normal(mu_g[stimuli[i]], ss_g[stimuli[i]]);
		b[i] ~ normal(mu_b[stimuli[i]], ss_b[stimuli[i]]);
		// hsv
		h[i] ~ von_mises(mu_h[stimuli[i]], kappa_h[stimuli[i]]);
		s[i] ~ normal(mu_s[stimuli[i]], ss_s[stimuli[i]]);
		v[i] ~ normal(mu_v[stimuli[i]], ss_v[stimuli[i]]);
	}
}
