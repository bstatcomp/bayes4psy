data {
	int<lower=1> n; // number of samples
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
	real<lower=0,upper=255> mu_r;
	real<lower=0> sigma_r;
	real<lower=0,upper=255> mu_g;
	real<lower=0> sigma_g;
	real<lower=0,upper=255> mu_b;
	real<lower=0> sigma_b;
	
	// hsv
	real<lower=0,upper=2*pi()> mu_h;
	real<lower=0> kappa_h;
	real<lower=0,upper=1> mu_s;
	real<lower=0> sigma_s;
	real<lower=0,upper=1> mu_v;
	real<lower=0> sigma_v;
}

model {
	// priors
	// rgb
	mu_r ~ uniform(0, 255);
	sigma_r ~ uniform(0, 100);
	mu_g ~ uniform(0, 255);
	sigma_g ~ uniform(0, 100);
	mu_b ~ uniform(0, 255);
	sigma_b ~ uniform(0, 100);
	// hsv
	mu_h ~ uniform(0, 2*pi()); 
	kappa_h ~ uniform(0, 2);
	mu_s ~ uniform(0, 1);
	sigma_s ~ uniform(0, 0.5);
	mu_v ~ uniform(0, 1);
	sigma_v ~ uniform(0, 0.5);

	// posteriors
	// rgb
	r ~ normal(mu_r, sigma_r);
	g ~ normal(mu_g, sigma_g);
	b ~ normal(mu_b, sigma_b);
	// hsv
	h ~ von_mises(mu_h, kappa_h);
	s ~ normal(mu_s, sigma_s);
	v ~ normal(mu_v, sigma_v);
}
