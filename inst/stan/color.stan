data {
	int<lower=0> n; // number of data entries
	// rgb
	vector<lower=0,upper=255>[n] r;
	vector<lower=0,upper=255>[n] g;
	vector<lower=0,upper=255>[n] b;
	// hsv
	vector<lower=0,upper=2*pi()>[n] h;
	vector<lower=0,upper=1>[n] s;
	vector<lower=0,upper=1>[n] v;

	// priors
	array[12] int<lower=0> p_ids;
	vector[24] p_values;
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
	real mu_h;
	real<lower=0> kappa_h;
	real<lower=0,upper=1> mu_s;
	real<lower=0> sigma_s;
	real<lower=0,upper=1> mu_v;
	real<lower=0> sigma_v;
}

model {
	// priors
	// mu_r
	int id = 1;
	if (p_ids[id] == 1) {
		mu_r ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		mu_r ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		mu_r ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		mu_r ~ beta(p_values[id*2-1], p_values[id*2]);
	}
	// sigma_r
	id = 2;
	if (p_ids[id] == 1) {
		sigma_r ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		sigma_r ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		sigma_r ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		sigma_r ~ beta(p_values[id*2-1], p_values[id*2]);
	}

	// mu_g
	id = 3;
	if (p_ids[id] == 1) {
		mu_g ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		mu_g ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		mu_g ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		mu_g ~ beta(p_values[id*2-1], p_values[id*2]);
	}
	// sigma_g
	id = 4;
	if (p_ids[id] == 1) {
		sigma_g ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		sigma_g ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		sigma_g ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		sigma_g ~ beta(p_values[id*2-1], p_values[id*2]);
	}

	// mu_b
	id = 5;
	if (p_ids[id] == 1) {
		mu_b ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		mu_b ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		mu_b ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		mu_b ~ beta(p_values[id*2-1], p_values[id*2]);
	}
	// sigma_b
	id = 6;
	if (p_ids[id] == 1) {
		sigma_b ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		sigma_b ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		sigma_b ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		sigma_b ~ beta(p_values[id*2-1], p_values[id*2]);
	}

	// mu_h
	id = 7;
	if (p_ids[id] == 1) {
		mu_h ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		mu_h ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		mu_h ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		mu_h ~ beta(p_values[id*2-1], p_values[id*2]);
	}
	// kappa_h
	id = 8;
	if (p_ids[id] == 1) {
		kappa_h ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		kappa_h ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		kappa_h ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		kappa_h ~ beta(p_values[id*2-1], p_values[id*2]);
	}

	// mu_s
	id = 9;
	if (p_ids[id] == 1) {
		mu_s ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		mu_s ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		mu_s ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		mu_s ~ beta(p_values[id*2-1], p_values[id*2]);
	}
	// sigma_s
	id = 10;
	if (p_ids[id] == 1) {
		sigma_s ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		sigma_s ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		sigma_s ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		sigma_s ~ beta(p_values[id*2-1], p_values[id*2]);
	}

	// mu_v
	id = 11;
	if (p_ids[id] == 1) {
		mu_v ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		mu_v ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		mu_v ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		mu_v ~ beta(p_values[id*2-1], p_values[id*2]);
	}
	// sigma_v
	id = 12;
	if (p_ids[id] == 1) {
		sigma_v ~ uniform(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 2) {
		sigma_v ~ normal(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 3) {
		sigma_v ~ gamma(p_values[id*2-1], p_values[id*2]);
	} else if (p_ids[id] == 4) {
		sigma_v ~ beta(p_values[id*2-1], p_values[id*2]);
	}

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
