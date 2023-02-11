data {
	int<lower=0> N;		// number of data items
	int<lower=0> P;		// number of fixed effect covariates
	int<lower=0> K;		// number of group effect covariates
	int<lower=0> M;		// number of groups

	vector[N] y;			// outcome vector
	array[N] int<lower=1, upper=M> group;	// neighborhood assignment

	matrix[N, P] x;		// fixed effect covariate matrix
	matrix[N, K] z;		// group effect covariate matrix

	real alpha0;			// sigma shape
	real sigma0;			// sigma scale
	real beta0;				// tau shape
	real tau0;				// tau scale
	real eta0;				// xi shape
	real xi0;					// xi scale

	vector[P] theta0;	// mean of fixed effect parameters
	vector[K] gamma0;	// mean of group effect parameters
}

parameters {
	vector<lower=0>[P] theta;	// fixed effect regressors
	matrix[M, K] gamma;				// group effect regressors

	real<lower=0> sigma;			// std for y
	real<lower=0> tau;				// std for theta
	real<lower=0> xi;					// std for gamma
}

model {
	for (i in 1:N) {
		y[i] ~ normal(dot_product(x[i], theta) + dot_product(z[i], gamma[group[i]]), sqrt(sigma));
	}
	sigma ~ inv_gamma(alpha0, alpha0 * sigma0);

	for (p in 1:P) {
		theta[p] ~ normal(theta0[p], sqrt(tau));
	}
	tau ~ inv_gamma(beta0, beta0 * tau0);

	for (j in 1:M) {
		for (k in 1:K) {
			gamma[j, k] ~ normal(gamma0[k], sqrt(xi));
		}
	}
	xi ~ inv_gamma(eta0, eta0 * xi0);
}

generated quantities {
   real log_likelihood = 0;
   real log_likelihood_sq = 0;
   for (i in 1:N){
		real mean = dot_product(x[i], theta) + dot_product(z[i], gamma[group[i]]);
		real sd = sqrt(sigma);
		log_likelihood += normal_lpdf(y[i] | mean, sd);
		log_likelihood_sq += normal_lpdf(y[i] | mean, sd) ^ 2;
   }
}
