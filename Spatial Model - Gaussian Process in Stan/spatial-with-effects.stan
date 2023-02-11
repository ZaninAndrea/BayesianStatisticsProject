data {
    int<lower=1> N;
    int<lower=1> p;
    vector<lower=0>[N] Y;
    matrix[N,p] X;
    matrix[N,N] dist;
    real<lower=0.000001> phi;
    vector<lower=0>[2] priorsigma2;
    vector<lower=0>[2] priortau2;
    matrix[N,N] Tau;
    real logDet;
}

transformed data {
    vector[N] mu_0 = rep_vector(0,N);
    real delta = 1e-5;
    matrix[N,N] I = diag_matrix(rep_vector(1,N));
}

parameters {
    real beta0;
    vector[p] beta; 
    real<lower=0.00001,upper=1> sigma_sq;
    real<lower=0.00001,upper=1> tau_sq;
    vector[N] spatialEffect;
}

model {
    vector[N] xbmodel;
    matrix[N,N] L;

    // Linear regression on the features
    beta[1:p] ~ normal(0, 1);
    beta0 ~ normal(0, 10);

    xbmodel = (X * beta) + rep_vector(beta0, N);
    
    // The price is given by regression on the house features + spatial effect + micro-scale variability 
    target += - 0.5*logDet - 0.25 * log(sigma_sq) - 0.5 * dot_product(spatialEffect' * Tau, spatialEffect);
    Y ~ normal(xbmodel + spatialEffect, tau_sq);

    sigma_sq ~ inv_gamma(priorsigma2[1], priorsigma2[2]);
    tau_sq ~ inv_gamma(priortau2[1], priortau2[2]);
}

// generated quantities {
//    print(sigma_sq);
//    print(tau_sq);
// }