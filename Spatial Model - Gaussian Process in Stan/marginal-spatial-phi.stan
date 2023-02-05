data {
    int<lower=1> N;
    int<lower=1> p;
    vector<lower=0>[N] Y;
    matrix[N,p] X;
    matrix[N,N] dist;
    vector<lower=0>[2] priorsigma2;
    vector<lower=0>[2] priortau2;
}

transformed data {
    vector[N] mu_0 = rep_vector(0,N);
    real delta = 1e-5;
    matrix[N,N] I = diag_matrix(rep_vector(1,N));
}

parameters {
    real beta0;
    vector[p] beta; 
    real<lower=0> sigma_sq;
    real<lower=0> tau_sq;
    real<lower=0.01,upper=1> phi;
}

model {
    vector[N] xbmodel;
    matrix[N,N] L;
    matrix[N,N] Sigma;

    // Computing the covariance matrix
    phi ~ uniform(0.01, 1);
    for (i in 1:N) {
        for (j in 1:N) {
            Sigma[i, j] = exp((-1)*phi*dist[i,j]);
        }
        Sigma[i, i]=Sigma[i,i]+delta;
    }

    // Linear regression on the features
    beta[1:p] ~ normal(0, 1000);
    beta0 ~ normal(0, 15000);

    xbmodel = (X * beta) + rep_vector(beta0, N);

    
    // The price is given by regression on the house features + spatial effect + micro-scale variability 
    L = cholesky_decompose((sigma_sq * Sigma) + (tau_sq * I));
    Y ~ multi_normal_cholesky(xbmodel, L);
    // Y ~ multi_normal(xbmodel, (sigma_sq * Sigma) + (tau_sq * I));
    
    sigma_sq ~ inv_gamma(priorsigma2[1], priorsigma2[2]);
    tau_sq ~ inv_gamma(priortau2[1], priortau2[2]);
    // sigma_sq ~ cauchy(0.0,1.0);
    // tau_sq ~ cauchy(0.0, 1.0);
}

// generated quantities {
//    print(sigma_sq);
//    print(tau_sq);
// }