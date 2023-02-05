data {
    int<lower=1> N;
    int<lower=1> p;
    vector<lower=0>[N] Y;
    matrix[N,p] X;
    matrix[N,N] dist;
    real<lower=0.000001> phi;
    vector<lower=0>[2] priorsigma2;
    vector<lower=0>[2] priortau2;
}

transformed data {
    real delta = 1e-5;
    vector[N] mu_0 = rep_vector(0,N);
}

parameters {
    real beta0;
    vector[p] beta; 
    real<lower=0> sigma_sq;
    vector[N] eta;
    real<lower=0> tau_sq;
}

model {
    vector[N] xbmodel;
    matrix[N,N] L;
    matrix[N,N] Sigma;

    # Linear regression on the features
    beta[1:p] ~ normal(0, 1000);
    beta0 ~ normal(0, 15000);

    xbmodel = (X * beta) + rep_vector(beta0, N);

    # The spatial effects have a normal distribution with covariance matrix
    # parameterized by an exponential family
    for (i in 1:N) {
        for (j in 1:N) {
            Sigma[i, j] = sigma_sq * exp((-1)*phi*dist[i,j]);
        }
        Sigma[i, i] = Sigma[i, i] + delta;
    }
    L = cholesky_decompose(Sigma);
    eta ~ multi_normal_cholesky(mu_0, L);
    
    # The price is given by regression on the house features + spatial effect + micro-scale variability 
    Y ~ normal(xbmodel+eta, sqrt(tau_sq));
    sigma_sq ~ inv_gamma(priorsigma2[1], priorsigma2[2]);
    tau_sq ~ inv_gamma(priortau2[1], priortau2[2]);

}


