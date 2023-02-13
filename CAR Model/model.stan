
data {
    int<lower=1> N; // number of houses
    int<lower=1> K; // number of neighborhoods
    int<lower=1> p; // number of covariates
    vector<lower=0>[N] Y; // response variable (price)
    matrix[N, p] X; // matrix of covariates
    matrix[K, K] W; // adjacency matrix of neighborhoods
    vector <lower=1, upper=K>[N] k; // neighborhood id for each house
    real<lower=0> a; // shape parameter for inverse-gamma prior
    real<lower=0> b; // scale parameter for inverse-gamma prior    
}
parameters {
    vector[p] beta; // neighborhood-specific coefficients
    real beta_intercept;
    vector[K] phi; // random effect for each neighborhood 
    real<lower=0> nu2; // common variance of response variable
    real<lower=0> tau2; // common variance of random effects
    real<lower=0, upper=1> rho; // correlation between random effects across neighborhoods
}
model {
    for (j in 1:p) {
        beta[j] ~ normal(0, 10);
    }
    beta_intercept ~ normal(4.5, 0.01);
    for (q in 1:K) {
         phi[q] ~ normal(rho*W[q,:]*phi/(rho*sum(W[q,:])+1-rho), sqrt(tau2/(rho*sum(W[q,:]) + 1-rho)));
    }
    nu2 ~ inv_gamma(a, b);
    tau2 ~ inv_gamma(a, b);
    rho ~ uniform(0, 1);
    for (i in 1:N) {
      for(l in 1:K){
        if(k[i]==l)
         Y[i] ~ normal(beta_intercept + X[i,:]*beta + phi[l], sqrt(nu2));       
      }
    }
}

