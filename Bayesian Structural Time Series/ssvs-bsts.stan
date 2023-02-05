data {
    int<lower=1> N;
    int<lower=1> p;
    vector<lower=0>[N] Y;
    vector[p] X[N];
    int<lower=2006,upper=2010> year[N];
    real<lower=0> tau_ss;
    real<lower=0> c_ss;
}

parameters {
    real beta0;
    vector[p] beta;
    real<lower=0> sigma;
    vector[4] D;
    real<lower=-1,upper=1> a;
    real<lower=0> sigma_d;
    real<lower=0,upper=1> theta[p];
}

model {
    # BSTS model assuming D_0 = 0
    a ~ uniform(-1,1);
    sigma_d ~ cauchy(0.0, 1.0);
    D[1] ~ normal(0, sigma_d);
    D[2:4] ~ normal(a * D[1:3], sigma_d);
    
    # Regression
    sigma ~ cauchy(0.0, 1.0);
    beta0 ~ normal(0, 15000);
    
    vector[N] mu;
    for (i in 1:N){
        mu[i] <- beta0 + dot_product(X[i], beta);
        if(year[i]!=2006){
            mu[i] <- mu[i] + D[year[i]-2006];
        }
    }
    
    # SSVS priors
    theta[1:p] ~ uniform(0,1);
    for (i in 1:p){
        target += log_mix(0.5, normal_lpdf(beta[i] | 0, tau_ss), normal_lpdf(beta[i] | 0, tau_ss * c_ss));
    }
    
    Y[1:N] ~ normal(mu[1:N],sigma);
}


