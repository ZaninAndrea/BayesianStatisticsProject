data {
    int<lower=1> N;
    int<lower=1> p;
    vector<lower=0>[N] Y;
    vector[p] X[N];
    int<lower=2006,upper=2010> year[N];
    real<lower=0> tau_ss;
    real<lower=0> c_ss;
    real sigma_max;
}

parameters {
    real beta0;
    vector[p] beta;
    real<lower=0> sigma;
    vector[4] D;
    real<lower=-1,upper=1> a;
    real<lower=0> sigma_d;
}

model {
    # BSTS model assuming D_0 = 0
    a ~ uniform(-1,1);
    sigma_d ~ inv_gamma(50,1);
    D[1] ~ normal(0, sigma_d);
    D[2:4] ~ normal(a * D[1:3], sigma_d);
    
    # Regression
    sigma ~ inv_gamma(3,1);
    beta0 ~ normal(0, 15);
    beta[1:p] ~ normal(0, 1);
    
    vector[N] mu;
    for (i in 1:N){
        mu[i] <- beta0 + dot_product(X[i], beta);
        if(year[i]!=2006){
            mu[i] <- mu[i] + D[year[i]-2006];
        }
    }
    Y[1:N] ~ normal(mu[1:N],sigma);
}


