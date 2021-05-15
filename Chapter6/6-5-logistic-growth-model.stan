data {
  int n_sample;
  int y[n_sample];
}

parameters {
  real<lower=0> r;
  real<lower=0> K;
  real mu_zero;
  real mu[n_sample];
  real<lower=0> s_w;
}

transformed parameters {
  real<lower=0> lambda[n_sample];
  for(i in 1:n_sample) {
    lambda[i] = exp(mu[i]);
  }
}

model {
  mu[1] ~ normal(mu_zero, sqrt(s_w));
  
  for(i in 2:n_sample) {
    mu[i] ~ normal(mu[i-1] + r*mu[i-1]*(1 - mu[i-1]/K), sqrt(s_w));
  }

  for(i in 1:n_sample) {
    y[i] ~ poisson(lambda[i]);
  }

  K ~ normal(7, 3);

}

generated quantities {
  real mu_smooth[n_sample];
  real lambda_smooth[n_sample];
  
  mu_smooth[1] = mu_zero;
  
  for(i in 2:n_sample) {
    mu_smooth[i] = mu_smooth[i-1] + r*mu_smooth[i-1]*(1 - mu_smooth[i-1]/K);
  }
  
  for(i in 1:n_sample) {
   lambda_smooth[i] = exp(mu_smooth[i]);
  }
}

