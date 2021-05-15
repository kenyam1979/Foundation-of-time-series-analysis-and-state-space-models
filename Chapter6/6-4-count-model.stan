data {
  int n_sample;
  int catch_y[n_sample];
  int y[n_sample];
}

parameters {
  real trend;
  real coef_catch_y;
  real mu_zero;
  real mu[n_sample];
  real mu_noise[n_sample];
  real<lower=0> s_w;
  real<lower=0> s_v;
}

transformed parameters {
  real delta[n_sample];
  real lambda[n_sample];
  
  for(i in 1:n_sample) {
    delta[i] = trend - coef_catch_y * catch_y[i];
  }

  for(i in 1:n_sample) {
    lambda[i] = exp(mu_noise[i]);
  }
}

model {
  mu[1] ~ normal(mu_zero, sqrt(s_w));
  
  for(i in 2:n_sample) {
    mu[i] ~ normal(mu[i-1] + delta[i-1], sqrt(s_w));
  }

  for(i in 1:n_sample) {
    mu_noise[i] ~ normal(mu[i], sqrt(s_v));
  }

  for(i in 1:n_sample) {
    y[i] ~ poisson(lambda[i]);
  }
}

generated quantities {
  real lambda_smooth[n_sample];
  real best_catch_y;
  
  for(i in 1:n_sample) {
    lambda_smooth[i] = exp(mu[i]);
  }

  best_catch_y = trend / coef_catch_y;

}
