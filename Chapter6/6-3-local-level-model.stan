data {
  int n_sample;
  real y[n_sample];
}

parameters {
  real mu_zero;
  real mu[n_sample];
  real<lower=0> s_w;
  real<lower=0> s_v;
}

model {
  mu[1] ~ normal(mu_zero, sqrt(s_w));
  
//  for(i in 2:n_sample) {
//    mu[i] ~ normal(mu[i-1], sqrt(s_w));
//  }
  mu[2:n_sample] ~ normal(mu[1:n_sample-1], sqrt(s_w));
  
//  for(i in 1:n_sample) {
//    y[i] ~ normal(mu[i], sqrt(s_v));
//  }
  y ~ normal(mu, sqrt(s_v));

}

