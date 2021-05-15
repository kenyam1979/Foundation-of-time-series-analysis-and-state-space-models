library(rstan)
library(ggplot2)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 3-3 ~ 3-8. データ生成 ----
n_sample <- 100
y <- numeric(n_sample)

mu_zero <- 100
mu <- numeric(n_sample)
s_w <- 1000
s_v <- 5000

set.seed(1)

mu[1] <- rnorm(n=1, mean=mu_zero, sd=sqrt(s_w))

i <- 1
my[i] <- rnomr(n=1, mean=mu_zero, sd=sqrt(s_w))
for (i in 2:n_sample) {
  mu[i] <- rnorm(n=1, mean=mu[i-1], sd=sqrt(s_w))
}
autoplot(ts(mu))

i <- 1
for (i in 1:n_sample) {
  y[i] <- rnorm(n=1, mean=mu[i], sd=sqrt(s_v))  
}
autoplot(ts(y))


# 3-9 ~ 3-12. Stanによる推定 ----
data_sim <- list(y=y, n_sample=n_sample)

## warmupが少なく収束してない
fit_stan_1 <-stan(
  file='Chapter6/6-3-local-level-model.stan',
  data=data_sim,
  iter=550,
  warmup=50,
  thin=1,
  chains=4,
  seed=1
)

print(fit_stan_1, digits=1, pars=c('s_w', 's_v', 'lp__'))
traceplot(fit_stan_1, pars=c('s_w', 's_v'))

## 十分収束させたもの
fit_stan_2 <-stan(
  file='Chapter6/6-3-local-level-model.stan',
  data=data_sim,
  iter=5000,
  warmup=2500,
  thin=5,
  chains=4,
  seed=1
)

print(fit_stan_2, digits=1, pars=c('s_w', 's_v', 'lp__'))
traceplot(fit_stan_2, pars=c('s_w', 's_v'))

# 3-13 ~ 3-14. 結果出力 ----
sampling_result <- rstan::extract(fit_stan_2)

## 過程誤差の分散
quantile(sampling_result$s_w)
data.frame(s_w=sampling_result$s_w) %>%
  ggplot(aes(x=s_w)) +
  geom_histogram()

## muの推定
sampling_result$mu[,1]
quantile(sampling_result$mu[,1])

model_mu <- t(apply(
  X=sampling_result$mu,
  MARGIN=2,
  FUN=quantile,
  probs=c(0.025, 0.5, 0.975)))
colnames(model_mu) <- c('lwr', 'fit', 'upr')

stan_df <- cbind(
  time=1:n_sample,
  y=y,
  as.data.frame(model_mu))

stan_df %>%
  ggplot(aes(x=time, y=y)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=fit), color='red', size=1.2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, fill='blue')
