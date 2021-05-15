library(rstan)
library(ggplot2)
library(ggfortify)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 5-2 ~5-3. データ準備 ----
data_file <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-tsa-ssm-foundation/master/book-data/6-5-logistic-growth-data.csv')
data_ts <- ts(data_file[,-1], start=1961, frequency=1)
autoplot(data_ts)

# 5-4 ~ モデル推定 ----

data_stan <- list(
  y=data_file$y,
  n_sample=nrow(data_file))

fit_stan_growth <- stan(
  file='Chapter6/6-5-logistic-growth-model.stan',
  data=data_stan,
  iter=5000,
  thin=5,
  chains=4,
  seed=1)



# 5-12 ~ 5-13. モデル結果分析 ----
print(fit_stan_growth, pars=c('r', 'K'))


sampling_result <- rstan::extract(fit_stan_growth)

model_lambda_smooth <- t(apply(
  X=sampling_result$lambda_smooth,
  MARGIN=2,
  FUN=quantile,
  probs=c(0.025, 0.5, 0.975)))
colnames(model_lambda_smooth) <- c('lwr', 'fit', 'upr')

stan_df <- cbind(
  time=data_file$time,
  y=data_file$y,
  as.data.frame(model_lambda_smooth))

stan_df %>%
  ggplot(aes(x=time, y=y)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=fit), color='red', size=1.2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, fill='blue')
