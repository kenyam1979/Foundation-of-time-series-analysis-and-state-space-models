library(rstan)
library(ggplot2)
library(ggfortify)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# 4-2 ~ 4-3. データ準備 ----
data_file <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-tsa-ssm-foundation/master/book-data/6-4-animal_catch_data.csv')
data_ts <- ts(data_file[,-1], start=1911, frequency=1)
autoplot(data_ts)

# 4-4 ~ 4-12. モデル推定 ----
data_stan <- list(
  y=data_file$y,
  catch_y=data_file$catch_y,
  n_sample=nrow(data_file))

fit_stan_count <- stan(
  file='Chapter6/6-4-count-model.stan',
  data=data_stan,
  iter=8000,
  thin=10,
  chains=4,
  seed=1)

print(fit_stan_count, pars=c('trend', 'coef_catch_y', 'best_catch_y'))

# 4-14 ~ 4-15. モデル結果確認 ----
sampling_result <- rstan::extract(fit_stan_count)

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
