library(KFAS)
library(forecast)
library(ggplot2)
library(ggfortify)


# 広告効果の分析 ----
n_sample <- 450
set.seed(10)

## 広告効果のあるデータ
true_reg_coef <- -log(1:50)*2 + 8 # 広告効果
mu <- cumsum(rnorm(n=n_sample, sd=0.5)) + 15 # ランダムウォークする水準値
x <- mu + c(rep(0,200), true_reg_coef, rep(0,200)) # 広告効果＋水準値
obs_error <- rnorm(n=n_sample, sd=2) # 観測誤差

sales_ad <- x + obs_error
ggtsdisplay(ts(sales_ad))

### 広告フラグ
ad_flg <- numeric(n_sample)
ad_flg[201:250] <- 1

## モデリング
build_reg <- SSModel(
  H=NA,
  sales_ad~
    SSMtrend(degree=1, Q=NA) +
    SSMregression(~ad_flg, Q=NA))

## 推定
fit_reg <- fitSSM(build_reg, inits=c(1,1,1))


result_reg <- KFS(
  fit_reg$model,
  filtering=c('state', 'mean'),
  smoothing=c('state', 'mean'))


## 広告効果(外生変数の係数)
interval_coef <- predict(
  fit_reg$model,
  states='regression',
  interval='confidence',
  level=0.95)

coef_df <- cbind(
  time=201:250,
  reg_coef=true_reg_coef,
  as.data.frame(interval_coef[201:250,]))

coef_df %>%
  ggplot(aes(x=time, y=reg_coef)) +
  geom_line(aes(y=reg_coef), color='grey') +
  geom_line(aes(y=fit)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill='blue', alpha=0.3)
