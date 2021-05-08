library(KFAS)
library(forecast)
library(ggplot2)
library(ggfortify)

# 9-2 ~ 9-3. トレンドのあるデータ ----
## 一定のトレンド
n_sample <- 450
t0 <- 0.2
constant_trend <- cumsum(rep(t0, n_sample))
autoplot(ts(constant_trend))

## 変化するトレンド
t1 <- 0.2
t2 <- 0.4
t3 <- 0
t4 <- -0.2
trend <- c(rep(t1, 100), rep(t2, 100), rep(t3, 100), rep(t4, 150))
change_trend <- cumsum(trend)
autoplot(ts(change_trend))

## シミュレーションデータ
set.seed(12)

### 過程誤差
system_noise <- rnorm(n=n_sample)         
alpha_true <- cumsum(system_noise + trend)
autoplot(ts(alpha_true))

### 観測誤差
obs_noise <- rnorm(n=n_sample, sd=5)
sales <- obs_noise + alpha_true + 11
autoplot(ts(sales))


# 9.4 ~ 9-6. ローカル線系トレンドモデル ----
## モデル推定
sales_train <- sales[1:400]
sales_test <- sales[401:450]
                    
build_trend <- SSModel(
  H=NA,
  sales_train~SSMtrend(degree=2, Q=c(list(NA), list(NA))))

fit_trend <- fitSSM(
  build_trend,
  inits=c(1,1,1))

fit_trend$model$H
fit_trend$model$Q
fit_trend$model$T

result_trend <- KFS(
  fit_trend$model,
  filtering=c('state', 'mean'),
  smoothing=c('state', 'mean'))


## トレンドの抽出
result_trend$alphahat # level=mu, slope=trend

trend_df <- data.frame(
  time=1:400,
  true_trend=trend[1:400],
  estimate_trend=result_trend$alphahat[, 'slope'])

trend_df %>%
  ggplot(aes(x=time, y=true_trend)) +
  geom_line(aes(y=true_trend), linetype='dashed') + 
  geom_line(aes(y=estimate_trend))


# 9.7 ~ 9.10 予測 ----
interval_trend <- predict(
  fit_trend$model,
  interval='prediction',
  level=0.95)

forecast_trend <- predict(
  fit_trend$model,
  interval='prediction',
  level=0.95,
  n.ahead=50)

estimate_all <- rbind(interval_trend, forecast_trend)
df <- cbind(
  time=1:450,
  sales=sales,
  as.data.frame(estimate_all))

df %>%
  ggplot(aes(x=time, y=fit)) +
  geom_point(aes(y=sales), size=1, alpha=0.3) +
  geom_line(aes(y=fit), color='red', size=1.2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill='blue', alpha=0.3)

## 補足: ARIMAでの予測
mod_arima <- auto.arima(sales_train)
forecast_arima <- forecast(mod_arima, h=50, level=0.95)
autoplot(forecast_arima)

accuracy(forecast_trend[,'fit'], sales_test)
accuracy(forecast_arima, sales_test)
