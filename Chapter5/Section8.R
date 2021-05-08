library(KFAS)
library(ggplot2)


# 8-2 ~ 8-4. KFASの使い方 ----
## 欠損データ作成
Nile
nile_train <- window(Nile, end=1950)
nile_train[41:60] <- NA


## モデル設定、推定、フィルタリングと平滑化
build_kfas <- SSModel(
  H=NA, # 観測誤差の分散
  nile_train~SSMtrend(degree=1, Q=NA)) # degree:ローカルモデル、Q:過程誤差の分散

fit_kfas <- fitSSM(build_kfas, inits=c(1,1)) # inits: H、Qの初期値
fit_kfas$model$H
fit_kfas$model$Q

result_kfas <- KFS(
  fit_kfas$model,
  filtering=c('state', 'mean'),
  smoothing=c('state', 'mean'))

mu_filter_kfas <- result_kfas$a[-1]
mu_smooth_kfas <- result_kfas$alphahat

autoplot(result_kfas)




# 8-5 ~ 8-6. 信頼、予測区間と予測 ----
## 信頼区間
smooth_conf <- predict(
  fit_kfas$model,
  interval='confidence',
  level=0.95)
smooth_conf

## 予測区間
smooth_pred <- predict(
  fit_kfas$model,
  interval='prediction',
  level=0.95)
smooth_pred
autoplot(smooth_pred)

## 予測
forecast_pred <- predict(
  fit_kfas$model,
  interval='prediction',
  level=0.95,
  n.ahead=20)

autoplot(forecast_pred)

estimate_all <- rbind(smooth_pred, forecast_pred)

df_forecast <- cbind(
  data.frame(y=as.numeric(Nile), time=1871:1970),
  as.data.frame(estimate_all))

df_forecast %>% 
  ggplot(aes(x=time, y=y)) +
  geom_point() +
  geom_line(aes(y=fit), color='red', size=1.2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3)

