library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)


# 7-1 ~ 7-6. データの探索 ----
data(Seatbelts)

## 対数
front <- Seatbelts[,'front']
log_front <- log(front)
ggtsdisplay(log_front)

## 対数差分
log_diff <- diff(log_front)
ggtsdisplay(log_diff)

## 季節性差分
ggsubseriesplot(log_diff)

seas_log_diff <- diff(log_diff, 12)
ggtsdisplay(seas_log_diff)

## コレログラム
acf(seas_log_diff)
pacf(seas_log_diff)





# 7-7 ~ 7-12. モデルの推定 ----

## データ準備
Seatbelts_log <- Seatbelts[, c('front', 'PetrolPrice', 'law')]
Seatbelts_log[, 'front'] <- log(Seatbelts[, 'front'])
Seatbelts_log[, 'PetrolPrice'] <- log(Seatbelts[, 'PetrolPrice'])

train <- window(Seatbelts_log, end=c(1983, 12))
test <- window(Seatbelts_log, start=c(1984,1))

petro_law <- train[, c('PetrolPrice', 'law')]

## ARIMAモデル推定
model_sarimax <- Arima(
  y=train[, 'front'],
  order=c(1, 1, 1),
  seasonal=list(order=c(1, 0, 0)),
  xreg=petro_law)

model_sarimax


## 差分系列と次数
### 以下が同じになる(差分)
Arima(
  y=log_diff,
  order=c(1, 0, 0),
  include.mean=F)
Arima(
  y=log_front,
  order=c(1, 1, 0),
  include.mean=F)

### 以下が同じになる(季節差分)
Arima(
  y=seas_log_diff,
  order=c(1, 0, 0),
  include.mean=F)
Arima(
  y=log_front,
  order=c(1, 1, 0),
  seasonal=list(order=c(0, 1, 0)),
  include.mean=F)

## 自動モデル選択
sarimax_petro_law <- auto.arima(
  y=train[, 'front'],
  xreg=petro_law,
  ic='aic',
  max.order=7,
  stepwise=F,
  approximation=F,
  parallel=F) # 並列処理エラーがでる
sarimax_petro_law # 書籍とは異なる結果

## 特性方程式の解
### AR項
abs(polyroot(c(1, -coef(sarimax_petro_law)[c('ar1', 'ar2')])))
### MA項
abs(polyroot(c(1, coef(sarimax_petro_law)[c('ma1')])))
### 季節性MA項
abs(polyroot(c(1, coef(sarimax_petro_law)[c('sma1')])))

## 残差のチェック
### 自己相関あり
checkresiduals(sarimax_petro_law)
### 正規性
jarque.bera.test(resid(sarimax_petro_law))
qqnorm(resid(sarimax_petro_law))
qqline(resid(sarimax_petro_law))


# 7-13 ~ 7-15. 予測 ----
## 未来の外生変数を使った予測
petro_law_test <- test[, c('PetrolPrice', 'law')]

sarimax_f <- forecast(
  sarimax_petro_law,
  xreg=petro_law_test,
  h=12,
  level=c(95, 70))
sarimax_f

autoplot(sarimax_f)

## 過去の外生変数を使った予測
### 平均
petro_law_mean <- data.frame(
  PetrolPrice=rep(mean(train[, 'PetrolPrice']), 12),
  law=rep(1, 12))

sarimax_f_mean <- forecast(
  sarimax_petro_law,
  xreg=as.matrix(petro_law_mean),
  h=12,
  level=c(95, 70))
sarimax_f_mean

autoplot(sarimax_f_mean)

### 最近値
petro_law_tail <- data.frame(
  PetrolPrice=rep(tail(train[, 'PetrolPrice'], 1), 12),
  law=rep(1, 12))

sarimax_f_tail <- forecast(
  sarimax_petro_law,
  xreg=as.matrix(petro_law_tail),
  h=12,
  level=c(95, 70))
sarimax_f_tail

autoplot(sarimax_f_tail)

## ナイーブ予測
naive_f_mean <- meanf(train[, 'front'], h=12)
naive_f_latest <- rwf(train[, 'front'], h=12)

## 予測の評価
accuracy(sarimax_f, test[, 'front'])
accuracy(sarimax_f_mean, test[, 'front'])
accuracy(sarimax_f_tail, test[, 'front'])
accuracy(naive_f_mean, test[, 'front'])
accuracy(naive_f_latest, test[, 'front'])
