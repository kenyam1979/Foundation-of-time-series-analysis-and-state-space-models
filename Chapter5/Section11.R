library(KFAS)
library(forecast)
library(ggplot2)
library(ggfortify)
library(xts)

# 周期性のあるデータ ----

## データ読み込み
file_data <- read.csv('https://raw.githubusercontent.com/logics-of-blue/book-tsa-ssm-foundation/master/book-data/5-11-sales_data.csv')
sales <- as.xts(read.zoo(file_data))
autoplot(sales)

## 休日の外生変数生成
### Nipponパッケージの代替
source("https://raw.githubusercontent.com/logics-of-blue/website/master/010_forecast/20190714_R%E8%A8%80%E8%AA%9E%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%97%A5%E6%9C%AC%E3%81%AE%E7%A5%9D%E6%97%A5%E5%88%A4%E5%AE%9A/jholiday.R", encoding="utf-8")

dates <- index(sales)

is.jholiday(dates)
weekdays(dates, T) != 'Sun'

holiday_date <- dates[is.jholiday(dates) & weekdays(dates, T) != 'Sun']

holiday_flg <- as.numeric(dates %in% holiday_date)
holiday_flg

## モデリング
build_cycle <- SSModel(
  H=NA,
  as.numeric(sales)~
    SSMtrend(degree=2, c(list(NA), list(NA))) +
    SSMseasonal(period=7, sea.type='dummy', Q=NA) +
    SSMregression(~holiday_flg, Q=0))

fit_cycle <- fitSSM(build_cycle, inits=c(1,1,1,1))

result_cycle <- KFS(
  fit_cycle$model,
  filtering=c('state', 'mean'),
  smoothing=c('state', 'mean'))

autoplot(sales)
autoplot(result_cycle$alphahat[,'level']) # トレンド+水準
autoplot(result_cycle$alphahat[,'sea_dummy1']) # シーズナリティ

## 予測区間
interval_cycle <- predict(
  fit_cycle$model,
  interval='prediction',
  level=0.95)

df <- cbind(
  time=as.POSIXct(index(sales)),
  sales=as.numeric(sales),
  as.data.frame(interval_cycle))

df %>% 
  ggplot(aes(x=time, y=sales)) +
  geom_point(aes(y=sales)) +
  geom_line(aes(y=fit), size=1.2, color='red') +
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill='blue', alpha=0.3)
