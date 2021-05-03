library(urca)
library(vars)
library(ggplot2)
library(ggfortify)

library(fpp) # データセット

# VARモデル ----

## データ確認
usconsumption
autoplot(usconsumption, facet=T)

### ADF検定
summary(ur.df(usconsumption[, 'consumption'], type='drift'))
summary(ur.df(usconsumption[, 'income'], type='drift'))

### 相互相関
ccf(
  usconsumption[, 'consumption'],
  usconsumption[, 'income'])


## VARモデル
## 次数の確認
select_result <- VARselect(usconsumption, lag.max=10, type='const')
select_result

var_bestorder <- VAR(
  y=usconsumption,
  type='const',
  p=select_result$selection[1]) # 次数

summary(var_bestorder)

# VARモデルの予測 ----
var_predict <- predict(var_bestorder, n.ahead=4)
autoplot(var_predict)


# 因果性の分析 ----

## Granger因果性の検定
causality(var_bestorder, cause="income")
causality(var_bestorder, cause="consumption")

## インパルス応答による評価
irf_consumption <- irf(
  var_bestorder,
  impulse='consumption',
  response=c('consumption', 'income'),
  n.ahead=12,
  boot=T)

plot(irf_consumption)

plot(fevd(var_bestorder, n.ahead=12))
