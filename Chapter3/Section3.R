library(xts)
library(fGarch)
library(rugarch)
library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# GARCHモデル ----
## シミュレーションデータ作成
n_sample = 1000

spec1 <- garchSpec(
  model=list(omega=0.001, alpha=0.4, beta=0.5, mu=0.1),
  cond.dist='norm')
set.seed(1)

sim_garch <- garchSim(spec1, n=n_sample, extended=T)
sim_garch <- ts(sim_garch)

autoplot(sim_garch[,-3], facets=T)

acf(sim_garch[,'garch'])
acf(sim_garch[,'garch']^2)

## fGarchによるモデリング
mod_fGarch <- garchFit(
  fomula=~ garch(1, 1),
  data=sim_garch[,'garch'],
  include.mean=T,
  trace=F)
mod_fGarch

## rugarchによるモデリング
spec_rugarch1 <- ugarchspec(
  variance.model=list(model='sGARCH', garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model='norm')

mod_rugarch <- ugarchfit(
  spec=spec_rugarch1,
  data=sim_garch[,'garch'],
  solver='hybrid')
mod_rugarch


# ARMA-GARCHモデル ----
## シミュレーションデータ生成
spec2 <- garchSpec(
  model=list(
    omega=0.001, alpha=0.5, beta=0.4,
    mu=0.1, ar=-0.6, ma=-0.5),
  cond.dist='norm')

set.seed(0)
sim_arma_garch <- garchSim(spec2, n=n_sample, extended=F)
autoplot(sim_arma_garch)

## ARMAでのモデル推定
mod_arma <- Arima(sim_arma_garch, order=c(1,0,1))
checkresiduals(mod_arma)
jarque.bera.test(resid(mod_arma))

## ARMA+GARCHでのモデル推定
spec_rugarch2 <- ugarchspec(
  variance.model=list(model='sGARCH', garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(1,1), include.mean=TRUE),
  distribution.model='norm')

mod_arma_garch <- ugarchfit(
  spec=spec_rugarch2,
  data=sim_arma_garch, 
  solver='hybrid')

mod_arma_garch

## 両モデルの残差の評価
autoplot(residuals(mod_arma_garch, standard=T))
autoplot(residuals(mod_arma)/sqrt(mod_arma$sigma2))


# GJR-GARCHモデル ----

## GJR-GARCHの推定
data('spyreal')
spec_rugarch3 <- ugarchspec(
  variance.model=list(model='gjrGARCH', garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(1,1)),
  distribution.model='std')

mod_gjr <- ugarchfit(
  spec=spec_rugarch3,
  data=spyreal[,1],
  solvere='hybrid')

mod_gjr

## GARCHとのモデル比較
spec_rugarch4 <- ugarchspec(
  variance.model=list(model='sGARCH', garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(1,1)),
  distribution.model='std')

mod_standard_garch <- ugarchfit(
  spec=spec_rugarch4,
  data=spyreal[,1],
  solvere='hybrid')

infocriteria(mod_gjr)
infocriteria(mod_standard_garch)


## ボラティリティの変動
autoplot(sigma(mod_gjr))

plot(mod_gjr) # これの3番でも表示される
