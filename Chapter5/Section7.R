library(dlm)
library(ggplot2)
library(ggfortify)

Nile

# 7-9 ~ 7-11. dlmによるカルマンフィルタ ----

## モデル設定
sigma_w <- 1000
sigma_v <- 10000

mod_dlm <- dlmModPoly(
  order=1, # ローカルモデル
  m0=0,
  C0=10000000,
  dW =sigma_w,
  dV=sigma_v)

## カルマンフィルタ
mu_filter_dlm <- dlmFilter(Nile, mod_dlm)
autoplot(mu_filter_dlm)

## 平滑化
mu_smooth_dlm <- dlmSmooth(mu_filter_dlm)
p <- autoplot(Nile)
autoplot(mu_smooth_dlm, colour='red', p=p)


# 7-12. パラメタ推定 ---- 
## モデル設定
build_local_level_dlm <- function(theta) {
  dlmModPoly(order=1, dV=exp(theta[1]), dW=exp(theta[2]))  
}

## パラメタ推定
par_local_level_dlm <- dlmMLE(Nile, parm=c(1,1), build_local_level_dlm)
par_local_level_dlm$par

## パラメタ組込
fit_local_level_dlm <- build_local_level_dlm(par_local_level_dlm$par)

## フィルタリング
filter_local_level_dlm <- dlmFilter(Nile, fit_local_level_dlm)
autoplot(filter_local_level_dlm)

## 平滑化
smooth_local_level_dlm <- dlmSmooth(filter_local_level_dlm)
p <- autoplot(Nile)
autoplot(smooth_local_level_dlm, colour='red', p=p)


# KASによる散漫カルマンフィルタ ----
library(KFAS)
mod_kfas <- SSModel(
  H=sigma_v,
  Nile~SSMtrend(degree=1, Q=sigma_w))

mu_filter_kfas <- KFS(
  mod_kfas,
  filtering=c('state', 'mean'),
  smoothing='none')

autoplot(mu_filter_kfas)
