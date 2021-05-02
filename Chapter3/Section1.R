library(urca)
library(lmtest)
library(prais)
library(ggplo2)
library(ggfortify)
library(gridExtra)

# 1-1 ~ 1.7. 見せかけの回帰 ----

n_sample = 400
set.seed(1)

## ホワイトノイズの回帰
y_wn <- rnorm(n=n_sample)
x_wn <- rnorm(n=n_sample)

mod_ols_wn <- lm(y_wn~x_wn)
summary(mod_ols_wn)

cbind(y_wn, x_wn) %>%
  ggplot(aes(x=x_wn, y=y_wn)) +
  geom_point() +
  geom_smooth(method='lm')

## 単位根の回帰
y_rw <- cumsum(rnorm(n=n_sample))
x_rw <- cumsum(rnorm(n=n_sample))

mod_ols_rw <- lm(y_rw~x_rw)
summary(mod_ols_rw)

cbind(y_rw, x_rw) %>%
  ggplot(aes(x=x_rw, y=y_rw)) +
  geom_point() +
  geom_smooth(method='lm')

## 定常ARの回帰
set.seed(2)
y_ar <- arima.sim(
  n=n_sample,
  model=list(order=c(1, 0, 0), ar=c(0.8)))
x_ar <- arima.sim(
  n=n_sample,
  model=list(order=c(1, 0, 0), ar=c(0.8)))
mod_ols_ar <- lm(y_ar~x_ar)
summary(mod_ols_ar)

## 残差の自己相関 (DW統計量)
resid_ols <- resid(mod_ols_rw)
dw <- sum(diff(resid_ols)^2) / sum((resid_ols)^2)
dw

Acf(resid(mod_ols_wn))
Acf(resid(mod_ols_rw))
Acf(resid(mod_ols_ar))

dwtest(mod_ols_wn)
dwtest(mod_ols_rw)
dwtest(mod_ols_ar)


# 1-8 ~ 1.15. 見せかけの回帰を防ぐ ----

## ADF検定
summary(ur.df(y_rw, type='none'))
summary(ur.df(x_rw, type='none'))

summary(ur.df(y_ar, type='none'))
summary(ur.df(x_ar, type='none'))

## 単位根でない場合-->一般化最小二乗法(GLS)
d <- data.frame(
  y_ar=y_ar,
  x_ar=x_ar)
mod_gls_PW <- prais_winsten(y_ar~x_ar, data=d)
summary(mod_gls_PW)

## 単位根で共和分でない場合-->差分系列で回帰
mod_lm_diff <- lm(diff(y_rw)~diff(x_rw))
summary(mod_lm_diff)

## 単位根で共和分の場合-->共和分関係
set.seed(10)
rw <- cumsum(rnorm(n_sample))

x_co <- 0.6 * rw + rnorm(n_sample)
y_co <- 0.4 * rw + rnorm(n_sample)

summary(ur.df(y_co, type='none'))
summary(ur.df(x_co, type='none'))

ts_df <- ts(data.frame(
  y_co=y_co, 
  x_co=x_co,
  z=x_co-(0.6/0.4)*y_co))
autoplot(ts_df, facets=T)

### PO検定(共和分関係の検定)
data_mat <- matrix(nrow=n_sample, ncol=2) 
data_mat[, 1] <- y_co
data_mat[, 2] <- x_co

summary(ca.po(data_mat, demean='none'))

### 共和分の場合の差分回帰
mod_lm_diff_cointegrate <- lm(diff(y_co)~diff(x_co))
summary(mod_lm_diff_cointegrate)
