library(dplyr) # работа с табличками
library(tidyverse) # работа с табличками
library(zoo) # работа с датами и рядами
library(xts) # работа с датами и рядами
library(lubridate) # работа с датами
library(broom) # обработка табличек с результатами
library(ggplot2) # графики
library(grid) # графики
library(gridExtra) # несколько графиков в одном
library(urca) # работа с временными рядами
library(forecast) # работа с временными рядами
library(rugarch) # одномерные гарчи
library(rmgarch) # многомерные гарчи


# загрузка данных
dfpath <- str_replace(getwd(), '/scripts', '/data/returns.csv')
df <- read.csv(dfpath)
# конвертируем даты в формат дат
df$TRADEDATE <- as.Date(df$TRADEDATE, format = '%Y-%m-%d')
glimpse(df)
# отлично, все данные открываются, причём цифры в нужном формате

# посмотрим на распределения доходностей и сравним их с нормальными
compare_distribution <-
  function(x, name) {
    plot <- qplot(x, geom = 'density',
                  main = paste0('Распределение ', toString(name), ' (синее) и нормальное')) +
      geom_density(fill = 'blue', alpha = 0.4) +
      geom_density(aes(rnorm(200000, mean(x), sd(x))), fill = 'red', alpha = 0.25) +
      labs(x = '') +
      theme(plot.title = element_text(hjust = 0.5))

    return(plot)
  }

grid.arrange(compare_distribution(df[!is.na(df$FIVE), 'FIVE'], 'FIVE'),
             compare_distribution(df$'MGNT', 'MGNT'),
             compare_distribution(df$'LNTA', 'LNTA'),
             compare_distribution(df$'MVID', 'MVID'),
             ncol = 2)


#####################
# Univariate models #
#####################

# посмотрим на ряд доходностей М.Видео
ggtsdisplay(df$MVID, main = 'MVID returns')

# тест на стационарность проведён в Питоне, но сделаем его и здесь
mvid_adf <- ur.df(type = 'none', df$MVID, selectlags = 'AIC')
summary(mvid_adf)

# можно предложить модель ARIMA(1, 0, 1) по графику, проверим, какую модель предложить нам auto.arima
mvid_arima <- auto.arima(df$MVID)
summary(mvid_arima)
# предлагает ARIMA(3, 0, 1), но третий лаг AR не значим - оставим модель среднего ARIMA(2, 0, 1)

# проверим визуально на наличие условной гетероскедастичности
ggtsdisplay((df$MVID^2), main = 'MVID squared returns')
# выраженная гетероскедастичность
# наверно нужны какие-то тесты на г/ск в остатках, но пока это не важно

# наконец запишем спецификацию GARCH-модели для MVID
# документация здесь:
# https://www.rdocumentation.org/packages/rugarch/versions/1.4-4/topics/ugarchspec-methods
# в модели среднего мы предполагаем, что доходность M.Video около нуля и константа не нужна
mvid_spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(3, 1), include.mean = FALSE))
mvid_model <- ugarchfit(spec = mvid_spec, data = df$MVID)
# посмотрим на результаты оценивания
mvid_model
# информационные критерии
infocriteria(mvid_model)
# построим кучу графиков и кайфанём что R сделал всё за нас
plot(mvid_model)
# особенно нас интересует, победили ли мы автокорреляцию и гетероскедастичность
plot(mvid_model, which = 10)
plot(mvid_model, which = 11)

# теперь попробуем учесть в модели не-Гауссово распределение доходностей
# у нас есть альтернатива - в R реализовано, например, распределение Стьюдента для случайной ошибки в GARCH
mvid_spec_t <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(3, 1), include.mean = FALSE),
                          distribution.model = 'std')
mvid_model_t <- ugarchfit(spec = mvid_spec_t, data = df$MVID)
# посмотрим на результаты оценивания
mvid_model_t
# сравним информационные критерии
infocrit_df <- cbind(infocriteria(mvid_model), infocriteria(mvid_model_t))
colnames(infocrit_df) <- c('GARCH(1, 1)',
                           'GARCH(1, 1) with T-errors')
infocrit_df
# модель с шоками из распределения Стьюдента лучше по всем информационным критериям
# нарисуем квадраты доходностей (несмещённая оценка волатильности) и оценку волатильности, полученную в GARCH(1,1)
# на графике видно, что в наиболее волатильные периоды спецификация с шоками, распределёнными по Стьюденту,
# лучше предсказывает всплески волатильности
plot(x = df$TRADEDATE, y = (df$MVID)^2, ylim = c(0, 0.02),
     type = 'l', col = 'gray', lwd = 2,
     main = 'Сравнение оценок волатильности', xlab = 'Дата', ylab = 'Оценка волатильности')
lines(x = df$TRADEDATE, y = mvid_model_t@fit$var, col = 'red', lwd = 2.5)
lines(x = df$TRADEDATE, y = mvid_model@fit$var, col = 'green', lwd = 2)
legend('topright',
       legend = c('Квадрат доходностей', 'Оценка GARCH (нормальный)', 'Оценка GARCH (Стьюдент)'),
       fill = c('gray', 'red', 'green'))

# прогноз на 10 дней - сравним с оценённой волатильностью за последний квартал торгов
# будем рисовать стандартные отклонения, так как у дисперсии слишком мелкий масштаб
mvid_t_fcst <- ugarchforecast(mvid_model_t, n_ahead = 10)
var_fcst <- c(rep(NA, 60), mvid_t_fcst@forecast$sigmaFor)
var_est <- c((tail(mvid_model_t@fit$var, 60))^(1 / 2), rep(NA, 10))
plot(x = 1:70, y = var_est,
     type = 'l', col = 'green', xlab = 'Дни',
     main = 'Оценка стандартного отклонения за последний квартал и прогноз на 10 дней')
lines(x = 1:70, y = var_fcst, col = 'red')
legend('topright',
       legend = c('Оценка', 'Прогноз'),
       fill = c('green', 'red'))

#### и так для остальных эмитентов, а также тест Купика, скользящие прогнозы и VaR


#######################
# Multivariate models #
#######################

# to work with multivariate models, we shall drop all rows with missing data
mdf <- df[!is.na(df$FIVE),]
glimpse(mdf)
mdf_ts <- xts(mdf[, c('FIVE', 'MGNT', 'LNTA', 'MVID')],
              order.by = mdf$TRADEDATE)
head(mdf_ts)

# GARCH-BEKK
# 1. At first, plot all returns on one chart
plot.xts(mdf_ts, multi.panel = TRUE)

# 2. Let's look at rolling half-year (126 trading days) correlation of series
# define function to calculte rolling correlation
rolling_corr <- function(df, window, tickers) {
  sub <- df[, tickers]
  out <- rollapply(sub,
                   width = window,
                   function(sub) cor(sub[, 1], sub[, 2]),
                   by.column = FALSE)
  return(out)
}

#function that calculates rolling corr between one asset and all others
ticker_rollcorr <- function(df, window, main_ticker, other_tickers) {
  out <- rolling_corr(df,
                      window,
                      c(main_ticker, other_tickers[1]))
  for (ticker in other_tickers[2:length(other_tickers)]) {
    out <- cbind(out,
                 rolling_corr(df,
                              window,
                              c(main_ticker, ticker)))
  }
  colnames(out) <- sapply(other_tickers,
                          function(x) paste0(main_ticker, ' and ', x))
  return(out)
}

plot_corr <- function(data,
                      window,
                      main_ticker,
                      other_tickers) {

  p <- plot.xts(ticker_rollcorr(data,
                                window = window,
                                main_ticker = main_ticker,
                                other_tickers = other_tickers),
                main = paste0('Скользящая ',
                              toString(window),
                              '-дневная корреляция ',
                              main_ticker),
                legend.loc = "bottomleft",
                auto.legend = TRUE)
  return(p)

}

p1 <- plot_corr(mdf_ts, 126, 'FIVE', c('MGNT', 'LNTA', 'MVID'))
p2 <- plot_corr(mdf_ts, 126, 'MGNT', c('FIVE', 'LNTA', 'MVID'))
p3 <- plot_corr(mdf_ts, 126, 'LNTA', c('FIVE', 'MGNT', 'MVID'))


par(mfrow = c(3, 1))
p1
p2
p3
dev.off()

# Best: https://www.youtube.com/watch?v=8VXmRl5gzEU
# https://www.youtube.com/watch?v=Sf1nwKYk9Iw
#https://rpubs.com/englianhu/binary-Q1Multi-GARCH
#
# http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/
# good https://rpubs.com/JesusZ/ModelGarch
# strange https://rpubs.com/englianhu/binary-Q1-Added
# good https://rpubs.com/samejimakim/MGARCH
# univariate not bad https://rpubs.com/florian1/garchmodeling
# good https://rpubs.com/EconFin/mgarch
#
# DCC-GARCH
# model specification setup
# at first assume some univariate volatility models of each asset
# for diagonal values of covariance matrix in DCC-GARCH
# first we shall select individual mean models for the period between 2018 and 2020 for each stock
plot_acf <- function(series, series_name) {
  p <- ggAcf(series) +
    ggtitle(paste0("Sample ACF for ", series_name)) +
    xlab("Lag") +
    ylab("ACF") +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}

plot_pacf <- function(series, series_name) {
  p <- ggPacf(series) +
    ggtitle(paste0("Sample PACF for ", series_name)) +
    xlab("Lag") +
    ylab("PACF") +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}

p1 <- plot_acf(mdf_ts$FIVE, 'FIVE')
p2 <- plot_pacf(mdf_ts$FIVE, 'FIVE')
p3 <- plot_acf(mdf_ts$MGNT, 'MGNT')
p4 <- plot_pacf(mdf_ts$MGNT, 'MGNT')
p5 <- plot_acf(mdf_ts$LNTA, 'LNTA')
p6 <- plot_pacf(mdf_ts$LNTA, 'LNTA')
p7 <- plot_acf(mdf_ts$MVID, 'MVID')
p8 <- plot_pacf(mdf_ts$MVID, 'MVID')

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,
             nrow = 4,
             top = textGrob('Sample ACF and PACF 2018-2021',
                            gp = gpar(fontsize = 20)))

# we see that all series nave no statistically significant sample ACF and PACF values
# and have zero mean, so the appropriate mean model is ARIMA(0,0,0)

# DCC-GARCH model itself
uspec_n <- multispec(replicate(4, ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))))
# estimate multiple univariate models
multifit <- multifit(uspec_n, mdf_ts)
multifit@fit
# on 10% level Ljung-Box H0 of no autocorrelation of residuals is not rejected for all univariate models
# specify DCC model with Student multivariate distribution and (1,1) conditional correlation order
dcc_spec <- dccspec(uspec = uspec_n,
                    dccOrder = c(1, 1),
                    distribution = "mvt")
# fit, fit.control - to control standard errors
dcc_fit <- dccfit(spec = dcc_spec,
                  data = as.matrix(mdf_ts),
                  fit.control = list(eval.se = TRUE),
                  fit = multifit)
# check that GARCH coefficents are the same as in the univariate models
dcc_fit
# we see that dcca1 is significant only at 10% level, and coefficient for unconditional covariance matrix is 1-.01-.07
# (only dccb1 significant value indicates that conditional correlation shall decline over time)
# let's test hypothesis of constant correlation
DCCtest(mdf_ts, garchOrder = c(1, 1), n.lags = 1, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
# we reject H0 of constant correlation at 5% level
#!!! проверить нулевую гипотезу теста, в документации есть ссылки на статьи
# сделать модель без мвидео
# the subject of specific interest in multivariate model is conditional covariances
dcc_cov <- rcov(dcc_fit)
dcc_corr <- rcor(dcc_fit)
# the object contains covariance matrix for each trading date
dim(dcc_cov)
# let's look at the last observed correlation matrix
dcc_corr[, , dim(dcc_corr)[3]]
# for instance, we are interested in correlation between MGNT and FIVE -> row 1, column 2
cor_fm <- dcc_corr[1, 2,]
cor_fm <- as.xts(cor_fm)
plot(cor_fm, main = 'Conditional correlation between FIVE and MGNT')
# finished at 26 minute of video
