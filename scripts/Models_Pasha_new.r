library(dplyr) # работа с табличками
library(tidyverse) # работа с табличками
library(openxlsx) # работа с эксель-файлами, необходима для сохранения результатов в эксель
library(zoo) # работа с датами и рядами
library(xts) # работа с датами и рядами
library(lubridate) # работа с датами
library(ggplot2) # графики
library(grid) # графики
library(gridExtra) # несколько графиков в одном
library(urca) # работа с временными рядами
library(forecast) # работа с временными рядами
library(rugarch) # одномерные гарчи
library(rmgarch) # многомерные гарчи
library(Hmisc) # корреляционная матрица с p-значениями

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
# предлагает ARIMA(2, 0, 1), но третий лаг AR не значим - оставим модель среднего ARIMA(2, 0, 1)

# проверим визуально на наличие условной гетероскедастичности
ggtsdisplay((df$MVID^2), main = 'MVID squared returns')
# выраженная гетероскедастичность
# наверно нужны какие-то тесты на г/ск в остатках, но пока это не важно

# наконец запишем спецификацию GARCH-модели для MVID
# документация здесь:
# https://www.rdocumentation.org/packages/rugarch/versions/1.4-4/topics/ugarchspec-methods
# в модели среднего мы предполагаем, что доходность M.Video около нуля и константа не нужна
mvid_spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(2, 1), include.mean = FALSE))
mvid_model <- ugarchfit(spec = mvid_spec, data = df$MVID)
# посмотрим на результаты оценивания
mvid_model
# информационные критерии
infocriteria(mvid_model)
# построим кучу графиков и кайфанём что R сделал всё за нас (убрать #)
# plot(mvid_model)
# особенно нас интересует, победили ли мы автокорреляцию и гетероскедастичность
plot(mvid_model, which = 10)
plot(mvid_model, which = 11)

# теперь попробуем учесть в модели не-Гауссово распределение доходностей
# у нас есть альтернатива - в R реализовано, например, распределение Стьюдента для случайной ошибки в GARCH
mvid_spec_t <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(2, 1), include.mean = FALSE),
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
       fill = c('gray', 'green', 'red'))

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

##
####
#### Построение моделей для других эмитентов ####
####
##

# посмотрим на ряд доходностей Магнита, Ленты, X5 Retail
ggtsdisplay(df$MGNT, main = 'MGNT returns')
ggtsdisplay(df$LNTA, main = 'LNTA returns')
ggtsdisplay(df$FIVE, main = 'X5 Retail returns')

# Проведем тест на стационарность для всех остальных эмитентов

mgnt_adf <- ur.df(type = 'none', df$MGNT, selectlags = 'AIC')
lnta_adf <- ur.df(type = 'none', df$LNTA, selectlags = 'AIC')
five_adf <- ur.df(type = 'none', df[!is.na(df$FIVE),'FIVE'], selectlags = 'AIC')

summary(mgnt_adf)
summary(lnta_adf)
summary(five_adf)

# Воспользуемся auto.arima для построения автоматической модели
mgnt_arima <- auto.arima(df$MGNT)
lnta_arima <- auto.arima(df$LNTA)
five_arima <- auto.arima(df$FIVE)

summary(mgnt_arima)
summary(lnta_arima)
summary(five_arima)

# проверим визуально на наличие условной гетероскедастичности для MGNT LNTA FIVE
ggtsdisplay((df$MGNT^2), main = 'MGNT squared returns')
ggtsdisplay((df$LNTA^2), main = 'LNTA squared returns')
ggtsdisplay((df$FIVE^2), main = 'FIVE squared returns')

# выраженная гетероскедастичность для всех эмитентов

# https://www.rdocumentation.org/packages/rugarch/versions/1.4-4/topics/ugarchspec-methods
# в модели среднего мы предполагаем, что доходность M.Video около нуля и константа не нужна
mgnt_spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
mgnt_model <- ugarchfit(spec = mgnt_spec, data = df$MGNT)

lnta_spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
lnta_model <- ugarchfit(spec = lnta_spec, data = df$LNTA)

five_spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
five_model <- ugarchfit(spec = five_spec, data = df[!is.na(df$FIVE),'FIVE'])

# посмотрим на результаты оценивания
mgnt_model
lnta_model
five_model
# информационные критерии
infocriteria(mgnt_model)
infocriteria(lnta_model)
infocriteria(five_model)

# учтем распределение стьюдента для случайной ошибки в GARCH, а также построим eGARCH для MGNT
mgnt_spec_t <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                          distribution.model = 'std')
mgnt_model_t <- ugarchfit(spec = mgnt_spec_t, data = df$MGNT)

lnta_spec_t <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                          distribution.model = 'std')
lnta_model_t <- ugarchfit(spec = lnta_spec_t, data = df$LNTA)

five_spec_t <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                          distribution.model = 'std')
five_model_t <- ugarchfit(spec = five_spec_t, data = df[!is.na(df$FIVE),'FIVE'])

# посмотрим на результаты оценивания
mgnt_model_t
lnta_model_t
five_model_t

# сравним информационные критерии

infocrit_df2 <- cbind(infocriteria(mgnt_model), infocriteria(mgnt_model_t))
colnames(infocrit_df2) <- c('GARCH(1, 1)',
                           'GARCH(1, 1) with T-errors')
infocrit_df2

infocrit_df3 <- cbind(infocriteria(lnta_model), infocriteria(lnta_model_t))
colnames(infocrit_df2) <- c('GARCH(1, 1)',
                            'GARCH(1, 1) with T-errors')
infocrit_df3

infocrit_df4 <- cbind(infocriteria(five_model), infocriteria(five_model_t))
colnames(infocrit_df2) <- c('GARCH(1, 1)',
                            'GARCH(1, 1) with T-errors')
infocrit_df4


# модель с шоками из распределения Стьюдента лучше по всем информационным критериям

# прогноз на 10 дней - сравним с оценённой волатильностью за последний квартал торгов
# будем рисовать стандартные отклонения, так как у дисперсии слишком мелкий масштаб
mgnt_t_fcst <- ugarchforecast(mgnt_model_t, n_ahead = 10)
var_fcst2 <- c(rep(NA, 60), mgnt_t_fcst@forecast$sigmaFor)
var_est2 <- c((tail(mgnt_model_t@fit$var, 60))^(1 / 2), rep(NA, 10))
plot(x = 1:70, y = var_est2,
     type = 'l', col = 'green', xlab = 'Дни',
     main = 'Оценка стандартного отклонения за последний квартал и прогноз на 10 дней')
lines(x = 1:70, y = var_fcst2, col = 'red')
legend('topright',
       legend = c('Оценка', 'Прогноз'),
       fill = c('green', 'red'))

lnta_t_fcst <- ugarchforecast(lnta_model_t, n_ahead = 10)
var_fcst3 <- c(rep(NA, 60), lnta_t_fcst@forecast$sigmaFor)
var_est3 <- c((tail(lnta_model_t@fit$var, 60))^(1 / 2), rep(NA, 10))
plot(x = 1:70, y = var_est3,
     type = 'l', col = 'green', xlab = 'Дни',
     main = 'Оценка стандартного отклонения за последний квартал и прогноз на 10 дней')
lines(x = 1:70, y = var_fcst3, col = 'red')
legend('topright',
       legend = c('Оценка', 'Прогноз'),
       fill = c('green', 'red'))

five_t_fcst <- ugarchforecast(five_model_t, n_ahead = 10)
var_fcst4 <- c(rep(NA, 60), five_t_fcst@forecast$sigmaFor)
var_est4 <- c((tail(five_model_t@fit$var, 60))^(1 / 2), rep(NA, 10))
plot(x = 1:70, y = var_est4,
     type = 'l', col = 'green', xlab = 'Дни',
     main = 'Оценка стандартного отклонения за последний квартал и прогноз на 10 дней')
lines(x = 1:70, y = var_fcst4, col = 'red')
legend('topright',
       legend = c('Оценка', 'Прогноз'),
       fill = c('green', 'red'))

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
  if (length(other_tickers) == 1) { return(out) } else {
    for (ticker in other_tickers[2:length(other_tickers)]) {
      out <- cbind(out,
                   rolling_corr(df,
                                window,
                                c(main_ticker, ticker)))
    }
    colnames(out) <- sapply(other_tickers,
                            function(x) paste0(main_ticker, ' and ', x))
    return(out) }
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

# нарисуем скользящие полугодовые корреляции для всех эмитентов
par(mfrow = c(3, 1))
p1
p2
p3
par(mfrow = c(1, 1))

# нарисуем график для отчёта
par(mfrow = c(2, 1), mar = c(2, 3, 2, 3))
plot(index(mdf_ts),
     rolling_corr(mdf_ts, 126, c('MGNT', 'FIVE')),
     type = 'l',
     col = '#b22222',
     ylim = c(-0.1, 0.8),
     xlab = 'Время',
     ylab = 'Корреляция',
     main = '')
lines(index(mdf_ts),
      rolling_corr(mdf_ts, 126, c('LNTA', 'FIVE')),
      col = 'black')
lines(index(mdf_ts),
      rolling_corr(mdf_ts, 126, c('LNTA', 'MGNT')),
      col = 'lightblue', lwd = 2)
legend('topleft',
       cex = 0.7,
       #box.col = 'white',
       #text.font=12,
       legend = c('FIVE и MGNT',
                  'FIVE и LNTA',
                  'MGNT и LNTA'),
       fill = c('#b22222', 'black', 'lightblue'))
# теперь то же самое для М.Видео
plot(index(mdf_ts),
     rolling_corr(mdf_ts, 126, c('MVID', 'FIVE')),
     type = 'l',
     col = '#b22222',
     ylim = c(-0.1, 0.4),
     xlab = 'Время',
     ylab = 'Корреляция',
     main = '')
lines(index(mdf_ts),
      rolling_corr(mdf_ts, 126, c('MVID', 'MGNT')),
      col = 'black')
lines(index(mdf_ts),
      rolling_corr(mdf_ts, 126, c('MVID', 'LNTA')),
      col = 'lightblue', lwd = 2)
legend('topleft',
       cex = 0.7,
       #box.col = 'white',
       #text.font=12,
       legend = c('MVID и FIVE',
                  'MVID и MGNT',
                  'MVID и LNTA'),
       fill = c('#b22222', 'black', 'lightblue'))

par(mfrow = c(1, 1))
# DCC-GARCH
# model specification setup
# at first assume some univariate volatility models of each asset
# for diagonal values of covariance matrix in DCC-GARCH
# first we shall select individual mean models for the period between 2018 and 2020 for each stock
plot_acf <- function(series, series_name) {
  p <- ggAcf(series) +
    ggtitle(paste0("Выборочная ACF для ", series_name)) +
    xlab("Lag") +
    ylab("ACF") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

plot_pacf <- function(series, series_name) {
  p <- ggPacf(series) +
    ggtitle(paste0("Выборочная PACF для ", series_name)) +
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
             top = textGrob('Выборочные ACF и PACF 2018-2021',
                            gp = gpar(fontsize = 18)))

# we see that all series nave no statistically significant sample ACF and PACF values
# and have zero mean, so the appropriate mean model is ARIMA(0,0,0)

# DCC-GARCH model itself
uspec_n <- multispec(replicate(4, ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))))
# estimate multiple univariate models
multifit <- multifit(uspec_n, mdf_ts)
multifit@fit
multifit_fit <- multifit@fit
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
# вытащим оценки коэффициентов и стандартные ошибки
dcc_coef <- slot(dcc_fit, 'mfit')[['matcoef']]
dcc_coef

# информационные критерии
infocriteria(dcc_fit)

# we see that dcca1 is significant only at 10% level, and coefficient for unconditional covariance matrix is 1-.01-.07
# (only dccb1 significant value indicates that conditional correlation shall decline over time)
# let's test hypothesis of constant correlation
DCCtest(mdf_ts, garchOrder = c(1, 1), n.lags = 1, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
# Нулевая гипотеза теста: корреляционная матрица не меняется с течением времени
# p-value гипотезы 0.034, то есть она отвергается на 5% и 10% уровне значимости
# источник теста:
# Engle, R.F. and Sheppard, K. 2001, Theoretical and empirical properties of dynamic conditional correlation multivariate GARCH, NBER Working Paper

# посмотрим на условные ковариации, оценённые внутри модели
dcc_cov <- rcov(dcc_fit)
dcc_corr <- rcor(dcc_fit)
# этот объект содержит ковариацонную матрицу для каждой даты торгов
dim(dcc_cov)
# посмотрим на матрицу для последней даты, для которой имеются наблюдения в наборе данных
dcc_corr[, , dim(dcc_corr)[3]]
# например, мы заинтересованы в оценке корреляции между MGNT and FIVE -> ряд 1, столбец 2
cor_fm <- dcc_corr[1, 2,]
cor_fm <- as.xts(cor_fm)
plot(cor_fm,
     type = 'l',
     main = 'Conditional correlation between FIVE and MGNT')

# объединим данные, чтобы сравнить скользящие корреляции и оценённые моделью, хотя это не одно и то же
rolling_five_q <- rolling_corr(mdf_ts, 63, c('FIVE', 'MGNT'))
rolling_five_m <- rolling_corr(mdf_ts, 21, c('FIVE', 'MGNT'))
# поскольку почему-то в данных появляются разные часы, уберём это с помощью to.daily
merged <- merge.xts(to.daily(cor_fm, OHLC = FALSE),
                    to.daily(rolling_five_q, OHLC = FALSE),
                    to.daily(rolling_five_m, OHLC = FALSE),
                    join = 'left')

colnames(merged) <- c('Model',
                      'Rolling 63-day window',
                      'Rolling 21-day window')

plot(merged$Model,
     type = 'l',
     ylim = c(-.7, .9),
     main = 'Различные корреляции между FIVE и MGNT')
lines(merged$`Rolling 21-day window`, col = 'red')
lines(merged$`Rolling 63-day window`, col = 'lightblue', lwd = 2)
# поскольку объект xts, у него свой способ добавления легенды, который связан с legend в base
addLegend('bottomright',
          #lty = 1, lwd = 1,
          legend.names = c('Модель',
                           'Скользящее 21-дневное окно',
                           'Скользящее 63-дневное окно'),
          fill = c('black', 'red', 'lightblue'),
          bg = "white",
          bty = "o")

# посмотрим на корреляцию М.Видео с X5
cor_mvid <- dcc_corr[1, 4,]
cor_mvid <- as.xts(cor_mvid)
plot(cor_mvid,
     type = 'l',
     main = 'Conditional correlation between FIVE and MVID')

# теперь нас интересуют прогнозы корреляций на 10 дней вперёд
dcc_forecast <- dccforecast(dcc_fit, n.ahead = 10)
dcc_forecast
corr_forecast <- dcc_forecast@mforecast$R
# это трёхмерная матрица с прогнозами
str(corr_forecast)
# нарисуем последние оценки корреляции и прогноз
plot_corr_fcst <- function(fit,
                           n_estimates,
                           n_ahead,
                           corr_mat_row,
                           corr_mat_col,
                           first_ticker,
                           second_ticker) {
  
  dcc_forecast <- dccforecast(fit, n.ahead = n_ahead)
  corr_forecast <- dcc_forecast@mforecast$R
  corr_fcst <- c(rep(NA, n_estimates),
                 corr_forecast[[1]][corr_mat_row, corr_mat_col,])
  corr_est <- c(tail(dcc_corr[corr_mat_row, corr_mat_col,], n_estimates),
                rep(NA, 10))
  
  plot(x = 1:70, y = corr_est,
       type = 'l', col = 'green', xlab = 'Дни',
       main = paste0('Оценка корреляции ',
                     first_ticker,
                     ' и ',
                     second_ticker,
                     ' за последний квартал и прогноз на 10 дней'))
  lines(x = 1:70, y = corr_fcst, col = 'red')
  legend('bottomright',
         legend = c('Оценка', 'Прогноз'),
         fill = c('green', 'red'))
}

par(mfrow = c(3, 1))
plot_corr_fcst(dcc_fit, 60, 10, 1, 2, 'FIVE', 'MGNT')
plot_corr_fcst(dcc_fit, 60, 10, 1, 3, 'FIVE', 'LNTA')
plot_corr_fcst(dcc_fit, 60, 10, 2, 3, 'MGNT', 'LNTA')
par(mfrow = c(1, 1))
# посмотрим на М.Видео отдельно
plot_corr_fcst(dcc_fit, 60, 10, 1, 4, 'FIVE', 'MVID')
# поскольку М.Видео меньше всего связан с другими эмитентами,
# возможно, имеет смысл для него использовать постоянные корреляции
# протестируем значимость выборочной корреляции между М.Видео и остальными акциями
cormat_test <- rcorr(mdf_ts, type = c('pearson'))
# сами корреляции
cormat_test$r
# p-values
cormat_test$P
# что интересно, на 10% уровне корреляции между М.Видео и остальными значимы
# на уровне 5% значимы корреляции М.Видео с Лентой и X5 Retail Group
# поэтому оставим всё как есть

# VaR
portfolio_variance <- function(w, cov_mat) {
  if (class(cov_mat) == 'list') {
    cov_mat <- matrix(unlist(cov_mat),
                      nrow = length(w))
  }
  return(t(w) %*% cov_mat %*% w) }

equal_weights <- function(n_assets) {
  return(rep(1 / n_assets, n_assets))
}

# также нам нужно многомерное распределение Стьюдента
# по результатам оценивания модели DCC-GARCH, у совместного распределения 5.96~6 степеней свободы
# проведём бэктест модели
# сколько наблюдений взять для бэктеста? Допустим, мы оцениваем модель на полугодовых данных
# скользящее окно - 126 наблюдений, в нашем датасете 810 наблюдений
# следующая строчка кода выполняется приблизительно 12 мин, поэтому не следует её запускать без лишней надобности
backtest_rolling <- dccroll(dcc_spec,
                            mdf_ts,
                            n.ahead = 1, # прогноз на 1 период вперёд
                            #forecast.length = 50,
                            refit.every = 1,
                            n.start = 127, # альтернатива forecast_length
                            refit.window = c("moving"),
                            window.size = 126,
                            solver = "solnp",
                            fit.control = list(eval.se = TRUE))

# нас в первую очередь интересуют оценки ковариационной матрицы
# backtest_rolling@mforecast - список из 683 элементов, и из каждого надо достать прогноз
# посмотрим на ковариацонную матрицу для первого дня прогноза
example_forecast <- backtest_rolling@mforecast[[1]]@mforecast$H
example_forecast

cov_list <- list()
for (i in 1:length(backtest_rolling@mforecast)) {
  cov_list[[i]] <- backtest_rolling@mforecast[[i]]@mforecast$H
}
# рассчитаем дисперсию всего портфеля
portfolio_vars <- c()
for (i in 1:length(cov_list)) {
  portfolio_vars[i] <- portfolio_variance(equal_weights(4), cov_list[[i]])
}
# нарисуем оценки стандартного отклонения, полученные в ходе бэктеста
plot(y = (portfolio_vars)**(1 / 2) * -1,
     x = tail(index(mdf_ts), 683),
     ylim = c(-0.05, 0.05),
     type = 'l',
     main = 'Оценка волатильности портфеля по модели DCC-GARCH',
     xlab = 'Время',
     ylab = '',
     lwd = 2)
lines(y = rowSums(tail(mdf_ts, 683) * equal_weights(4)),
      x = tail(index(mdf_ts), 683),
      col = 'gray', type = 'p', pch = 19, cex = .9)
lines(y =
        (portfolio_vars)**(1 / 2) *
        qdist(distribution = 'std', shape = 5.96, p = 0.05),
      x = tail(index(mdf_ts), 683),
      col = 'red',
      lwd = 2)
lines(y =
        (portfolio_vars)**(1 / 2) *
        qdist(distribution = 'std', shape = 5.96, p = 0.01),
      x = tail(index(mdf_ts), 683),
      col = 'lightblue',
      lwd = 2)
legend('topleft',
       cex = 1,
       #box.col = 'white',
       #text.font=12,
       legend = c('Доходность портфеля',
                  'Прогноз стандартного отклонения на 1 период',
                  '5% Value at Risk',
                  '1% Value at Risk'),
       fill = c('gray', 'black', 'red', 'lightblue'))

# посчитаем число пробитий и проведём тест Купика, H0 - число пробитий совпадает с ожидаемым
var_est_1 <- (portfolio_vars)**(1 / 2) *
  qdist(distribution = 'std', shape = 5.96, p = 0.01)
var_est_5 <- (portfolio_vars)**(1 / 2) *
  qdist(distribution = 'std', shape = 5.96, p = 0.05)
test <- rowSums(tail(mdf_ts, 683) * equal_weights(4))

print(paste0('Ожидаемое число пробитий 5% VaR ',
             683 * 0.05,
             ', фактическое ',
             sum(test < var_est_5)))
print(paste0('Ожидаемое число пробитий 1% VaR ',
             683 * 0.01,
             ', фактическое ',
             sum(test < var_est_1)))

# для 1% VaR наблюдаем пробитий (ожидали 7), для 5% VaR пробитий (ожидали )
# проведём тест Купика для бэктеста, используем скользящие прогнозы на 1 период вперёд
kupiec_test(683, test, var_est_1, 0.01)
kupiec_test(683, test, var_est_5, 0.05)
# на уровне 5% p-value теста 82%, на уровне 1% p-value 42%
# при любых уровнях значимости гипотеза о некорректной модели VaR отвергается

# сохраним оценки волатильности из многомерной модели в эксель

# создадим эксель-файл для сохранения табличек с результатами
fname <- 'dcc_garch.xlsx'
excel <- createWorkbook(fname)
# add seet to excel file
firstSheet <- 'multiv. GARCH data'
addWorksheet(excel, firstSheet)
writeData(excel, sheet = 1, mdf_ts, rowNames = TRUE)

secondSheet <- 'DCC-GARCH coef'
addWorksheet(excel, secondSheet)
writeData(excel, sheet = 2, dcc_coef, rowNames = TRUE)

thirdSheet <- 'DCC-GARCH infocriteria'
addWorksheet(excel, thirdSheet)
writeData(excel, sheet = 3, infocriteria(dcc_fit), rowNames = TRUE)

# проведём тест Льюнга-Бокса для стандартизированных остатков и их квадратов
residuals_list <- list()
for (i in 1:4) { residuals_list[[i]] <- residuals(multifit@fit[[i]],
                                                  standardize = TRUE) }
for (i in 1:4) {
  print(Box.test(residuals_list[[i]]**2,
                 lag = 1, type = c("Ljung-Box"), fitdf = 0)) }
# сохраним в файл результаты тестирования значимости выборочных корреляций
fourthSheet <- 'corr matrix'
addWorksheet(excel, fourthSheet)
writeData(excel, sheet = 4, cormat_test$r, rowNames = TRUE)

fifthSheet <- 'corr matrix p-values'
addWorksheet(excel, fifthSheet)
writeData(excel, sheet = 5, cormat_test$P, rowNames = TRUE)

# сохраним оценки Value at Risk и волатильности
sixSheet <- 'conditional vol and VaR'
addWorksheet(excel, sixSheet)

vars_and_vols <- cbind(var_est_5, var_est_1, portfolio_vars**(1 / 2), test)
dateindex <- tail(index(mdf_ts), 683)
vars_and_vols <- xts(vars_and_vols, order.by = dateindex)
colnames(vars_and_vols) <- c('5% VaR', '1% VaR', 'Conditional vol', 'Portdolio returns')
writeData(excel, sheet = 6, vars_and_vols, rowNames = TRUE)

saveWorkbook(excel, file = fname, overwrite = TRUE)

# тажке нас просят посчитать прогноз Value at Risk - сделаем на один период вперёд
dcc_forecast <- dccforecast(dcc_fit, n.ahead = 1)
dcc_forecast
cov_forecast <- dcc_forecast@mforecast$H
portfolio_cov_forecast <- portfolio_variance(equal_weights(4),
                                             cov_forecast)
portfolio_cov_forecast
portfolio_VaR_5 <- -(portfolio_var_forecast)**(1 / 2) *
  qdist(distribution = 'std', shape = 5.96, p = 0.05)
portfolio_VaR_1 <- -(portfolio_var_forecast)**(1 / 2) *
  qdist(distribution = 'std', shape = 5.96, p = 0.01)
portfolio_VaR_5
# 1.40%
portfolio_VaR_1
# 2.27%